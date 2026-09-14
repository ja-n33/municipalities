pacman::p_load(dplyr, httr, jsonlite, ggplot2, sf, leaflet, scales, cowplot, magick, patchwork)

output_dir <- here::here("audit_outcomes", "output")

ifelse(dir.exists(file.path(output_dir)), print("Path Exists"), 
                dir.create(file.path(output_dir), recursive = TRUE))

substack_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "#EDE8DA", colour = NA),
    panel.background  = element_rect(fill = "#EDE8DA", colour = NA),
    panel.grid.major  = element_line(colour = "#DCE0C8", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    panel.border = element_rect(colour = "#DCE0C8", fill = NA, linewidth = 0.6),
    axis.title.x = element_text(colour = "#1F2A24", margin = margin(t = 8), size = rel(0.9)),
    axis.title.y = element_text(colour = "#1F2A24", margin = margin(r = 8), size = rel(0.9)),
    axis.text    = element_text(colour = "#5C6B5A", size = rel(0.85)),
    axis.title        = element_text(colour = "#1F2A24"),
    plot.title        = element_text(colour = "#1F2A24", face = "bold", size = rel(1.15), margin = margin(b = 4)),
    plot.caption      = element_text(colour = "#7A8A6F", size = 9, hjust = 0, margin = margin(t = 8)),
    plot.caption.position = "plot", 
    legend.position   = "bottom",
    legend.title      = element_blank(),
    legend.text       = element_text(colour = "#1F2A24"),
    legend.background = element_rect(fill = "#EDE8DA", colour = NA),
    legend.key        = element_rect(fill = "#EDE8DA", colour = NA),
  )

####################################################################################################################################################################################

                                                                     ## MUNICIPAL BASIC DATA ##

####################################################################################################################################################################################
if (TRUE){
census_zip <- here::here("data", "census_sample.zip")

census_dir <- here::here("data", "census_sample")

ifelse(dir.exists(file.path(census_dir)), print("Path Exists"), 
                dir.create(file.path(census_dir), recursive = TRUE))

unzip(census_zip, exdir = census_dir)

munic_qid <- readr::read_csv(here::here(census_dir, "Census2022sample_F18.csv")) %>%
        dplyr::select(QID, Municipality)


hh_sz <- readr::read_csv(here::here(census_dir, "Census2022sample_F21.csv")) %>%
        dplyr::select(QID, PID) %>%
        dplyr::group_by(QID) %>%
        dplyr::summarize(hhsz = n_distinct(PID))

hh_wgt <- readr::read_csv(here::here(census_dir, "Census2022sample_F19.csv")) %>%
        dplyr::select(QID, HH_WGT, H05_WATERPIPED, H09_ENERGY_COOKING) %>%
        dplyr::mutate(socio = case_when(
                H05_WATERPIPED %in% c(1, 2) & H09_ENERGY_COOKING == 1 ~ 1, 
                TRUE ~ 0) * HH_WGT) %>%
        dplyr::select(QID, HH_WGT, socio) 

hh_df <- left_join(hh_wgt, hh_sz, by = "QID") %>%
        dplyr::mutate(pop = HH_WGT * hhsz) %>%
        dplyr::select(QID, HH_WGT, socio, hhsz, pop)

munic_census <- left_join(munic_qid, hh_df, by = "QID")  %>%
                dplyr::group_by(Municipality) %>%
                dplyr::summarise(hh_n = sum(HH_WGT, na.rm = TRUE), 
                                hh_socio = sum(socio, na.rm = TRUE), 
                                pop_n = sum(pop, na.rm = TRUE)) %>%
                dplyr::mutate(socio = hh_socio / hh_n, 
                               scaled_socio =  scale(socio), 
                                qtl_socio = ntile(scaled_socio, 3))


resp_munic <- GET("https://municipaldata.treasury.gov.za/api/cubes/incexp_v2/members/demarcation",
            query = list(pagesize = 300)
)

meta_munic <- GET("https://municipaldata.treasury.gov.za/api/cubes/municipalities/facts")

munic_meta <- fromJSON(content(meta_munic, "text", encoding = "UTF-8"))$data

munic_labels <- fromJSON(content(resp_munic, "text", encoding = "UTF-8"))$data

munic_names_temp <- left_join(munic_labels, munic_meta %>% dplyr::select(municipality.demarcation_code, municipality.parent_code, municipality.miif_category, municipality.province_code, municipality.miif_category), by = c("demarcation.code" = "municipality.demarcation_code")) 


districts_df <- munic_names_temp %>%
                dplyr::filter(stringr::str_detect(stringr::str_sub(demarcation.code, 1, 2), "DC")) %>%
                dplyr::select(demarcation.code, municipality.miif_category) %>%
                dplyr::rename(municipality.parent_code = demarcation.code, district.miif_category = municipality.miif_category)

munic_names <- left_join(munic_names_temp, districts_df, by = "municipality.parent_code")


# Old classifications need to be merged with new classifications. There are 34 cases. 


# Assign LIM342 to LIM343, KZN283 to KZN282 and LIM352 to LIM354

munic_census <- munic_census %>%
                dplyr::mutate(dem_code = case_when(
                        Municipality %in% c("EC103", "EC107") ~ "EC101", 
                        Municipality %in% c("EC127", "EC128") ~ "EC129", 
                        Municipality %in% c("EC132", "EC133", "EC134") ~ "EC139", 
                        Municipality %in% c("EC143", "EC144") ~ "EC145", 
                        Municipality == "FS164" ~ "MAN", 
                        Municipality %in% c("GT482", "GT483") ~ "GT485", 
                        Municipality %in% c("KZN211", "KZN215") ~ "KZN216", 
                        Municipality %in% c("KZN232", "KZN233") ~ "KZN238", 
                        Municipality %in% c("KZN234", "KZN236") ~ "KZN237", 
                        Municipality %in% c("KZN273", "KZN274") ~ "KZN276", 
                        Municipality == "KZN283" ~ "KZN282", 
                        Municipality %in% c("KZN431", "KZN432") ~ "KZN436", 
                        Municipality == "LIM342" ~ "LIM343", 
                        Municipality == "LIM352" ~ "LIM354", 
                        Municipality %in% c("LIM365", "LIM364") ~ "LIM368", 
                        Municipality %in% c("LIM365", "LIM364") ~ "LIM368", 
                        Municipality %in% c("LIM474", "LIM475") ~ "LIM476",
                        Municipality %in% c("MP322", "MP323") ~ "MP326",
                        Municipality %in% c("NC081", "NC083") ~ "NC087",
                        Municipality %in% c("NW401", "NW402") ~ "NW405",
                        TRUE ~ Municipality
                        )) 


munic_list <- left_join(munic_census, munic_names, by  = c("dem_code" = "demarcation.code")) %>%
                dplyr::filter(!(stringr::str_detect(stringr::str_sub(dem_code, 1, 2), "DC"))) %>%
                dplyr::rename(demarcation.code = dem_code) %>%
                mutate(district.miif_category = case_when(is.na(district.miif_category) ~ "C3", 
                                                                TRUE ~ district.miif_category))

}


####################################################################################################################################################################################

                                                                     ## MUNICIPAL AUDIT DATA ##

####################################################################################################################################################################################

# Using the xl spreadsheets is better as it provides disaggregated information

if (TRUE){


fy_annexures <- c()
df_list <- list()

for (fy in c("2021", "2122", "2223", "2324", "2425")){

    path <- here::here("data", paste0("mfma", fy, ".xlsx"))

    heads <- readxl::read_excel(path, sheet = 1, skip = 2, n_max = 2, col_names = FALSE)

    category <- zoo::na.locf(unlist(heads[1, ], use.names = FALSE), na.rm = FALSE)
    subcat <- unlist(heads[2, ], use.names = FALSE)

    col_names <- ifelse(is.na(subcat), tolower(category), tolower(paste0(category, "_", subcat)))

    col_names <- janitor::make_clean_names(col_names)

    if (fy == "2021"){
    temp_data <- readxl::read_excel(path, sheet = 1, skip = 5, col_names = col_names) %>%
                dplyr::mutate(period = paste0("20", stringr::str_sub(fy, start = -2, end = -1)),
                                  dplyr::across(c("findings_on_specific_risk_areas_human_resource_management","findings_on_specific_risk_areas_information_technology"), ~ as.character(.x))) %>%
                dplyr::rename_with(~ stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_"), 
                        .cols = 8:10) %>%
                dplyr::rename_with(~ paste0("prior", stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_")), 
                        .cols = 11:13)
                        }

    else if (fy == "2223") {
        subcat_2223 <- subcat
        subcat_2223[subcat_2223 %in% c("Number", "Auditee ID", "Auditee",
                                        "Auditee category", "Auditee type", "Province")] <- NA
        col_names_2223 <- janitor::make_clean_names(
            ifelse(is.na(subcat_2223), tolower(category), tolower(paste0(category, "_", subcat_2223)))
        )
        temp_data <- readxl::read_excel(path, sheet = 1, skip = 5, col_names = col_names_2223) %>%
                dplyr::mutate(period = paste0("20", stringr::str_sub(fy, start = -2, end = -1)),
                                  dplyr::across(c("findings_on_specific_risk_areas_human_resource_management","findings_on_specific_risk_areas_information_technology"), ~ as.character(.x))) %>%
                dplyr::rename_with(~ stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_"), .cols = 7:9) %>%
                dplyr::rename_with(~ paste0("prior", stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_")), .cols = 10:12)
    }

    else if (fy == "2425") {
        
        col_names_2425 <- stringr::str_remove(janitor::make_clean_names(col_names), "na_")

        temp_data <- readxl::read_excel(path, sheet = 1, skip = 5, col_names = col_names_2425) %>%
                dplyr::mutate(period = paste0("20", stringr::str_sub(fy, start = -2, end = -1)),
                                  dplyr::across(c("findings_on_specific_risk_areas_human_resource_management","findings_on_specific_risk_areas_information_technology"), ~ as.character(.x))) %>%
                dplyr::rename_with(~ stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_"), .cols = 7:9) %>%
                dplyr::rename_with(~ paste0("prior", stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_")), .cols = 10:12)
    }


    else {
        temp_data <- readxl::read_excel(path, sheet = 1, skip = 5, col_names = col_names) %>%
                dplyr::mutate(period = paste0("20", stringr::str_sub(fy, start = -2, end = -1)), 
                                dplyr::across(c("findings_on_specific_risk_areas_human_resource_management","findings_on_specific_risk_areas_information_technology"), ~ as.character(.x))) %>%
                dplyr::rename_with(~ stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_"),
                        .cols = 7:9) %>%
                dplyr::rename_with(~ paste0("prior", stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_")),
                        .cols = 10:12)
                        }

    df_list[[max(temp_data$period)]] <- temp_data 

    fy_annexures <- dplyr::bind_rows(fy_annexures, temp_data) 
}

view24 <- df_list[["2024"]]

view25 <- df_list[["2025"]]



fy_annexures <- fy_annexures %>%
    dplyr::mutate(auditee = case_when(
        auditee == "Emalahleni" & stringr::str_detect(province, "MP") ~ "Emalahleni (MP)",
        auditee == "Emalahleni" & stringr::str_detect(province, "EC") ~ "Emalahleni (EC)",
        auditee %in% c("Dr A B Xuma Local Municipality", "Dr A B Xuma", "Dr. A.B. Xuma") ~ "Engcobo",
        auditee %in% c("Elias Motsoaledi", "Elias Motsoaledi (Greater Groblersdal)") ~ "Elias Motsoaledi (Greater Groblersdal)",
        auditee == "Lekwa-Teemane" ~ "Lekwa Teemane",
        auditee %in% c("Maluti-A-Phofung", "Maluti-a-Phofung") ~ "Maluti-A-Phofung", 
        auditee %in% c("Dr JS Moroka", "Dr. JS Moroka") ~ "Dr. JS Moroka",
        auditee == "Big 5 Hlabisa" ~ "Big Five Hlabisa",
        auditee == "Johannes Phumani Phungula" ~ "Ubuhlebezwe",
        auditee == "Dr Nkosazana Dlamini Zuma" ~ "Dr Nkosazana Dlamini-Zuma",
        auditee == "Port St. Johns" ~ "Port St Johns",
        auditee == "Kumkani Mhlontlo" ~ "Mhlontlo",
        TRUE ~ auditee)) 

label_code_lookup <- fy_annexures %>%
    dplyr::filter(!is.na(municipal_mdb_codes)) %>%
    dplyr::select(municipal_mdb_codes, auditee) %>%
    dplyr::filter(!stringr::str_detect(stringr::str_sub(municipal_mdb_codes, start = 1, end = 2), "DC")) %>%
    dplyr::distinct(municipal_mdb_codes, auditee) 

dropcols <- c("province", "auditee_category", "number", "municipal_mdb_codes")

annexures_df <- fy_annexures %>%
    dplyr::select(-all_of(dropcols)) %>%
    dplyr::left_join(label_code_lookup, by = "auditee") %>%
    dplyr::mutate(
    unauthorised_irregular_fruitless_qual = dplyr::coalesce(
      financial_statement_qualification_areas_unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure,
      financial_statement_qualification_areas_unauthorised_irregular_and_fruitless_and_wasteful_expenditure
    ),
    unauth_amt = dplyr::coalesce(
      unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure_unauthorised_expenditure_amount_r,
      unauthorised_irregular_and_fruitless_and_wasteful_expenditure_unauthorised_expenditure_amount_r
    ),
    irreg_amt = dplyr::coalesce(
      unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure_irregular_expenditure_amount_r,
      unauthorised_irregular_and_fruitless_and_wasteful_expenditure_irregular_expenditure_amount_r
    ),
    fruitless_amt = dplyr::coalesce(
      unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure_fruitless_and_wasteful_expenditure_amount_r,
      unauthorised_irregular_and_fruitless_and_wasteful_expenditure_fruitless_and_wasteful_expenditure_amount_r
    )
    ) %>%
    dplyr::rename(demarcation.code = municipal_mdb_codes) %>%
    dplyr::filter(auditee_type %in% c("LM", "IC", "ICM", "MET"), 
                demarcation.code %in% munic_list$demarcation.code, 
                outcomes_audit_opinion == "Audit not finalised at legislated date" |
                !period != "2025" |
                !is.na(unauthorised_irregular_fruitless_qual) |
                !is.na(unauth_amt) | 
                !is.na(irreg_amt)) %>%
        dplyr::distinct(demarcation.code, period, .keep_all = TRUE)
        # dplyr::select(-auditee)


yearcheck <- annexures_df %>% dplyr::count(period)


# dupes <- annexures_df %>%
#   dplyr::group_by(period) %>%
#   dplyr::add_count(demarcation.code, name = "n") %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(n > 1) %>%
#   dplyr::arrange(period, demarcation.code)


drilldown_audop <- paste(
    "financial_year_end.year",
    "demarcation.code",
    "demarcation.label",
    "opinion.code",
    "opinion.label", sep = "|"
)

fetch_data_audop <- function(year, 
                        amount_type = "AUDA", 
                        url = "https://municipaldata.treasury.gov.za/api/cubes/audit_opinions/aggregate", 
                        drilldown = drilldown_audop,
                        pagesize = 5000, 
                        page = 1, 
                        itemcode = "fruitless"){

  base_url <- url
  
  cut <- sprintf("financial_year_end.year:%d", year)

  page <- 1 

  all_pages <- list()

  repeat{
        resp <- GET(base_url, query = list(
        drilldown = drilldown_audop,
        cut = cut, 
        page = page, 
        pagesize = pagesize, 
        format = "csv"))

        df_temp <- readr::read_csv(content(resp, "text", encoding = "UTF-8"), 
                                # col_types = readr::cols(opinion.code = readr::col_character()),
                                show_col_types = FALSE)
        if(nrow(df_temp) == 0) break 

        all_pages[[page]] <- df_temp

        if (nrow(df_temp) < pagesize) break
        page <- page + 1
        Sys.sleep(0.5)  
        
        }

        bind_rows(all_pages)

}

nt_data_audop <- list()

for (yr in (2018:2025)){

                        year_df <- fetch_data_audop(year = yr) 

                        if (nrow(year_df) > 1){
                        year_df <- year_df %>%
                        dplyr::rename(period = financial_year_end.year) %>%
                        dplyr::mutate(period = as.character(period))
                        }

                        nt_data_audop[[as.character(yr)]] <- year_df

                message(yr)
}


audop_nt <- dplyr::bind_rows(nt_data_audop) %>%
        dplyr::filter(!stringr::str_detect(stringr::str_sub(demarcation.code, 1, 2), "DC"))


audits_df <- dplyr::left_join(annexures_df, audop_nt, by = c("period", "demarcation.code")) %>%
            dplyr::filter(as.numeric(period) >= 2020) %>%
            dplyr::mutate(dplyr::across(
                                dplyr::matches("^outcomes_predetermined_objectives|^outcomes_compliance_with_legislation|financial_statement_qualification_areas|findings_on"),
                                     ~ as.numeric(dplyr::case_when(.x %in% c("N", "R") ~ 1,
                                            is.na(.x) ~ 0,
                                            .x == "A" ~ 0,
                                      TRUE ~ 0)))) %>%
            dplyr::mutate(findings = rowSums(dplyr::across(dplyr::matches("^outcomes_predetermined_objectives|^outcomes_compliance_with_legislation")), na.rm = TRUE),
                            outcomes_audit_opinion_i = dplyr::case_when(
                            stringr::str_detect(outcomes_audit_opinion, "Unqualified") &  findings == 0 ~ "Unqualified with no findings",
                            stringr::str_detect(outcomes_audit_opinion, "Unqualified") &  findings != 0 ~ "Unqualified with findings",
                            stringr::str_detect(outcomes_audit_opinion, "Qualified") &  findings == 0 ~ "Qualified with no findings",
                            stringr::str_detect(outcomes_audit_opinion, "Qualified") &  findings != 0 ~ "Qualified with findings",
                            TRUE ~ outcomes_audit_opinion
                        )) 

look <- audits_df %>%
        dplyr::filter(outcomes_audit_opinion_i == "Unqualified with no findings" & period == "2025") %>%
        dplyr::select(auditee, demarcation.code, outcomes_audit_opinion, outcomes_audit_opinion_i, findings) 

}
# req <- httr::GET("https://municipaldata.treasury.gov.za/api/cubes/audit_opinions/model") 

# model <- httr::content(req, as = "text", encoding = "UTF-8") %>%
#     jsonlite::fromJSON()

# model$model

#   httr2::req_perform() %>%
#   httr2::resp_body_json()

rm(munic_qid)
####################################################################################################################################################################################

                                                                     ## Plot Data ##

####################################################################################################################################################################################


print(unique(view24$outcomes_audit_opinion))
print(unique(view25$outcomes_audit_opinion))


line_df <- audits_df %>%
            dplyr::select(outcomes_audit_opinion_i, period) %>%
            dplyr::group_by(period, outcomes_audit_opinion_i, .drop = FALSE) %>%
            dplyr::summarise(opinions = dplyr::n()) %>%
            dplyr::filter(as.numeric(period) > 2021) %>%
            dplyr::mutate(period = as.numeric(period)) %>%
            dplyr::rename(label = outcomes_audit_opinion_i, 
                            value = opinions) 

label_colour_map <- c(
    "Unqualified with no findings"  = "#2E4B3C",
    "Unqualified with findings"     = "#5C8A5A",
    "Qualified with no findings"    = "#809360",
    "Qualified with findings"       = "#A59D65",
    "Adverse"                       = "#C9A66B",
    "Disclaimer"                    = "#7A2E2E",
    "Audit not finalised at legislated date" = "#8C8C8C"
)

if (TRUE){

line_plot <- ggplot(data = line_df, aes(x = period, y = value, group = label, colour = label)) +
        geom_line(linewidth = 2.5) +
        geom_point(size = 2.5, shape = 15) +
        geom_text(aes(label = value, hjust = ifelse(period != max(line_df$period), -0.5, 1.5)), vjust = - 2, size = 3, show.legend = FALSE) +
        scale_colour_manual(values = label_colour_map) +
        guides(colour = guide_legend(reverse = TRUE)) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 6),
                        labels  = function(x) ifelse(x == 0, "", x), 
                        expand = expansion(mult = c(0.001, 0.05))) +
        scale_x_continuous(
                breaks =  c(min(line_df$period):max(line_df$period)),
                labels = function(x) paste0(as.numeric(x) - 1, "/", as.numeric((stringr::str_sub(as.character(x), start = 3, end = 4)))), 
                expand = expansion(mult = c(0, 0))) + 
        labs(title = "Audit Opinions, 2020/21 - 2024/25", 
            x = "Financial Year",
            y = "Number of Municipalities", 
            caption = "Source: Auditor General SA") +
        substack_theme

line_plot
line_path <- paste0(output_dir, "/lineplot.png")


png(filename = line_path,
        width = 950,
        height = 650,
        res = 130)
print(line_plot)
dev.off()
}


####
## Tile Plot
####

if (TRUE){

opinion_order <- dput(unique(audits_df$outcomes_audit_opinion_i))

scoring <- c(
    "Unqualified with no findings"  = 6,
    "Unqualified with findings"     = 5,
    "Qualified with no findings"    = 4,
    "Qualified with findings"       = 3,
    "Adverse"                       = 2,
    "Disclaimer"                    = 1,
    "Audit not finalised at legislated date" = 0
)

recent_period <- max(as.numeric(audits_df$period, na.rm = TRUE))

unqual_rank <- audits_df %>%
  dplyr::mutate(
    score = dplyr::recode(as.character(outcomes_audit_opinion_i), !!!scoring, .default = NA_real_), #!!! tells R: "take this vector and unpack it into separate named arguments, one per element" — effectively expanding severity_score into exactly the four individual name = value pairs recode() is expecting, as if I'd typed them out by hand.
    weight = as.numeric(period) - min(as.numeric(period), na.rm = TRUE) + 1
  ) %>%
  dplyr::group_by(demarcation.code) %>%
  dplyr::summarise(recent_score = score[as.numeric(period) == recent_period], 
                    weighted_score = sum(score * weight, na.rm = TRUE)) %>%
  dplyr::arrange(recent_score, weighted_score)

tile_df <- audits_df %>%
        dplyr::mutate(outcomes_audit_opinion = factor(outcomes_audit_opinion_i, levels = opinion_order), 
                demarcation.code = factor(demarcation.code, unqual_rank$demarcation.code), 
                demarcation.code = forcats::fct_rev(demarcation.code)) 

tileplot <- ggplot(data = tile_df, aes(x = period, y = demarcation.code, fill = outcomes_audit_opinion_i)) +
        geom_tile() +
        scale_fill_manual(values = label_colour_map, breaks = names(label_colour_map)) +
        scale_x_discrete(
                # breaks =  c(min(line_df$period):max(line_df$period)),
                labels = function(x) paste0(as.numeric(x) - 1, "/", as.numeric((stringr::str_sub(as.character(x), start = 3, end = 4)))), 
                expand = expansion(mult = c(0, 0))) + 
        labs(title = "Audit Outcomes by Municipality, 2020/21 - 2024/25",
            x = "Financial Year", y = NULL, fill = "Audit Opinion",
            caption = "Source: Auditor General SA") +
        substack_theme +
        theme(axis.text.y = element_blank())

tileplot

tile_path <- paste0(output_dir, "/tileplot.png")


png(filename = tile_path,
        width = 1050,
        height = 650,
        res = 130)

print(tileplot)
dev.off()

}


####
## Financial Statement Qualification Areas
####

#Dumbbell plot comparing with 2020/21
if (TRUE){


}


####
## Findings on predetermined objectives
####

#Dumbbell plot comparing with 2020/21
if (TRUE){


}



####
## Findings on predetermined objectives
####

#Dumbbell plot comparing with 2020/21
if (TRUE){


}


