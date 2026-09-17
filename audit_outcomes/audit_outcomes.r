pacman::p_load(dplyr, httr, jsonlite, ggplot2, sf, leaflet, scales, cowplot, magick, patchwork)

output_dir <- here::here("audit_outcomes", "output")

git_dir <- "/Users/jansmuts/municipalitymonitor_3/"

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
            dplyr::mutate(findings = rowSums(dplyr::across(dplyr::matches("^findings_on_predetermined_objectives|^findings_on_non_compliance") &
                                        !dplyr::matches("quality_of_submitted_performance_reports|supply_chain_management")), na.rm = TRUE),
                            qualifications = rowSums(dplyr::across(dplyr::matches("^financial_statement_qualification_areas")), na.rm = TRUE),
                            outcomes_audit_opinion_i = dplyr::case_when(
                            stringr::str_detect(outcomes_audit_opinion, "Unqualified") &  findings == 0 ~ "Unqualified with no findings",
                            stringr::str_detect(outcomes_audit_opinion, "Unqualified") &  findings != 0 ~ "Unqualified with findings",
                            stringr::str_detect(outcomes_audit_opinion, "Qualified") &  findings == 0 ~ "Qualified with no findings",
                            stringr::str_detect(outcomes_audit_opinion, "Qualified") &  findings != 0 ~ "Qualified with findings",
                            TRUE ~ outcomes_audit_opinion
                        ), 
                            qualified = ifelse(stringr::str_detect(outcomes_audit_opinion_i, "^Qualified"), 1, 0)) 
                

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


line_df <- audits_df %>%
            dplyr::select(outcomes_audit_opinion_i, period) %>%
            dplyr::group_by(period, outcomes_audit_opinion_i, .drop = FALSE) %>%
            dplyr::summarise(opinions = dplyr::n()) %>%
            dplyr::filter(as.numeric(period) > 2020) %>%
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
        dplyr::select(-demarcation.label) %>%
        dplyr::left_join(munic_list %>% dplyr::select(demarcation.code, demarcation.label), by = "demarcation.code") %>%
        dplyr::mutate(outcomes_audit_opinion = factor(outcomes_audit_opinion_i, levels = opinion_order), 
                demarcation.code = factor(demarcation.code, unqual_rank$demarcation.code), 
                demarcation.code = forcats::fct_rev(demarcation.code)) 
        

tileplot <- ggplot(data = tile_df, aes(x = period, y = demarcation.code, fill = outcomes_audit_opinion_i, text = paste0(demarcation.label, "<br>", period, ": ", outcomes_audit_opinion))) +
        geom_tile() +
        scale_fill_manual(values = label_colour_map, breaks = names(label_colour_map)) +
        scale_x_discrete(
                # breaks =  c(min(line_df$period):max(line_df$period)),
                labels = function(x) paste0(as.numeric(x) - 1, "/", as.numeric((stringr::str_sub(as.character(x), start = 3, end = 4)))), 
                expand = expansion(mult = c(0, 0))) + 
        labs(title = "Audit Outcomes by Municipality, 2020/21 - 2024/25",
            x = "Financial Year", y = NULL, fill = NULL,
            caption =  "substack.com/@onderstromings\n \nSource: Auditor General SA") +
        substack_theme +
        theme(axis.text.y = element_blank(), 
                legend.title = element_blank())

tileplot

## make interactive

tileplot_hover <- plotly::ggplotly(tileplot, tooltip = "text") %>%
                plotly::layout(
    annotations = list(
      list(
        text = "  substack.com/@onderstromings<br>Source: Auditor General SA",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        x = 0, y = -0.08,
        xanchor = "left", yanchor = "top",
        font = list(size = 10, color = "#8C8C8C")
      )
    )
  )


tileplot_hover

tile_path <- paste0(output_dir, "/tileplot.png") 


png(filename = tile_path,
        width = 1050,
        height = 650,
        res = 130)

print(tileplot)
dev.off()


## make interactive

tileplot_hover <- plotly::ggplotly(tileplot, tooltip = "text")

tileplot_hover

tile_html_path <- paste0(output_dir, "/tileplot.html")


htmlwidgets::saveWidget(tileplot_hover, file = tile_html_path, selfcontained = TRUE)

}

####
## Financial Statement Qualification Areas
####

minyear <- "2021"
minyear_quote <- paste0(as.numeric(minyear) - 1, "/" ,as.numeric(stringr::str_sub(minyear, -2, -1)))
print(minyear_quote)
#Dumbbell plot comparing with 2020/21
if (TRUE){

qualifications_df <- audits_df %>%
            dplyr::select(period, dplyr::matches("^financial_statement_qualification_areas"), qualified) %>%
            dplyr::group_by(period) %>%
            dplyr::filter(period %in% c(minyear, "2025")) %>%
            dplyr::summarise(dplyr::across(c("qualified", dplyr::matches("^financial_statement_qualification_areas")), ~ sum(.x, na.rm = TRUE))) %>%
            dplyr::rename_with( ~ stringr::str_remove(.x, "financial_statement_qualification_areas_"), dplyr::matches("^financial_statement_qualification_areas_")) %>%
            dplyr::mutate(uifwe = dplyr::coalesce(unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure, 
                                    unauthorised_irregular_and_fruitless_and_wasteful_expenditure)) %>%
            dplyr::select(-unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure, 
                                    - unauthorised_irregular_and_fruitless_and_wasteful_expenditure) %>%
            tidyr::pivot_longer(cols = -period, 
                                names_to = "label", 
                                values_to = "value") %>%
            dplyr::mutate(label = case_when(
                                            label == "qualified" ~ "Qualified Audits", 
                                            label == "non_current_assets" ~ "Non-current Assets", 
                                            label == "current_assets" ~ "Current Assets", 
                                            label == "liabilities" ~ "Liabilities", 
                                            label == "capital_and_reserves" ~ "Capital and Reserves", 
                                            label == "other_disclosure_items" ~ "Other Disclosure Items", 
                                            label == "revenue" ~ "Revenue", 
                                            label == "expenditure" ~ "Expenditure", 
                                            label == "aggregate_misstatements" ~ "Aggregate Misstatements", 
                                            label == "cash_flow_statements" ~ "Cash Flow Statements", 
                                            label == "uifwe" ~ "UIFWE"
                                            )) %>%
            tidyr::pivot_wider(names_from = period,
                                values_from = value) %>%
            dplyr::mutate(change = `2025` - .data[[minyear]], 
                            maxval = pmax(.data[[minyear]], `2025`), 
                            is_max_2022 = .data[[minyear]] > `2025`,
                            is_max_2025 = `2025` > .data[[minyear]],
                            label = forcats::fct_reorder(label, maxval)) 



label_exprs <- purrr::map(levels(qualifications_df$label), function(l) {
  if (l == "Qualified Audits") bquote(bold(.(l))) else l
})

custom_title_qual <- paste0("Change in Financial Statement Qualifications, ",  minyear_quote ," - 2024/25")

qualification_plot <- ggplot() +
                geom_segment(data = qualifications_df %>% dplyr::filter(label != "Qualified Audits"), aes(x = label, xend = label, y = .data[[minyear]], yend = `2025`, colour = change > 0), 
                                size = 2) + 
                geom_point(data = qualifications_df %>% dplyr::filter(label != "Qualified Audits"), aes(x = label, y = .data[[minyear]], colour = change > 0, shape = minyear_quote), size = 4) +
                geom_point(data = qualifications_df %>% dplyr::filter(label != "Qualified Audits"), aes(x = label, y = `2025`, colour = change > 0, shape = "2024/25"), size = 4) +
                geom_text(data = qualifications_df %>% dplyr::filter(label != "Qualified Audits"),
                            aes(x = label, y = `2025`, label = `2025`, colour = change > 0, vjust = ifelse(is_max_2022, 2, -1.5)),
                            size = 4, show.legend = FALSE) +
                geom_text(data = qualifications_df %>% dplyr::filter(label != "Qualified Audits"), 
                            aes(x = label, y = .data[[minyear]], label = .data[[minyear]], colour = change > 0, vjust = ifelse(is_max_2025, 2, -1.5)),
                            size = 4, show.legend = FALSE) +
                geom_segment(data = qualifications_df %>% dplyr::filter(label == "Qualified Audits"), aes(x = label, xend = label, y = .data[[minyear]], yend = `2025`, colour = change > 0), 
                                size = 3) + 
                geom_point(data = qualifications_df %>% dplyr::filter(label == "Qualified Audits"), aes(x = label, y = .data[[minyear]], colour = change > 0, shape = minyear_quote), size = 6) +
                geom_point(data = qualifications_df %>% dplyr::filter(label == "Qualified Audits"), aes(x = label, y = `2025`, colour = change > 0, shape = "2024/25"), size = 6) +
                geom_text(data = qualifications_df %>% dplyr::filter(label == "Qualified Audits"),
                            aes(x = label, y = `2025`, label = `2025`, colour = change > 0, vjust = ifelse(is_max_2022, 2, -1.5)),
                            size = 6, fontface = "bold", show.legend = FALSE) +
                geom_text(data = qualifications_df %>% dplyr::filter(label == "Qualified Audits"), 
                            aes(x = label, y = .data[[minyear]], label = .data[[minyear]], colour = change > 0, vjust = ifelse(is_max_2025, 2, -1.5)),
                            size = 6, fontface = "bold", show.legend = FALSE) +            
                scale_colour_manual(values = c("TRUE" = "#8C5E58", "FALSE" = "#2E4B3C")) +
                scale_shape_manual(name = NULL, values = setNames(c(1, 16), c(minyear_quote, "2024/25"))) +
                labs(title = custom_title_qual,
                        x = NULL, 
                        y = "Frequency",
                        caption = "substack.com/@onderstromings\n \nSource: Auditor General SA") +
                scale_x_discrete(labels = label_exprs) +
                scale_y_continuous(limits = c(0, 80), 
                        expand = expansion(mult = c(0.05, 0.05))) +
                guides(shape = guide_legend(override.aes = list(colour = "black")),
                        colour = FALSE) +
                substack_theme + 
                theme(
                # panel.grid.major.x = element_blank(),
                # panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                text               = element_text(colour = "#1F2A24"),
                axis.text          = element_text(colour = "#4A4460"),
                axis.text.x          = element_text(colour = "#4A4460", 
                                                    angle = 45,
                                                    hjust = 1,
                                                    vjust = 1),
                legend.position    = "bottom",
                legend.key.size    = unit(0.45, "cm"),
                plot.margin        = margin(12, 16, 8, 12), 
                plot.title        = element_text(colour = "#1F2A24", face = "plain", size = rel(1), margin = margin(b = 4), hjust = 0.5),
                ) 

qualification_plot

qual_path <- paste0(output_dir, "/qualplot.png")


png(filename = qual_path,
        width = 1050,
        height = 750,
        res = 110)

print(qualification_plot)
dev.off()
}


####
## Findings on predetermined objectives and non-compliance
####


#Dumbbell plot comparing with 2020/21
if (TRUE){

findings_df <- audits_df %>%
            dplyr::select(period, (dplyr::matches("^findings_on_predetermined_objectives|^findings_on_non_compliance|^findings") & 
                        !dplyr::matches("quality_of_submitted_performance_reports|supply_chain_management|^outcomes_audit_opinion"))) %>%
            dplyr::group_by(period) %>%
            dplyr::filter(period %in% c(minyear, "2025")) %>%
            dplyr::mutate(dplyr::across(dplyr::matches("^findings_on_"), as.numeric), 
                            findings_yn = ifelse(findings > 0, 1, 0)
                            ) %>%
            dplyr::summarise(dplyr::across(dplyr::matches("^findings_|^outcomes_"), ~ sum(.x, na.rm = TRUE))) %>%
            dplyr::rename_with(~ stringr::str_remove(.x , "^findings_on_predetermined_objectives_|^findings_on_non_compliance_|^findings_on_non-compliance_"), dplyr::matches("^findings_on")) %>%
            dplyr::mutate(material_misstatement_or_limitations_in_submitted_afs = coalesce(
                                material_misstatement_or_limitations_in_submitted_afs,
                                material_misstatement_or_limitations_in_submitted_financial_statements
                            ), 
                            information_not_submitted_in_time_for_audit = coalesce(
                                information_not_submitted_in_time_for_audit,
                                information_not_submitted_in_time_for_auditing
                            ), 
                            underlying_records_planning_documents_not_submitted_for_audit = coalesce(
                                underlying_records_planning_documents_not_submitted_for_audit, 
                                underlying_records_planning_documents_not_submitted_for_auditing
                            ), 
                            unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure = coalesce(
                                unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure, 
                                unauthorised_irregular_and_fruitless_and_wasteful_expenditure
                            ), 
                            hr_management = coalesce(
                                hr_management, 
                                human_resource_management
                            )) %>%
            dplyr::select(-material_misstatement_or_limitations_in_submitted_financial_statements, 
                        -information_not_submitted_in_time_for_auditing,
                         -underlying_records_planning_documents_not_submitted_for_auditing,
                         -unauthorised_irregular_and_fruitless_and_wasteful_expenditure, 
                         -human_resource_management) %>%
            tidyr::pivot_longer(cols = -period, 
                                names_to = "label", 
                                values_to = "value") %>%
            dplyr::mutate(label = case_when(
                                            label == "reported_information_not_useful" ~ "Reported Information Not Useful",
                                            label == "reported_information_not_reliable" ~ "Reported Information Not Reliable",
                                            label == "information_not_submitted_in_time_for_audit" ~ "Information Not Submitted in Time for Audit",
                                            label == "no_annual_performance_report" ~ "No Annual Performance Report",
                                            label == "underlying_records_planning_documents_not_submitted_for_audit" ~ "Underlying Records/Planning Documents Not Submitted",
                                            label == "material_misstatement_or_limitations_in_submitted_afs" ~ "Material Misstatement or Limitations in Submitted AFS",
                                            label == "unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure" ~ "UIFWE",
                                            label == "annual_financial_statements_and_annual_report" ~ "Annual Financial Statements and Annual Report",
                                            label == "asset_management" ~ "Asset Management",
                                            label == "liability_management" ~ "Liability Management",
                                            label == "budgets" ~ "Budgets",
                                            label == "expenditure_management" ~ "Expenditure Management",
                                            label == "consequence_management" ~ "Consequence Management",
                                            label == "audit_committees" ~ "Audit Committees",
                                            label == "internal_audit" ~ "Internal Audit",
                                            label == "revenue_management" ~ "Revenue Management",
                                            label == "strategic_planning_and_performance_management" ~ "Strategic Planning and Performance Management",
                                            label == "transfer_and_conditional_grants" ~ "Transfer and Conditional Grants",
                                            label == "procurement_management" ~ "Procurement Management",
                                            label == "hr_management" ~ "HR Management",
                                            label == "environmental_management" ~ "Environmental Management",
                                            label == "other" ~ "Other", 
                                            label == "findings_yn" ~ "Audits with Findings"
                                            )) %>%
            dplyr::filter(label != "NA" | !is.na(label)) %>%
            tidyr::pivot_wider(names_from = period,
                                values_from = value) %>%
            dplyr::filter(!(.data[[minyear]] == 0 & `2025` == 0)) %>%
            dplyr::mutate(change = `2025` - .data[[minyear]], 
                            maxval = pmax(.data[[minyear]], `2025`), 
                            is_max_2022 = .data[[minyear]] > `2025`,
                            is_max_2025 = `2025` > .data[[minyear]],
                            label_findings = forcats::fct_reorder(label, maxval)) 


label_exprs <- purrr::map(levels(findings_df$label_findings), function(l) {
  if (l == "Audits with Findings") bquote(bold(.(l))) else l
})

custom_title_findings <- paste0("Change in Audit Findings, ",  minyear_quote ," - 2024/25")


findings_plot <- ggplot() +
                geom_segment(data = findings_df %>% dplyr::filter(label_findings != "Audits with Findings"), aes(x = label_findings, xend = label_findings, y = .data[[minyear]], yend = `2025`, colour = change > 0), 
                                size = 2) + 
                geom_point(data = findings_df %>% dplyr::filter(label_findings !=  "Audits with Findings"), aes(x = label_findings, y = .data[[minyear]], colour = change > 0, shape = minyear_quote), size = 4) +
                geom_point(data = findings_df %>% dplyr::filter(label_findings != "Audits with Findings"), aes(x = label_findings, y = `2025`, colour = change > 0, shape = "2024/25"), size = 4) +
                geom_text(data = findings_df %>% dplyr::filter(label_findings != "Audits with Findings"),
                            aes(x = label_findings, y = `2025`, label = `2025`, colour = change > 0, vjust = ifelse(is_max_2022, 2, -1.5)),
                            size = 4, show.legend = FALSE) +
                geom_text(data = findings_df %>% dplyr::filter(label_findings != "Audits with Findings"), 
                            aes(x = label_findings, y = .data[[minyear]], label = .data[[minyear]], colour = change > 0, vjust = ifelse(is_max_2025, 2, -1.5)),
                            size = 4, show.legend = FALSE) +
                geom_segment(data = findings_df %>% dplyr::filter(label_findings == "Audits with Findings"), aes(x = label_findings, xend = label_findings, y = .data[[minyear]], yend = `2025`, colour = change > 0), 
                                size = 3) + 
                geom_point(data = findings_df %>% dplyr::filter(label_findings == "Audits with Findings"), aes(x = label_findings, y = .data[[minyear]], colour = change > 0, shape = minyear_quote), size = 6) +
                geom_point(data = findings_df %>% dplyr::filter(label_findings == "Audits with Findings"), aes(x = label_findings, y = `2025`, colour = change > 0, shape = "2024/25"), size = 6) +
                geom_text(data = findings_df %>% dplyr::filter(label_findings == "Audits with Findings"),
                            aes(x = label_findings, y = `2025`, label = `2025`, colour = change > 0, vjust = ifelse(is_max_2022, 2, -1)),
                            size = 6, fontface = "bold", show.legend = FALSE) +
                geom_text(data = findings_df %>% dplyr::filter(label_findings == "Audits with Findings"), 
                            aes(x = label_findings, y = .data[[minyear]], label = .data[[minyear]], colour = change > 0, vjust = ifelse(is_max_2025, 2, -1)),
                            size = 6, fontface = "bold", show.legend = FALSE) +            
                scale_colour_manual(values = c("TRUE" = "#8C5E58", "FALSE" = "#2E4B3C")) +
                scale_shape_manual(name = NULL, values = setNames(c(1, 16), c(minyear_quote, "2024/25"))) +
                labs(title = custom_title_findings,
                        x = NULL, 
                        y = "Frequency",
                        caption = "substack.com/@onderstromings\n \nSource: Auditor General SA") +
                scale_x_discrete(labels = label_exprs) +
                scale_y_continuous( expand = expansion(mult = c(0.14, 0.14))) +
                guides(shape = guide_legend(override.aes = list(colour = "black")),
                        colour = FALSE) +
                substack_theme + 
                theme(
                # panel.grid.major.x = element_blank(),
                # panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                text               = element_text(colour = "#1F2A24"),
                axis.text          = element_text(colour = "#4A4460"),
                axis.text.x          = element_text(colour = "#4A4460", 
                                                    angle = 45,
                                                    hjust = 1,
                                                    vjust = 1),
                legend.position    = "bottom",
                legend.key.size    = unit(0.45, "cm"),
                plot.margin        = margin(12, 16, 8, 12), 
                plot.title        = element_text(colour = "#1F2A24", face = "plain", size = rel(1), margin = margin(b = 4), hjust = 0.5),
                ) 

findings_plot

find_path <- paste0(output_dir, "/findplot.png")


png(filename = find_path,
        width = 1050,
        height = 750,
        res = 100)

print(findings_plot)
dev.off()
}

####
## Performance by municipality
####

##Do a scatterplot with change in qualifications on Y and change in findings on X, with relative amount of quals as size

##Create qualificaitons and findings seperately then join them. 

if (TRUE){

zscore <- function(col) {
    mean_temp <- mean(col, na.rm = TRUE)
    sd_temp <- sd(col, na.rm = TRUE)

    return((col - mean_temp) / sd_temp)

}

quals_full <- audits_df %>%
            dplyr::filter(period %in% c(minyear, "2025")) %>%
            dplyr::select(demarcation.code, period, qualifications, dplyr::matches("^financial_statement_qualification_areas")) %>%
            dplyr::rename_with( ~ stringr::str_remove(.x, "financial_statement_qualification_areas_"), dplyr::matches("^financial_statement_qualification_areas_")) %>%
            dplyr::mutate(uifwe = dplyr::coalesce(unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure, 
                                    unauthorised_irregular_and_fruitless_and_wasteful_expenditure)) %>%
            dplyr::select(-unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure, 
                                    - unauthorised_irregular_and_fruitless_and_wasteful_expenditure) %>%
            tidyr::pivot_longer(cols = c(-period, -demarcation.code), 
                                names_to = "label", 
                                values_to = "value") %>%
            dplyr::mutate(label = case_when(
                                            label == "qualified" ~ "Qualified Audits", 
                                            label == "non_current_assets" ~ "Non-current Assets", 
                                            label == "current_assets" ~ "Current Assets", 
                                            label == "liabilities" ~ "Liabilities", 
                                            label == "capital_and_reserves" ~ "Capital and Reserves", 
                                            label == "other_disclosure_items" ~ "Other Disclosure Items", 
                                            label == "revenue" ~ "Revenue", 
                                            label == "expenditure" ~ "Expenditure", 
                                            label == "aggregate_misstatements" ~ "Aggregate Misstatements", 
                                            label == "cash_flow_statements" ~ "Cash Flow Statements", 
                                            label == "uifwe" ~ "UIFWE",
                                            TRUE ~ label
                                            )) %>%
            tidyr::pivot_wider(id_cols = c(demarcation.code, period),
                                names_from = label, 
                                values_from = value) 

findings_full <- audits_df %>%
                dplyr::filter(period %in% c(as.character(minyear), "2025")) %>%
                dplyr::select(period, demarcation.code, findings, outcomes_audit_opinion_i, (dplyr::matches("^findings_on_predetermined_objectives|^findings_on_non_compliance|^findings") & 
                        !dplyr::matches("quality_of_submitted_performance_reports|supply_chain_management|^outcomes_audit_opinion"))) %>%
                dplyr::group_by(demarcation.code) %>%
                dplyr::mutate(outcome_2025 = dplyr::first(outcomes_audit_opinion_i[period == "2025"], default = NA_character_)) %>%
                dplyr::ungroup() %>%
                dplyr::rename_with(~ stringr::str_remove(.x , "^findings_on_predetermined_objectives_|^findings_on_non_compliance_|^findings_on_non-compliance_"), dplyr::matches("^findings_on")) %>%
                dplyr::mutate(material_misstatement_or_limitations_in_submitted_afs = coalesce(
                                        material_misstatement_or_limitations_in_submitted_afs,
                                        material_misstatement_or_limitations_in_submitted_financial_statements
                                ), 
                                information_not_submitted_in_time_for_audit = coalesce(
                                        information_not_submitted_in_time_for_audit,
                                        information_not_submitted_in_time_for_auditing
                                ), 
                                underlying_records_planning_documents_not_submitted_for_audit = coalesce(
                                        underlying_records_planning_documents_not_submitted_for_audit, 
                                        underlying_records_planning_documents_not_submitted_for_auditing
                                ), 
                                unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure = coalesce(
                                        unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure, 
                                        unauthorised_irregular_and_fruitless_and_wasteful_expenditure
                                ), 
                                hr_management = coalesce(
                                        hr_management, 
                                        human_resource_management
                                )) %>%
                dplyr::select(-material_misstatement_or_limitations_in_submitted_financial_statements, 
                                -information_not_submitted_in_time_for_auditing,
                                -underlying_records_planning_documents_not_submitted_for_auditing,
                                -unauthorised_irregular_and_fruitless_and_wasteful_expenditure, 
                                -human_resource_management,
                                -outcomes_audit_opinion_i) %>%
                tidyr::pivot_longer(cols = c(-period, -demarcation.code, -outcome_2025), 
                                        names_to = "label", 
                                        values_to = "value") %>%
                dplyr::mutate(label = case_when(
                                                label == "reported_information_not_useful" ~ "Reported Information Not Useful",
                                                label == "reported_information_not_reliable" ~ "Reported Information Not Reliable",
                                                label == "information_not_submitted_in_time_for_audit" ~ "Information Not Submitted in Time for Audit",
                                                label == "no_annual_performance_report" ~ "No Annual Performance Report",
                                                label == "underlying_records_planning_documents_not_submitted_for_audit" ~ "Underlying Records/Planning Documents Not Submitted",
                                                label == "material_misstatement_or_limitations_in_submitted_afs" ~ "Material Misstatement or Limitations in Submitted AFS",
                                                label == "unauthorised_irregular_as_well_as_fruitless_and_wasteful_expenditure" ~ "UIFWE",
                                                label == "annual_financial_statements_and_annual_report" ~ "Annual Financial Statements and Annual Report",
                                                label == "asset_management" ~ "Asset Management",
                                                label == "liability_management" ~ "Liability Management",
                                                label == "budgets" ~ "Budgets",
                                                label == "expenditure_management" ~ "Expenditure Management",
                                                label == "consequence_management" ~ "Consequence Management",
                                                label == "audit_committees" ~ "Audit Committees",
                                                label == "internal_audit" ~ "Internal Audit",
                                                label == "revenue_management" ~ "Revenue Management",
                                                label == "strategic_planning_and_performance_management" ~ "Strategic Planning and Performance Management",
                                                label == "transfer_and_conditional_grants" ~ "Transfer and Conditional Grants",
                                                label == "procurement_management" ~ "Procurement Management",
                                                label == "hr_management" ~ "HR Management",
                                                label == "environmental_management" ~ "Environmental Management",
                                                label == "other" ~ "Other", 
                                                label == "findings_yn" ~ "Audits with Findings",
                                                label == "findings" ~ "n_findings"
                                                )) %>%
                dplyr::filter(label != "NA" | !is.na(label)) %>%
                tidyr::pivot_wider(id_cols = c(demarcation.code, period, outcome_2025),
                                        names_from = label,
                                        values_from = value) 

performance_df <- left_join(quals_full, findings_full, by = c("demarcation.code", "period")) %>%
        dplyr::select(demarcation.code, period, qualifications, n_findings, outcome_2025) %>%
        tidyr::pivot_wider(names_from = period, 
                            values_from = c(qualifications, n_findings)) %>%
        dplyr::mutate(chg_qual = (qualifications_2025 - .data[[paste0("qualifications_", minyear)]]),
                    zs_qual  = as.numeric(scale(.data[[paste0("qualifications_", minyear)]])),
                    chg_findings = (n_findings_2025 - .data[[paste0("n_findings_", minyear)]]),
                    zs_findings  = as.numeric(scale(.data[[paste0("n_findings_", minyear)]])),
                )

performance_df <- performance_df %>%
            dplyr::mutate(dplyr::across(c(chg_qual, chg_findings), ~ {
                max_finite <- max(.x[is.finite(.x)], na.rm = TRUE)
                dplyr::case_when(is.na(.x) ~ 0,
                                is.infinite(.x) ~ max_finite + 100, 
                                TRUE ~ .x)})) %>%
                left_join(munic_list %>% dplyr::select(demarcation.code, demarcation.label),  by = "demarcation.code")

##I flipped the signs so that right and top are good

scatterplot <- ggplot(data = performance_df, aes(x = chg_qual, y = chg_findings, colour = outcome_2025, text = ifelse(outcome_2025 != "Audit not finalised at legislated date", paste0(demarcation.label, " (", demarcation.code, ")", "<br>", " 2024/25 Audit Opinion : ", outcome_2025, "<br>", "Qualifications : ", qualifications_2025, " (", qualifications_2021, ")", "<br>", "Findings : ", n_findings_2025, " (", n_findings_2021, ")"), paste0(demarcation.label, " (", demarcation.code, ")", "<br>", " 2024/25 Audit Opinion : ", outcome_2025, "<br>", "Qualifications : NA (", qualifications_2021, ")", "<br>", "Findings : NA (", n_findings_2021, ")")))) +
                geom_hline(yintercept = 0, linewidth = 0.75, colour = "#2E4B3C", linetype = "dashed") +
                geom_vline(xintercept = 0, linewidth = 0.75, colour = "#2E4B3C", , linetype = "dashed") + 
                geom_point(aes(size = (n_findings_2021 + 5) * (qualifications_2021 + 5), colour = outcome_2025), alpha = 0.75, position = position_jitter(width = 0.2, height = 0.3), show.legend = FALSE) +
                scale_size_area(max_size = 12) +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 6), 
                                expand = c(0.05, 0.05)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 6), 
                                expand = c(0.05, 0.05)) +
                scale_colour_manual(values = label_colour_map) +
                labs(title = "Change in Qualifications and Findings by Municipalities,\n2020/21 - 2024/25", 
                x = "Change in Qualifications", 
                y = "Change in Findings", 
                caption = "substack.com/@onderstromings\n \nSource: Auditor General SA\nNote: Right and top quadrants indicate deteriorations, larger dots reflect initial number of findings and qualifications") +
                substack_theme +
                theme(
                panel.grid.major.x = element_blank(),
                # panel.grid.minor.x = element_blank(),
                # panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                text               = element_text(colour = "#1F2A24"),
                # axis.text          = element_text(colour = "#4A4460"),
                # axis.text.x          = element_text(colour = "#4A4460", 
                #                                     angle = 45,
                #                                     hjust = 1,
                #                                     vjust = 1),
                legend.position    = "bottom",
                legend.key.size    = unit(0.45, "cm"),
                plot.margin        = margin(12, 16, 8, 12), 
                ) 

scatterplot

scatter_path <- paste0(output_dir, "/scatterplot.png")


png(filename = scatter_path,
        width = 950,
        height = 850,
        res = 130)

print(scatterplot)
dev.off()

scatter_hover <- plotly::ggplotly(scatterplot, tooltip = "text") %>%
        plotly::layout(
    annotations = list(
      list(
        text = "  substack.com/@onderstromings<br>Source: Auditor General SA<br>Note: Right and top quadrants indicate deteriorations, larger dots reflect initial number of findings and qualifications",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        x = 0, y = -0.30,
        xanchor = "left", yanchor = "top",
        font = list(size = 10, color = "#8C8C8C")
      )
    )
  )

scatter_hover

hover_path <- paste0(output_dir, "/tileplot_hover.html")

scatter_html_path <- paste0(output_dir, "/scatterplot.html")
htmlwidgets::saveWidget(tileplot_hover, file = scatter_html_path, selfcontained = TRUE)


}


####
## GeoMap
####


if (TRUE){

# Leaves will bt coloured by 24/25 outcome and contain     1) Municipality Name
#                         2) 2020/21 OUTCOME, QUALIFICATIONS, FINDINGS
#                         3) 2021/22 OUTCOME, QUALIFICATIONS, FINDINGS
#                         4) 2022/23 OUTCOME, QUALIFICATIONS, FINDINGS
#                         5) 2023/24 OUTCOME, QUALIFICATIONS, FINDINGS
#                         6) 2024/25 OUTCOME, QUALIFICATIONS, FINDINGS
                          
map_resp <- httr::GET("https://services7.arcgis.com/oeoyTUJC8HEeYsRB/arcgis/rest/services/LocalMunicipalities2018_Final/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")

writeLines(httr::content(map_resp, "text", encoding = "UTF-8"), here::here("data", "munic_map.geojson"))

map <- sf::st_read(here::here("data", "munic_map.geojson")) %>%
                dplyr::select(CAT_B, DATE, Shape__Area, Shape__Length, geometry) %>%
                dplyr::rename(demarcation.code = CAT_B)


map_df_data <- audits_df %>%
        dplyr::select(demarcation.code, period, findings, qualifications, outcomes_audit_opinion_i) %>%
        dplyr::rename(outcome = outcomes_audit_opinion_i) %>%
                tidyr::pivot_wider(
                        id_cols = demarcation.code, 
                        names_from = period,
                        values_from = c(findings, qualifications, outcome)
                )

map_df <- dplyr::left_join(dplyr::left_join(map_df_data, map, by = "demarcation.code"), munic_list %>% dplyr::select(demarcation.code, demarcation.label), by = "demarcation.code")  
                
pal <- leaflet::colorFactor(
  palette = unname(label_colour_map),
  levels = names(label_colour_map)
)

leaf_info <- list()

for (i in c(2021:2025)){

        i_char <- as.character(i)

        outcome <- paste0("outcome_", i_char)
        qualifications <- paste0("qualifications_", i_char)
        findings <- paste0("findings_", i_char)

        year <- paste0(as.character(i - 1), "/", stringr::str_sub(i_char, -2, -1))
        
        leaf_info[[i_char]] <- ifelse(map_df[[outcome]] != "Audit not finalised at legislated date",
                 paste0(
        "<b>", year , "</b><br>",
        "&nbsp;&nbsp;&nbsp;Audit Opinion: ", map_df[[outcome]], "<br>",
        "&nbsp;&nbsp;&nbsp;Qualifications: ", map_df[[qualifications]], "<br>",
        "&nbsp;&nbsp;&nbsp;Findings: ", map_df[[findings]], "<br>"),
                paste0(
        "<b>", year , "</b><br>",
        "&nbsp;&nbsp;&nbsp;Audit Opinion: ", map_df[[outcome]], "<br>",
        "&nbsp;&nbsp;&nbsp;Qualifications: NA", "<br>",
        "&nbsp;&nbsp;&nbsp;Findings: NA", "<br>"))


}

combined_labels <- purrr::pmap_chr(leaf_info, paste0)

interact_map <- leaflet(
                map_df %>%
                st_as_sf()
                ) %>%
        addPolygons(
                fillColor = ~pal(outcome_2025),
                weight = 1, 
                color = "white",
                fillOpacity = 0.75,
                highlightOptions = highlightOptions(
                        weight = 2,
                        color = "black",
                        fillOpacity = 0.9, 
                        bringToFront = TRUE
                ),
                label = ~lapply(paste0(
  "<b>", demarcation.label, "</b><br>", combined_labels)
  , htmltools::HTML),
        labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "6px 8px"),
        textsize = "13px",
        direction = "auto"
        )
        ) %>%
        addLegend(
                 pal = pal,
                values = ~outcome_2025,
                title = "2024/25 Audit Opinion",
                position = "bottomright") %>%
        htmlwidgets::onRender("
    function(el, x) {
      this.getContainer().style.background = '#EDE8DA';
    }
  ")

interact_map


filepath_html <- paste0(output_dir, "/audit_map.html")
htmlwidgets::saveWidget(interact_map, file = filepath_html, selfcontained = TRUE)



filepath_png <- paste0(output_dir, "/audit_map.png")
webshot2::webshot(filepath_html, file = filepath_png, cliprect = "viewport")
}


####
## Make HTMLs Visual
####

plot_meta <- list(
  audit_map = list(
    title = "Municipal Audit Outcomes Map",
    description = "Interactive map of municipal audit opinions, 2020/21-2024/25",
    image = "preview_map.png"
  ),
  scatterplot = list(
    title = "Change in Qualifications and Findings by Municipality",
    description = "Interactive scatter plot of audit outcome trends, 2020/21-2024/25",
    image = "preview_scatter.png"
  ),
  tileplot = list(
    title = "Audit Outcomes by Municipality, 2020/21-2024/25",
    description = "Interactive tile plot of municipal audit opinions over five years",
    image = "preview_tileplot.png"
  )
)

for (pl in names(plot_meta)) {
  path_in  <- paste0(output_dir, "/", pl, ".html")
  path_out <- paste0(output_dir,"/", pl, "_vis.html")

  html_content <- readLines(path_in)
  head_line <- grep("<head>", html_content)[1]

  meta <- plot_meta[[pl]]
  og_tags <- c(
    sprintf('<meta property="og:title" content="%s" />', meta$title),
    sprintf('<meta property="og:description" content="%s" />', meta$description),
    sprintf('<meta property="og:image" content="https://ja-n33.github.io/municipalitymonitor_3/%s" />', meta$image),
    sprintf('<meta property="og:url" content="https://ja-n33.github.io/municipalitymonitor_3/%s_vis.html" />', pl)
  )

  writeLines(append(html_content, og_tags, after = head_line), path_out)
}
