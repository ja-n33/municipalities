pacman::p_load(dplyr, httr, jsonlite, ggplot2, sf, leaflet, scales, cowplot, magick, patchwork)

output_dir <- here::here("revenue", "output")

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

                                                        ## MUNICIPAL FINANCIAL DATA ##

####################################################################################################################################################################################

drilldown_oprev <- paste("amount_type.code", 
                        "financial_year_end.year",
                        # "function.category_label",
                        # "function.code",
                        # "function.label", ##the  function breakdown adds a lot of additinal complexity
                        "item.code", 
                        "item.label",
                        "demarcation.code",  sep  = "|")

fetch_data_op_rev <- function(year = 2025, 
                        amount_type = "AUDA", 
                        url = "https://municipaldata.treasury.gov.za/api/cubes/incexp_v2/aggregate", 
                        drilldown = drilldown_oprev,
                        pagesize = 5000, 
                        page = 1, 
                        itemcode = item){

  base_url <- url
  
  cut <- sprintf('financial_year_end.year:%d|item.code:"%s"|amount_type.code:"%s"', year, itemcode, amount_type)

  page <- 1 

  all_pages <- list()

  repeat{
        resp <- GET(base_url, query = list(
        drilldown = drilldown,
        cut = cut, 
         aggregates = "amount.sum",
        page = page, 
        pagesize = pagesize, 
        format = "csv"))

        df_temp <- readr::read_csv(content(resp, "text", encoding = "UTF-8"), 
                                col_types = readr::cols(item.code = readr::col_character(),
                                amount.sum = readr::col_double()),
                                show_col_types = FALSE)
        if(nrow(df_temp) == 0) break 

        all_pages[[page]] <- df_temp

        if (nrow(df_temp) < pagesize) break
        page <- page + 1
        Sys.sleep(0.5)  
        
        }

        bind_rows(all_pages)

}



resp_rev <- httr::GET("https://municipaldata.treasury.gov.za/api/cubes/incexp_v2/members/item")

items_rev <- jsonlite::fromJSON(httr::content(resp_rev, "text", encoding = "UTF-8"))

itemcodes_rev <- items_rev$data %>%
        dplyr::select(item.code) %>%
        dplyr::filter(as.integer(item.code) < 2900 |  as.integer(item.code) %in% c(3500, 4000)) %>%
        dplyr::pull(item.code) 

itemcodes_rev_vect <- paste(itemcodes_rev, collapse = '";"')

nt_revenue <- list()

for (year in c(2020:2025)){
        for (budget in c("ORGB", "AUDA")){
            temp_df <- fetch_data_op_rev(year = year,
                                            amount_type = budget, 
                                            itemcode = itemcodes_rev_vect
                                            )

            nt_revenue[[as.character(year)]][[budget]] <- temp_df
            message(paste0(year, ", ", budget))
                                    }
}

df_raw <- c()

for (year in c(2020:2025)){
            temp_df <- bind_rows(nt_revenue[[as.character(year)]]) %>%
                    dplyr::mutate(item.label = dplyr::case_when(item.code == "2500" ~ "operational_revenue_i",
                                                     TRUE ~ item.label))


            df_raw <- rbind(df_raw, temp_df)
}

oprev_df <- df_raw %>%
    dplyr::rename(period = financial_year_end.year) %>%
    tidyr::pivot_wider(id_cols = c(demarcation.code, period, amount_type.code), 
                        names_from = item.label,
                        values_from = amount.sum) %>%
    dplyr::rename_with( ~ janitor::make_clean_names(.x), .cols = 4:32)

####
## Get debtor data
####

drilldown_debtors <- paste(
  "amount_type.label", "amount_type.code",
  "customer_group.code", 
  "financial_period.period",
  "financial_year_end.year",
  "item.label", "item.code",
  "demarcation.label", "demarcation.code",
  sep = "|"
)

resp_debt <- httr::GET("https://municipaldata.treasury.gov.za/api/cubes/aged_debtor_v2/members/item")

items_debt <- jsonlite::fromJSON(httr::content(resp_debt, "text", encoding = "UTF-8"))

itemcodes_debt <- items_debt$data %>%
        dplyr::select(item.code, item.label) %>%
        dplyr::filter(as.numeric(item.code) <= 1700) %>%
        dplyr::pull(item.code) 

itemcodes_debt_vect <- paste(itemcodes_debt, collapse = '";"')

fetch_data_debtors <- function(year, 
                        url = "https://municipaldata.treasury.gov.za/api/cubes/aged_debtor_v2/aggregate", 
                        drilldown = drilldown_debtors,
                        pagesize = 5000, 
                        page = 1, 
                        itemcode = itemcodes_debt, 
                        amount_type = "AUDA",
                        aggregate = "total_amount.sum"){

  base_url <- url

  
  cut <- sprintf('amount_type.code:"%s"|financial_year_end.year:%d|item.code:"%s"|period_length.length:%s', amount_type, year, itemcodes_debt_vect, "year")

  page <- 1 

  all_pages <- list()

  repeat{
        resp <- GET(base_url, query = list(
        drilldown = drilldown,
        cut = cut, 
        aggregates = aggregate,
        page = page, 
        pagesize = pagesize, 
        format = "csv"))

        df_temp <- readr::read_csv(content(resp, "text", encoding = "UTF-8"), 
                                col_types = readr::cols(item.code = readr::col_character()),
                                show_col_types = FALSE)
        if(nrow(df_temp) == 0) break 

        all_pages[[page]] <- df_temp

        if (nrow(df_temp) < pagesize) break
        page <- page + 1
        Sys.sleep(0.5)  
        
        }

        bind_rows(all_pages)

}

nt_data_debtor <- list()

for (yr in c(2020:2025)){
                aggs <- "total_amount.sum|badi_amount.sum|bad_amount.sum|l30_amount.sum|l60_amount.sum|l90_amount.sum|l120_amount.sum|l150_amount.sum|l180_amount.sum|g1_amount.sum|l1_amount.sum"
                debtor_url <- "https://municipaldata.treasury.gov.za/api/cubes/aged_debtor_v2/aggregate"

                year_df <- fetch_data_debtors(year = yr, url = debtor_url, aggregate = aggs)

                nt_data_debtor[[as.character(yr)]] <- year_df

                message(paste0(yr, " completed"))
                }

debt <- bind_rows(nt_data_debtor)

debt_merge <- debt %>%
        dplyr::select(dplyr::any_of(c("customer_group.code", "financial_period.period", "item.code", "demarcation.code", "total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")),
                        dplyr::matches("^l"), amount_type.code) %>%
        dplyr::rename(period = financial_period.period) %>%
        dplyr::group_by(demarcation.code, period, amount_type.code) %>%
        dplyr::summarise(
                        dplyr::across(c(dplyr::any_of(c("total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")), dplyr::matches("^l")),
                         ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(across(c("g1_amount.sum", dplyr::matches("^l")), ~.x / total_amount.sum * 100, .names = "{stringr::str_remove(.col, 'amount.sum')}pct"))

###
## CPI
###

df_cpi <- econdatar::read_dataset(id = "CPI_ANL_SERIES",
                            series_key = "CPS00000") %>%
                            as_tibble() %>%
                            mutate(period = ifelse(lubridate::month(time_period) < 7, 
                                                    lubridate::year(time_period), 
                                                    lubridate::year(time_period) + 1), 
                                    value = CPS00000) %>%
                            group_by(period) %>%
                            dplyr::summarize(cpi = (sum(value) / 12)) %>%
                            dplyr::filter(period >= 2020) %>%
                            mutate(cpi_indexed = cpi / cpi[period == 2021]) 

####
## Audit Opinions
####

drilldown_audop <- paste(
    "financial_year_end.year",
    "demarcation.code",
    "demarcation.label",
    "opinion.code",
    "opinion.label", sep = "|"
)

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
    dplyr::filter(!stringr::str_detect(stringr::str_sub(municipal_mdb_codes, start = 1, end = 2), "DC")) 

dropcols <- c("province", "auditee_category", "number", "municipal_mdb_codes")

annexures_df <- fy_annexures %>%
    dplyr::select(-all_of(dropcols)) %>%
    dplyr::left_join(label_code_lookup, by = "auditee") %>%
    dplyr::rename(demarcation.code = municipal_mdb_codes) %>%
    dplyr::filter(auditee_type %in% c("LM", "IC", "ICM", "MET")) %>%
    dplyr::distinct(demarcation.code, period, .keep_all = TRUE) %>%
    dplyr::select(period, demarcation.code, outcomes_audit_opinion)



####
## Join Datasets
###

revenue_df <- dplyr::left_join(left_join(oprev_df, munic_list %>% dplyr::select(demarcation.code, demarcation.label, municipality.miif_category, district.miif_category), by = "demarcation.code"), debt_merge, by = c("period", "demarcation.code", "amount_type.code")) %>%
                    dplyr::filter(!(stringr::str_detect(stringr::str_sub(demarcation.code, 1, 2), "DC"))) %>%
                    dplyr::mutate(revenue_group = case_when(municipality.miif_category == "A" ~ "A",
                                        municipality.miif_category == "B1" ~ "B1",
                                        municipality.miif_category == "B2" & district.miif_category == "C1" ~ "B2/C1",
                                        municipality.miif_category == "B2" & district.miif_category == "C2" ~ "B2/C2",
                                        municipality.miif_category == "B3" & district.miif_category == "C1" ~ "B3/C1",
                                        municipality.miif_category == "B3" & district.miif_category == "C2" ~ "B3/C2",
                                        municipality.miif_category == "B4" & district.miif_category == "C1" ~ "B4/C1",
                                        municipality.miif_category == "B4" & district.miif_category == "C2" ~ "B4/C2"))





itemresp <- httr::GET("https://municipaldata.treasury.gov.za/api/cubes/incexp_v2/members/item")

items_list <- jsonlite::fromJSON(httr::content(itemresp, "text", encoding = "UTF-8"))




####################################################################################################################################################################################

                                                        ## PLOT DATA ##

####################################################################################################################################################################################


##Do a stacked plot with the top 6 categories and then 1 for others

if (TRUE){

bar_df <- revenue_df %>%
        dplyr::filter(amount_type.code == "AUDA", period >= 2021) %>%
        dplyr::select(period, colnames(revenue_df[, 4:30])) %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(across(c(any_of(c(colnames(revenue_df)))), ~sum(.x, na.rm = TRUE))) %>%
        tidyr::pivot_longer(-period,
                names_to = "type",
                values_to = "value") %>%
        dplyr::filter(!type %in% c("debt_impairment", "irrecoverable_debts_written_off"))
                                
barsum_df <- bar_df %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(total_revenue = sum(value, na.rm = TRUE)) %>%
        dplyr::left_join(df_cpi, by = "period") %>%
        dplyr::mutate(revenue_inf = total_revenue / cpi_indexed, 
                        revenue_indexed = revenue_inf / revenue_inf[period == "2021"] * 100) %>%
        dplyr::select(period, revenue_indexed) %>%
        tidyr::pivot_longer(-period, 
                            names_to = "type", 
                            values_to = "value")

top_items <- bar_df %>%
        dplyr::filter(period == 2025) %>%
        dplyr::arrange(desc(value)) %>%
        head(5) %>%
        pull(type)

items <- c(unlist(top_items), "other")

item_colours <- c(
                "#2E4B3C", 
                "#A3B86C",
                "#5C8A5A",
                "#C9A66B",
                "#7A6B4F", 
                "#9E9E9E"
)

item_labels <- c(
            "Electricity Services",
            "Property Rates",
            "Operational Transfers and Subsidies",
            "Water Services", 
            "Waste Water Management Services", 
            "Other"
)

shapes <- c(
    "0300" = 16,   # Electricity — circle
    "0200" = 18,   # Property Rates — diamond
    "2200" = 15,   # Operational Transfers — square
    "0400" = 17,   # Water — triangle
    "0500" = 8,    # Waste Water — asterisk
    "other" = 4   # Other — cross
)

ref_table <- tibble::tibble(item = items, 
                item_colours = item_colours, 
                item_labels = item_labels, 
                item_shapes = shapes)

label_map <- setNames(ref_table$item_labels, ref_table$item)

colour_map <- setNames(ref_table$item_colours, ref_table$item)

shape_map <- setNames(ref_table$item_shapes, ref_table$item)

bar_df <- bar_df %>%
        dplyr::mutate(type = ifelse(type %in% top_items, type, "other")) %>%
        dplyr::group_by(period, type) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(type = factor(type, levels = items)) %>%
        dplyr::bind_rows(barsum_df) %>%
        dplyr::arrange(period) %>%
        dplyr::group_by(type) %>%
        dplyr::mutate(growth = (value - lag(value)) / lag(value))

primary_lo <- 100e9   # position for index = 80
primary_hi <- 450e9   # position for index = 130

axis_fact  <- (primary_hi - primary_lo) / (130 - 80)
axis_offset <- primary_lo - axis_fact * 80


area_plot <- ggplot(data = bar_df, aes(x = period, y = value, fill = type)) + 
        geom_area(linewidth = 4, 
                outline.type = "full",
                position = position_stack(reverse = TRUE), 
                aes(fill = type)) +
        geom_line(data = bar_df %>% dplyr::filter(type == "revenue_indexed"), 
                    aes(x = period, y = axis_offset + value * axis_fact), 
                    linewidth = 1.5, 
                    linetype = "dashed") + 
        scale_fill_manual(values = colour_map, labels = label_map) +
        scale_y_continuous(name = "Amount (R billion)", 
                breaks = scales::breaks_pretty(n = 6),
                minor_breaks = scales::breaks_pretty(n = 12),
                labels  = function(x) ifelse(x == 0, "", scales::label_number(scale = 1e-9, big.mark = ",")(x)), 
                expand = expansion(mult = c(0.001, 0.05)), 
                sec.axis = sec_axis(~ (. - axis_offset) / axis_fact, 
                        name = "Operational Revenue Index (2020/21 = 100)", 
                        breaks = seq(80, 130, by = 10),
                        labels  = function(x)(x))) +
        scale_x_continuous(name = "Financial Year", 
                breaks =  c(2018:2026),
                labels = function(x) paste0(x-1, "/", stringr::str_sub(x, start = 3, end = 4)), 
                expand = expansion(mult = c(0, 0.025))) + 
        labs(title    = "Municipal Operational Revenue Composition,  2020/21 - 2024/25",
        x        = "Financial Year",
        caption = "substack.com/@onderstromings\n \nSource: EconData, National Treasury, Statistics SA") +
        guides(fill = guide_legend(title = NULL, order = 1)) +
        substack_theme + 
        theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                panel.border = element_blank())


area_plot
area_path <- paste0(output_dir, "/areaplot.png")


png(filename = area_path,
        width = 1050,
        height = 700,
        res = 130)
print(area_plot)
dev.off()

}

####
## Scatter plot for each year
####

if (TRUE){

scatter_df <- revenue_df %>%
    dplyr::filter(period >= 2021)
    dplyr::select(!(colnames(revenue_df)[stringr::str_detect(colnames(revenue_df), "demarcation.label|miif|revenue_group|impair|debt_")])) %>%
    dplyr::group_by(period, amount_type.code, demarcation.code) %>%
    dplyr::summarise(dplyr::across(any_of(c(colnames(revenue_df))), ~sum(.x, na.rm = TRUE))) %>%
    dplyr::group_by(period, demarcation.code) %>%
    dplyr::filter(any(amount_type.code == "AUDA" & 
                       rowSums(across(any_of(colnames(revenue_df[, 4:30]))), na.rm = TRUE) > 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-demarcation.code) %>%
    dplyr::group_by(period, amount_type.code) %>%
    dplyr::summarise(dplyr::across(any_of(c(colnames(revenue_df))), ~sum(.x, na.rm = TRUE))) 

scatter_list <- list()
scatter_max <- max(scatter_df[, 4:ncol(scatter_df), ], na.rm = TRUE)

scatter_range <- c(min(is.integer(scatter_df[, 4:ncol(scatter_df) ]), na.rm = TRUE), max(scatter_df[, 4:ncol(scatter_df), ], na.rm = TRUE))

scatter_list <- list()

for (fy in unique(revenue_df$period)){

    fy_full <- paste0(as.character(fy-1), "/", as.character(stringr::str_sub(fy, -2, -1)))

    temp_title <- paste0("Projected vs Audited Actual Revenue by Source, ", fy_full)

                
    temp_df <- scatter_df[, 1:30] %>%
                    dplyr::ungroup() %>%
                    dplyr::filter(period == fy) %>%
                    dplyr::select(-period) %>%
                    tidyr::pivot_longer(-amount_type.code, 
                                        names_to = "type",
                                        values_to = "value") %>%
                    tidyr::pivot_wider(names_from = amount_type.code) %>%
                    dplyr::filter(type != "period") %>%
                    dplyr::mutate(type = ifelse(type %in% top_items, type, "other")) %>%
                    dplyr::group_by(type) %>%
                    dplyr::summarise(across(c("AUDA", "ORGB"), ~sum(.x, na.rm = TRUE)),) %>%
                    dplyr::mutate(direction = factor(ifelse(AUDA > ORGB, "Exceeded", "Fell Short"),
                                  levels = c("Exceeded", "Fell Short")), 
                                  type = factor(type, levels = ref_table$item)) %>%
                    dplyr::left_join(ref_table %>% dplyr::select(-item_colours), by = c("type" = "item"))


    temp_scatter <- ggplot(data = temp_df, aes(x = ORGB, y = AUDA, colour = direction, shape = type)) + 
                            geom_abline(slope = 1, intercept = 0, linewidth = 1, linetype = "dashed", colour = "#2E4B3C") +
                            geom_point(size = 6) + 
                            scale_x_continuous(name = "Original Budget (R Billion)",
                                        limits = c(0, 1.7e+11), 
                                        labels  = function(x) ifelse(x == 0, "", scales::label_number(scale = 1e-9, big.mark = ",")(x)),
                                        expand = expansion(mult = c(0, 0.05))) +
                            scale_y_continuous(name = "Audited Budget (R Billion)",
                                        limits = c(0, 1.7e+11), 
                                        labels  = scales::label_number(scale = 1e-9, big.mark = ","),
                                        expand = expansion(mult = c(0, 0.05))) +
                            scale_colour_manual(values = c("Exceeded" = "#5C8A5A",  "Fell Short" = "#8C5E58"),
                                                drop = FALSE,  
                                                guide = guide_legend(title = NULL, nrow = 1, ncol = 2, order = 1)) +
                            scale_shape_manual(values = shape_map, labels = item_labels, 
                                                guide = guide_legend(title = NULL, nrow = 3, ncol = 2, order = 2)) +
                            labs(title = fy_full, 
                                    x = "Original Budget", 
                                    y = "Audited Budget", 
                                    # caption = "substack.com/@onderstromings\n \nSource: National Treasury"
                                    ) +
                            substack_theme +
                            theme(panel.grid.major = element_blank(), 
                                    legend.position = "none")

    scatter_list[[as.character(fy)]] <- temp_scatter

}

legend_df <- tidyr::expand_grid(
  type = factor(ref_table$item, levels = ref_table$item),
  direction = factor(c("Exceeded", "Fell Short"), levels = c("Exceeded", "Fell Short"))
) %>%
  dplyr::mutate(x = 1, y = 1)   # dummy coordinates, values don't matter

legend_plot <- ggplot(legend_df, aes(x = x, y = y, colour = direction, shape = type)) +
  geom_point(size = 6) +
  scale_colour_manual(values = c("Exceeded" = "#5C8A5A", "Fell Short" = "#8C5E58"), 
                       guide = guide_legend(title = NULL, nrow = 2, ncol = 1, order = 1)) +
  scale_shape_manual(values = shape_map, labels = item_labels, 
                      guide = guide_legend(title = NULL, nrow = 2, ncol = 3, order = 2)) +
  substack_theme +
  theme(legend.position = "bottom", legend.box = "horizontal")

shared_legend <- cowplot::get_legend(legend_plot)

panels <- patchwork::wrap_plots(scatter_list, nrow = 1, ncol = 5) +
          patchwork::plot_layout(axis_titles = "collect", axes = "collect")

scatter_combined_plot <- (panels / patchwork::wrap_elements(shared_legend) + 
                ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#EDE8DA", colour = NA))) +
                patchwork::plot_layout(axis_titles = "collect", 
                                        guides = "collect", 
                                        axes = "collect", 
                                        heights = c(3.5, 0.75)) +
                patchwork::plot_annotation(title = "Projected vs Audited Revenue by Source, 2021/22 - 2024/25",
                                caption = "substack.com/@onderstromings\nSource: National Treasury\nNote: Municipalities with missing audits removed.", 
                                theme = substack_theme + theme(axis.title = element_text(face = "plain"), 
                                                                plot.background = element_rect(fill = "#EDE8DA", colour = NA),
                                                                legend.position = "bottom",
                                                                legend.box = "horisontal",
                                                                legend.background = element_rect(fill = "#EDE8DA", colour = NA),
                                                                legend.box.background = element_rect(fill = "#EDE8DA", colour = NA)))

scatter_combined_plot
scatter_year_path <- paste0(output_dir, "/budget_scatter.png")

png(filename = scatter_year_path,
        width = 1100,
        height = 500,
        res = 110)
print(scatter_combined_plot)
dev.off()
}

####
## Over/under
####

if (TRUE){

col_df <- revenue_df %>%
        dplyr::filter(period >= 2021)
        dplyr::select(!(colnames(revenue_df)[stringr::str_detect(colnames(revenue_df), "demarcation.label|miif|revenue_group|debt_|irrecoverable")])) %>%
        dplyr::group_by(period, amount_type.code, demarcation.code) %>%
        dplyr::summarise(dplyr::across(any_of(c(colnames(revenue_df))), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        dplyr::group_by(period, demarcation.code) %>%
        dplyr::filter(any(amount_type.code == "AUDA" & 
                        rowSums(across(any_of(colnames(revenue_df[, 4:30]))), na.rm = TRUE) > 0)) %>%
        dplyr::ungroup() %>%
        dplyr::select(1:30, -demarcation.code) %>%
        dplyr::group_by(period, amount_type.code) %>%
        dplyr::summarise(across(c(any_of(c(colnames(revenue_df)))), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        tidyr::pivot_longer(!c(period, amount_type.code), 
                                        names_to = "type",
                                        values_to = "value") %>%
        tidyr::pivot_wider(names_from = amount_type.code) %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(across(c("AUDA", "ORGB"), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        dplyr::mutate(difference = AUDA - ORGB, 
                        pct = AUDA / ORGB * 100)

pct_min <- 95
primary_min <- -10e+9

pct_max <- 105
primary_max <- 10e+9

pct_scale  <- (primary_max - primary_min) / (pct_max - pct_min)
pct_offset <- primary_min - pct_scale * pct_min

to_primary_pct   <- function(pct) pct_offset + pct * pct_scale
to_secondary_pct <- function(primary_val) (primary_val - pct_offset) / pct_scale

year_cols <- ggplot(data = col_df, aes(x = period, y = difference)) +
            geom_col(data = col_df, aes(x = period, y = difference, fill = difference > 0)) +
            geom_line(data = col_df, aes(x = period, y = to_primary_pct(pct)), linewidth = 1.5, linetype = "dashed") +
            scale_fill_manual(values = c("TRUE" = "#5C8A5A",  "FALSE" = "#8C5E58")) +
            scale_y_continuous(name = "Difference (R Billion)",
                                labels  = scales::label_number(scale = 1e-9, big.mark = ","), 
                                breaks = scales::pretty_breaks(n = 6), 
                                expand = 0.15, 
                                sec.axis = sec_axis(~ to_secondary_pct(.), 
                                            name = "% of Original Budget", 
                                            breaks = seq(90, 110, by = 5))
                                            ) + 
            scale_x_continuous(name = "Financial Year", 
                breaks =  c(2018:2026),
                labels = function(x) paste0(x-1, "/", stringr::str_sub(x, start = 3, end = 4)), 
                expand = 0.05) + 
            labs(title = "Difference between Audited Budget Revenue and Original Budget Revenue,\n2021/22 - 2024/25",
                    caption = "substack.com/@onderstromings\n \nSource: National Treasury\nNote: Municipalities with missing audits removed.") +
            substack_theme +
            theme(legend.position = "none", 
                    panel.grid.major.x  = element_blank()
            )
year_cols

year_cols_path <- paste0(output_dir, "/year_cols.png")

png(filename = year_cols_path,
        width = 1100,
        height = 650,
        res = 110)

print(year_cols)

dev.off()

}

####
## Composition by MIIF
####

miif_df <- revenue_df %>%
        dplyr::filter(amount_type.code == "AUDA") %>%
        dplyr::select(period, colnames(revenue_df[, 4:30]), revenue_group, demarcation.code) %>%
        dplyr::group_by(period, demarcation.code, revenue_group) %>%
        dplyr::summarise(across(c(any_of(c(colnames(revenue_df)))), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        dplyr::mutate(total = rowSums(across(any_of(c(colnames(revenue_df[, 4:30])))), na.rm = TRUE)) %>%
        tidyr::pivot_longer(!c(period, demarcation.code, revenue_group, total), 
                            names_to = "type", 
                            values_to = "value") %>%
        dplyr::mutate(type = ifelse(type %in% top_items, type, "other")) %>%
        dplyr::group_by(period, demarcation.code, revenue_group, total, type) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(pct = value / total * 100) %>%                              # % computed AFTER collapsing to other
        dplyr::group_by(revenue_group, type) %>%
        dplyr::summarise(value = mean(pct, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(type = factor(type, items))


composition_by_miif <- ggplot(miif_df, aes(x = revenue_group, y = value, fill = type)) +
                    geom_col() + 
                    scale_y_continuous(limits = c(0, 100), 
                                        expand = c(0, 0)) +
                    scale_x_discrete(expand = c(0, 0)) +
                    scale_fill_manual(values = colour_map, labels = label_map) +
                    labs(title = "Composition of Operational Revenue by MIIF Category,\n2020/21 - 2024/25", 
                            y = "Share (%)", 
                            x = NULL, 
                            caption = "substack.com/@onderstromings\nSource: National Treasury") +
                    substack_theme 

composition_by_miif


revcomp_path <- paste0(output_dir, "/revenue_comp.png")

png(filename = revcomp_path,
        width = 1000,
        height = 650,
        res = 130)

print(composition_by_miif)

dev.off()

####
## Receivables
####


# $aggregates$badi_amount.sum
# $aggregates$badi_amount.sum$label
# [1] "Impairment - Bad Debts i.t.o. Council Policy"

debt_bar_df <- revenue_df %>%
        dplyr::filter(amount_type.code == "AUDA") %>%
        dplyr::select(period, colnames(revenue_df[, 36:54])) %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(across(c(any_of(c(colnames(revenue_df)))), ~sum(.x, na.rm = TRUE))) %>%
        dplyr::arrange(as.numeric(period)) %>%
        dplyr::mutate(outst_pct = g1_amount.sum / (lag(g1_amount.sum) + lag(l1_amount.sum) + lag(l30_amount.sum) + lag(l60_amount.sum) + lag(l90_amount.sum) + lag(l120_amount.sum) + lag(l150_amount.sum) + lag(l180_amount.sum)) * 100) %>%
        tidyr::pivot_longer(-period,
                names_to = "type",
                values_to = "value") %>%
        dplyr::mutate(type = case_when(type %in% c("l30_amount.sum", "l60_amount.sum", "l90_amount.sum", "l120_amount.sum", "l150_amount.sum", "l180_amount.sum") ~ "l180_amount.sum", 
                        TRUE ~ type)) %>%
        dplyr::filter(type %in% c("l180_amount.sum", "l1_amount.sum", "g1_amount.sum", "badi_amount.sum", "bad_amount.sum", "collectability_i", "collectability_a", "outst_pct")) %>%
        dplyr::group_by(period, type) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::filter(period >= 2021)

debt_item_colours <- c("#2E4B3C", 
                "#A3B86C",
                "#5C8A5A",
                "#C9A66B", 
                "#7A6B4F")

debt_item_labels <- c("Less than 6 Months",
            "Less than 1 Year",
            "Greater than 1 year",
            "Debt Impaired", 
            "Debt Written off")


debt_colours <- setNames(debt_item_colours, c("l180_amount.sum", "l1_amount.sum", "g1_amount.sum", "badi_amount.sum", "bad_amount.sum"))

debt_labels <- setNames(debt_item_labels, c("l180_amount.sum", "l1_amount.sum", "g1_amount.sum", "badi_amount.sum", "bad_amount.sum"))

debt_pct_min <- 40
debt_primary_min <- 100e+9

debt_pct_max <- 100
debt_primary_max <- 400e+9

debt_pct_scale  <- (debt_primary_max - debt_primary_min) / (debt_pct_max - debt_pct_min)
debt_pct_offset <- debt_primary_min - debt_pct_scale * debt_pct_min

debt_composition <- ggplot(data = debt_bar_df, aes(x = period, y = value)) +
                    geom_col(data = debt_bar_df %>% dplyr::filter(!stringr::str_detect(type, "collectability")), aes(x = period, y = value, fill = type)) + 
                    geom_line(data = debt_bar_df %>% dplyr::filter(stringr::str_detect(type, "outst")), aes(x = period, y = value * debt_pct_scale), colour = "#5C6B5A") + 
                    geom_text(data = debt_bar_df %>% dplyr::filter(stringr::str_detect(type, "outst")), aes(x = period, y = (value + 2) * debt_pct_scale, label = round(value, digits = 2)), colour = "#5C6B5A") + 
                    scale_y_continuous(labels  = scales::label_number(scale = 1e-9, big.mark = ","), 
                                breaks = scales::pretty_breaks(n = 6),
                                expand = expansion(mult = c(0, 0.1)), 
                                sec.axis = sec_axis(~ (. - debt_pct_offset) / debt_pct_scale, 
                                            name = "% of debt older than a year outstanding at close", 
                                            breaks = seq(40, 100, by = 20),
                                            labels  = function(x)(x))
                                ) +
                    scale_x_continuous(labels = function(x) paste0(x - 1, "/", stringr::str_sub(x, start = 3, end = 4)),
                                        expand = c(0, 0)) +
                    scale_fill_manual(values = debt_colours, labels = debt_labels) +
                    labs(title = "Debt Composition, 2020/21 - 2024/25", 
                            y = "Amount (R Billion)", 
                            x = "Financial Year", 
                            caption = "substack.com/@onderstromings\nSource: National Treasury") +
                    substack_theme +
                    theme(axis.text.x = element_text(), 
                        panel.grid.major.x = element_blank())

debt_composition


debtcomp_path <- paste0(output_dir, "/debt_comp.png")

png(filename = debtcomp_path,
        width = 1000,
        height = 650,
        res = 130)

print(debt_composition)

dev.off()

####
## GeoMap
####


if (TRUE){

# Leaves will bt coloured by Property and Services as % of all revenue relative to its group
#                         1) Municipality Name
#                         2) 2020/21 Total Revenue, Autonomous Amount, Audited vs Original, Debt outstanding** the data is sparse for the latest year
#                         3) 2021/22 OUTCOME, QUALIFICATIONS, FINDINGS
#                         4) 2022/23 OUTCOME, QUALIFICATIONS, FINDINGS
#                         5) 2023/24 OUTCOME, QUALIFICATIONS, FINDINGS
#                         6) 2024/25 OUTCOME, QUALIFICATIONS, FINDINGS

autonomous_df <- revenue_df %>%
                dplyr::left_join(annexures_df %>% dplyr::mutate(period = as.numeric(period)), by = c("demarcation.code", "period")) %>%
                dplyr::filter(amount_type.code == "AUDA") %>%
                dplyr::select(demarcation.code, revenue_group, period, outcomes_audit_opinion, setdiff(colnames(revenue_df[, 4:32]), c("debt_impairment", "irrecoverable_debts_written_off"))) %>%
                dplyr::mutate(flagged = ifelse(stringr::str_detect(tolower(outcomes_audit_opinion), "qualified"), 0, 1)) %>%
                dplyr::filter(flagged == 0) %>%
                dplyr::group_by(demarcation.code, revenue_group, period, flagged) %>%
                dplyr::summarise(across(c(any_of(c(colnames(revenue_df)))), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
                dplyr::mutate(total_revenue = rowSums(dplyr::across(4:30), na.rm = TRUE)) %>%
                dplyr::mutate(autonomous_revenue = ((total_revenue - transfer_and_subsidies_operational) / total_revenue) * 100) %>%
                dplyr::group_by(revenue_group, period) %>%
                dplyr::mutate(autonomous_score = scale(autonomous_revenue)) %>%
                dplyr::select(demarcation.code, autonomous_revenue, autonomous_score)

projections_df <-  revenue_df %>%
                dplyr::filter(demarcation.code != "FS185") %>%
                dplyr::select(demarcation.code, period, amount_type.code, setdiff(colnames(revenue_df[, 4:32]), c("debt_impairment", "irrecoverable_debts_written_off"))) %>%
                dplyr::group_by(demarcation.code, period, amount_type.code) %>%
                dplyr::summarise(across(c(any_of(c(colnames(revenue_df)))), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
                dplyr::mutate(total_revenue = rowSums(dplyr::across(4:30), na.rm = TRUE)) %>%
                dplyr::select(demarcation.code, period, amount_type.code, total_revenue) %>%
                dplyr::group_by(period, demarcation.code) %>%
                tidyr::pivot_wider(id_cols = c(demarcation.code, period), 
                                    names_from = amount_type.code, 
                                    values_from = total_revenue) %>%
                dplyr::mutate(projection_acc = AUDA / ORGB) 

collection_df <- revenue_df %>%
                dplyr::filter(amount_type.code == "AUDA") %>%
                dplyr::select(1, 2, 36:46) %>%
                dplyr::ungroup() %>%
                dplyr::group_by(demarcation.code) %>%
                dplyr::arrange(as.numeric(period)) %>%
                dplyr::mutate(outst_pct = g1_amount.sum / (lag(g1_amount.sum) + lag(l1_amount.sum) + lag(l30_amount.sum) + lag(l60_amount.sum) + lag(l90_amount.sum) + lag(l120_amount.sum) + lag(l150_amount.sum) + lag(l180_amount.sum)) * 100) %>%
                dplyr::select(period, demarcation.code, outst_pct, total_amount.sum)


munic_df_map <- munic_list %>%    
            dplyr::mutate(revenue_group = case_when(municipality.miif_category == "A" ~ "A",
                                        municipality.miif_category == "B1" ~ "B1",
                                        municipality.miif_category == "B2" & district.miif_category == "C1" ~ "B2/C1",
                                        municipality.miif_category == "B2" & district.miif_category == "C2" ~ "B2/C2",
                                        municipality.miif_category == "B3" & district.miif_category == "C1" ~ "B3/C1",
                                        municipality.miif_category == "B3" & district.miif_category == "C2" ~ "B3/C2",
                                        municipality.miif_category == "B4" & district.miif_category == "C1" ~ "B4/C1",
                                        municipality.miif_category == "B4" & district.miif_category == "C2" ~ "B4/C2"))

map_df_data <- munic_df_map %>%
                dplyr::select(demarcation.code, demarcation.label, revenue_group) %>%
                tidyr::expand_grid(period = 2021:2025) %>%
                dplyr::left_join(projections_df, by = c("demarcation.code", "period")) %>%
                dplyr::left_join(collection_df, by = c("demarcation.code", "period")) %>%
                dplyr::left_join(annexures_df %>% dplyr::mutate(period = as.numeric(period)), 
                                    by = c("demarcation.code", "period")) %>%
                dplyr::left_join(autonomous_df %>% dplyr::select(-revenue_group), by = c("demarcation.code", "period")) %>%
                dplyr::group_by(demarcation.code) %>%
                dplyr::arrange(period, .by_group = TRUE) %>%
                dplyr::mutate(
                  base_value = AUDA[period == 2021],
                  n_years = period - 2021,
                  cagr = ifelse(n_years > 0, (AUDA / base_value)^(1 / n_years) - 1, NA_real_),
                base_value_aut = autonomous_revenue[period == 2021],
                  n_years = period - 2021,
                  cagr_aut = ifelse(n_years > 0, (AUDA / base_value)^(1 / n_years) - 1, NA_real_)) %>%
                dplyr::ungroup() %>%
                dplyr::filter(period == 2025) %>%
                dplyr::mutate(flagged = ifelse(stringr::str_detect(tolower(outcomes_audit_opinion), "qualified"), 0, 1)) 

map_resp <- httr::GET("https://services7.arcgis.com/oeoyTUJC8HEeYsRB/arcgis/rest/services/LocalMunicipalities2018_Final/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")

writeLines(httr::content(map_resp, "text", encoding = "UTF-8"), here::here("data", "munic_map.geojson"))

map <- sf::st_read(here::here("data", "munic_map.geojson")) %>%
                dplyr::select(CAT_B, DATE, Shape__Area, Shape__Length, geometry) %>%
                dplyr::rename(demarcation.code = CAT_B)

map_df <- left_join(map_df_data, map,  by = "demarcation.code") %>%
        dplyr::mutate(autonomous_score = ifelse(flagged == 0, autonomous_score, NA_real_))

pal <- colorNumeric(palette = c("#F7F5EF", "#DCE9C4", "#B8C99A", "#8AA870", "#5C8A5A", "#2E4B3C", "#1A2E22"),
                domain = map_df$autonomous_score[map_df$flagged != 1], 
                na.color = "#9E9E9E")

i_char <- as.character(2025)

year <- paste0("2024/25")

leaf_info[[i_char]] <- ifelse(!is.na(map_df[["AUDA"]]) & map_df[["AUDA"]] > 0,
            paste0(
"&nbsp;&nbsp;&nbsp;Total Operational Revenue (% Change since 2020/21): R", scales::label_number(big.mark = " ")(map_df[["AUDA"]]), " (", round(map_df[["cagr"]] * 100, digits = 2), "%)", "<br>",
"&nbsp;&nbsp;&nbsp;Autonomous Revenue : ", 
  ifelse(map_df[["flagged"]] == 0, 
         paste0("R", scales::label_number(big.mark = " ")(map_df[["AUDA"]] * map_df[["autonomous_revenue"]] / 100), 
                " (", round(map_df[["autonomous_revenue"]], digits = 2), "%)"), 
         "NA"), 
"<br>",
"&nbsp;&nbsp;&nbsp;Audited vs Original: ", round(map_df[["projection_acc"]] * 100, digits = 2), "%", "<br>",
"&nbsp;&nbsp;&nbsp;Operational Debt Outstanding (% Older than 1 year): ", scales::label_number(big.mark = " ")(map_df[["total_amount.sum"]]), 
  ifelse(is.finite(map_df[["outst_pct"]]), paste0(" (", round(map_df[["outst_pct"]], digits = 2), "%)"), " (NA)"), 
"<br>"),
        paste0(
"&nbsp;&nbsp;&nbsp;Total Operational Revenue (% Change since 2020/21): ", scales::label_number(big.mark = " ")(map_df[["AUDA"]]), " (", round(map_df[["cagr"]] * 100, digits = 2), "%)", "<br>",
"&nbsp;&nbsp;&nbsp;Autonomous Revenue: NA", "<br>",
"&nbsp;&nbsp;&nbsp;Projection Accuracy: NA", "<br>",
"&nbsp;&nbsp;&nbsp;Operational Debt Outstanding (% Older than 1 year): NA", "<br>"))

combined_labels <- leaf_info[["2025"]]

interact_map <- leaflet(
                map_df %>%
                st_as_sf()
                ) %>%
        addPolygons(
                fillColor = ~pal(autonomous_score),
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
  "<b>", ifelse(flagged == 0, demarcation.label, paste0(demarcation.label, "*")), " (", revenue_group.x, ")", "</b><br>", combined_labels)
  , htmltools::HTML),
        labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "6px 8px"),
        textsize = "13px",
        direction = "auto"
        )
        ) %>%
        addLegend(
                 pal = pal,
                values = ~autonomous_score,
                title = "2024/25 Autonomous Revenue Score",
                position = "bottomright",
                na.label = ) %>%
        htmlwidgets::onRender("
    function(el, x) {
      this.getContainer().style.background = '#EDE8DA';
    }
  ")

interact_map

filepath_html <- paste0(output_dir, "/revenue_map.html")
htmlwidgets::saveWidget(interact_map, file = filepath_html, selfcontained = TRUE)

datawrapper_table <- map_df_data %>% 
        dplyr::ungroup() %>%
        dplyr::left_join(munic_list %>% dplyr::select(demarcation.code, municipality.province_code), by = "demarcation.code") %>%
        dplyr::select(demarcation.label, municipality.province_code, AUDA, projection_acc, outst_pct, autonomous_revenue, autonomous_score, cagr) %>%
        dplyr::mutate(across(c(projection_acc, cagr), 
        ~.x * 100, .names = "{.col}"), 
        autonomous_revenue_r = AUDA * (autonomous_revenue * 100)) %>%
        dplyr::rename(Municipality = demarcation.label, 
                        Province = municipality.province_code, 
                        `Total Operational Revenue (R)` = AUDA,   
                        `Yearly Growth` = cagr,
                        `Audited vs Original (%)` = `projection_acc`, 
                        `Aged Debt (%)` = outst_pct,
                        `Autonomous Revenue (R)` = autonomous_revenue_r,
                        `Autonomous Revenue (%)` = autonomous_revenue,
                        `Autonomous Revenue (Score)` = autonomous_score) 

csvpath <- paste0(output_dir, "/revenue.csv")
utils::write.csv(datawrapper_table, csvpath, row.names = FALSE)

}

