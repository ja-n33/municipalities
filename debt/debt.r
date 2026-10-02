pacman::p_load(dplyr, httr, jsonlite, ggplot2, sf, leaflet, scales, cowplot, magick, patchwork)

output_dir <- here::here("debt", "output")

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
        dplyr::filter(as.numeric(item.code) <= 1900) %>%
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
        dplyr::group_by(demarcation.code, period, amount_type.code, item.code) %>%
        dplyr::summarise(
                        dplyr::across(c(dplyr::any_of(c("total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")), dplyr::matches("^l")),
                         ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(g90_amount.sum = total_amount.sum - (l30_amount.sum + l60_amount.sum + l90_amount.sum), 
                        l1y_amount.sum = total_amount.sum - g1_amount.sum, 
                        dplyr::across(dplyr::matches("^l|^g"), ~.x / total_amount.sum * 100, .names = "{stringr::str_remove(.col, 'amount.sum')}pct"), 
                        ) %>%
        tidyr::pivot_wider(id_cols = c(demarcation.code, period, amount_type.code), 
                        names_from = item.code, 
                        values_from = 5:dplyr::last_col()) 


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
    dplyr::distinct(demarcation.code, period, .keep_all = TRUE) 


####
## Get Balance Sheet Data
####

drilldown_sofp <- paste(
        "amount_type.label", "amount_type.code",
        "financial_year_end.year",
        "period_length.length",
        "item.code",
        "demarcation.code",
        sep = "|"
)

itemcodes_sofp <- c("0130", "0140", "0150", "0280", "0290")
itemcodes_sofp_vect <- paste(itemcodes_sofp, collapse = '";"')

fetch_data_sofp <- function(year, 
                        url = "https://municipaldata.treasury.gov.za/api/cubes/financial_position_v2/aggregate", 
                        drilldown = drilldown_sofp,
                        pagesize = 5000, 
                        page = 1, 
                        itemcode = itemcodes_sofp_vect, 
                        amount_type = "AUDA",
                        aggregate = "amount.sum"){

  base_url <- url

  
  cut <- sprintf('amount_type.code:"%s"|financial_year_end.year:%d|item.code:"%s"|period_length.length:%s', amount_type, year, itemcodes_sofp_vect, "year")

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

nt_data_sofp <- list()

for (yr in c(2020:2025)){
                sofp_url <- "https://municipaldata.treasury.gov.za/api/cubes/financial_position_v2/aggregate"

                year_df <- fetch_data_sofp(year = yr, url = sofp_url)

                nt_data_sofp[[as.character(yr)]] <- year_df

                message(paste0(yr, " completed"))
                }

sofp <- bind_rows(nt_data_sofp)

sofp <- sofp %>%
        dplyr::rename(period = financial_year_end.year) %>%
        dplyr::group_by(demarcation.code, period) %>%
        dplyr::summarise(open_debt = sum(amount.sum, na.rm = TRUE))


####
## Join Datasets
###

revenue_df <- dplyr::left_join(dplyr::left_join(left_join(oprev_df, munic_list %>% dplyr::select(demarcation.code, demarcation.label, municipality.miif_category, district.miif_category, hh_n), by = "demarcation.code"), debt_merge, by = c("period", "demarcation.code", "amount_type.code")), sofp, by = c("demarcation.code", "period")) %>%
                    dplyr::filter(!(stringr::str_detect(stringr::str_sub(demarcation.code, 1, 2), "DC"))) %>%
                    dplyr::mutate(revenue_group = case_when(municipality.miif_category == "A" ~ "A",
                                        municipality.miif_category == "B1" ~ "B1",
                                        municipality.miif_category == "B2" & district.miif_category == "C1" ~ "B2/C1",
                                        municipality.miif_category == "B2" & district.miif_category == "C2" ~ "B2/C2",
                                        municipality.miif_category == "B3" & district.miif_category == "C1" ~ "B3/C1",
                                        municipality.miif_category == "B3" & district.miif_category == "C2" ~ "B3/C2",
                                        municipality.miif_category == "B4" & district.miif_category == "C1" ~ "B4/C1",
                                        municipality.miif_category == "B4" & district.miif_category == "C2" ~ "B4/C2"))
   
####################################################################################################################################################################################

                                                        ## MUNICIPAL FINANCIAL DATA ##

####################################################################################################################################################################################


####
## Debt distribution cont'd
####


debt_totals <- debt %>%
        dplyr::select(dplyr::any_of(c("customer_group.code", "financial_period.period", "item.code", "demarcation.code", "total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")),
                        dplyr::matches("^l"), amount_type.code) %>%
        dplyr::filter(amount_type.code == "AUDA") %>%
        dplyr::rename(period = financial_period.period) %>%
        dplyr::group_by(demarcation.code, period) %>%
        dplyr::summarise(
                        dplyr::across(c(dplyr::any_of(c("total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")), dplyr::matches("^l")),
                         ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(across(c("g1_amount.sum", dplyr::matches("^l")), ~.x / total_amount.sum * 100, .names = "{stringr::str_remove(.col, 'amount.sum')}pct"))

debt_distribution_df_i <- debt_totals %>% 
        dplyr::left_join(debt_totals %>%
                        dplyr::transmute(demarcation.code, 
                                        period = period + 1, 
                                        prev_book = g1_amount.sum + l1_amount.sum + l30_amount.sum + l60_amount.sum +
                                   l90_amount.sum + l120_amount.sum + l150_amount.sum + l180_amount.sum), 
                                   by = c("demarcation.code", "period")) %>% 
        dplyr::mutate(outst_pct = g1_amount.sum / prev_book) %>%
        tidyr::pivot_longer(!c(period, demarcation.code),
                names_to = "type",
                values_to = "value") %>%
        dplyr::mutate(type = case_when(type %in% c("l30_pct", "l60_pct", "l90_pct", "l120_pct", "l150_pct", "l180_pct") ~ "l180_pct", 
                        TRUE ~ type)) %>%
        dplyr::filter(type %in% c("l180_pct", "l1_pct", "g1_pct", "badi_pct", "bad_pct", "collectability_i", "collectability_a", "outst_pct")) %>%
        dplyr::filter(period >= 2021 & type == "outst_pct" & is.finite(value)) %>%
        dplyr::mutate(value_raw_i = value, 
                        value = ifelse(value > 1, 1, value),
                         decile = floor(value / 0.1), 
                         decile = ifelse(decile == -2, -1, decile))



count_df <- debt_distribution_df_i %>%
        dplyr::group_by(period, decile) %>%
        dplyr::summarise(n = dplyr::n()) 

total_df <- count_df %>% 
        dplyr::group_by(period) %>%
        dplyr::summarise(n_total = sum(n, na.rm = TRUE)) 

count_df <- dplyr::left_join(count_df, total_df, by = "period")  %>%
        dplyr::mutate(n_pct = n / n_total) 

aged_debt_df <- debt_distribution_df_i %>% 
                dplyr::filter(value_raw_i < 10 & value_raw_i > 0) %>%
                dplyr::group_by(period) %>%
                dplyr::summarise(raw_mean = mean(value_raw_i, na.rm = TRUE),
                                raw_median = median(value_raw_i, na.rm = TRUE)) %>%
                dplyr::full_join(count_df, by = "period")

bins <- sort(unique(aged_debt_df$decile))

print(bins)
label_map_aged_debt <- setNames(c(" ", "0% - 10%", "11% - 20%", "21% - 30%", "31% - 40%", "41% - 50%", "51% - 60%", "61% - 70%", "71% - 80%", "81% - 90%", "91% - 100%", "> 100%"), bins)

names(label_map_aged_debt)

aged_debt <- list()

for (year in (2022:2025)){

        plot_data <- aged_debt_df %>%
                dplyr::filter(period == year) 

        fyear <- paste0((year - 1), "/", stringr::str_sub(year, -2, -1))

        mean_x <- round(plot_data$raw_mean[1], digits = 3) * 10

        median_x <- round(plot_data$raw_median[1], digits = 3) * 10

        nobs <- paste0(nrow(debt_distribution_df_i %>% dplyr::filter(period == year & !is.na(value_raw_i))), "/", 213)

        debthist_temp <- ggplot(data = plot_data %>% dplyr::filter(decile >= 0), aes(x = decile, y = n_pct)) +
                geom_col(fill = "#5C8A5A") +
                geom_vline(xintercept  = pmin(mean_x, 10), 
                                linetype = "solid", 
                                linewidth = 0.75) + 
                geom_vline(xintercept  = pmin(median_x, 10), 
                                linetype = "dashed", 
                                linewidth = 0.75) +
                scale_x_continuous(breaks = 0:10,
                                labels = function(b) ifelse((b + 1) %% 2 == 1, make_bin_labels(b), ""), 
                                expand = c(0.01, 0.01)) +
                scale_y_continuous(limits = c(0, 0.30), 
                        minor_breaks = scales::breaks_pretty(n = 6),
                        labels  = function(x) ifelse(x == 0, "", x * 100),
                        expand = expansion(mult = c(0, 0))) +
                labs(y = "Percentage of Municipalities",
                        x = "Percentage of debt outstanding for over 1 year", 
                        title = paste0(fyear, " (", nobs, ")")) +
                coord_cartesian(xlim = c(-0.5, 10.5), ylim = c(0, max_y)) +
                annotate("text", 
                                x = c(ifelse(mean_x < 10,mean_x, 10), ifelse(median_x < 10, median_x, 10)), 
                                y = 0.25, 
                                label = c(paste0("Mean: ", ifelse(mean_x < 10, round(mean_x * 10, digits = 2), "> 100"), "%"), 
                                        paste0("Median: ", ifelse(median_x < 10, round(median_x * 10, digits = 2), "> 100"), "%")), 
                                vjust = c(-1, 1), 
                                hjust = c(ifelse(mean_x < 10, -0.1, 1.1), 1.1), 
                                size = 3) +
                substack_theme +
                theme( panel.grid.minor = element_blank(), 
                        plot.title.position = "panel", 
                        plot.title = element_text(colour = "#1F2A24", size = rel(1), hjust = 0.5, face = "plain"),
                        panel.border = element_blank()) 

        aged_debt[[as.character(year)]] <- debthist_temp
}

combined_aged_debt <- patchwork::wrap_plots(aged_debt, nrow = length(aged_debt)) +
                patchwork::plot_layout(axis_titles = "collect", guides = "collect", axes = "collect") +
                patchwork::plot_annotation(title = "Percentage of debt unresolved from previous\n year by Municipality, 2021/22 - 2024/25",
                                caption = "substack.com/@onderstromings\nSource: National Treasury\nNote: Municipalities without data excluded\n          Mean values are represented by solid lines, dashed lines indicate median values.", 
                                theme = substack_theme + theme(axis.title = element_text(face = "plain")))

debt_plot <- cowplot::ggdraw(combined_aged_debt) +   
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
debt_plot

debtcomp_path <- paste0(output_dir, "/debt_comp.png")

png(filename = debtcomp_path,
        width = 650,
        height = 1100,
        res = 130)

print(debt_plot)

dev.off()

revenue_source_df <- tibble::tibble(code = c(as.character(seq(1200, 1600, by = 100))), 
                                title = c("Water", "Electricity", "Property Rates", "Waste Water Management", "Waste Management"))

make_bin_labels <- function(bins, width = 0.1, cap = 1) {
  top <- floor(cap / width)          # the bin that capped values fall into (10)
  dplyr::case_when(
    bins < 0    ~ "< 0%",
    bins >= top ~ paste0("\u2265 ", cap * 100, "%"),
    TRUE        ~ paste0(bins * width * 100, "% - ", (bins + 1) * width * 100, "%")
  )
}

make_bin_labels(-1:10)

debt <- dplyr::left_join(debt, revenue_df %>% dplyr::distinct(demarcation.code, revenue_group), by = "demarcation.code" )

years_outst <- list()

for (i in 1:nrow(revenue_source_df)){

        itemcode <- revenue_source_df$code[i]

        title_id <- revenue_source_df$title[i]
        
        file_id <- gsub(" ", "_", tolower(title_id))

        plot_title <- paste0("Percentage of ", title_id, " debt unresolved\nfrom previous year by Municipality, 2021/22 - 2024/25")

        if (stringr::str_detect(file_id, "water")){
                debt_totals <- debt %>%
                        dplyr::select(dplyr::any_of(c("customer_group.code", "financial_period.period", "item.code", "demarcation.code", "total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")),
                                        dplyr::matches("^l"), amount_type.code, revenue_group) %>%
                        dplyr::filter(amount_type.code == "AUDA" & 
                                        item.code == itemcode &
                                        !stringr::str_detect(stringr::str_sub(demarcation.code, start = 1, end = 2), "DC") &
                                        !stringr::str_detect(revenue_group, "C2")) %>%
                        dplyr::rename(period = financial_period.period) %>%
                        dplyr::group_by(demarcation.code, period) %>%
                        dplyr::summarise(
                                        dplyr::across(c(dplyr::any_of(c("total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")), dplyr::matches("^l")),
                                        ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
                        dplyr::mutate(book = g1_amount.sum + l1_amount.sum + l30_amount.sum + l60_amount.sum +
                        l90_amount.sum + l120_amount.sum + l150_amount.sum + l180_amount.sum) %>%
                        dplyr::filter(book > 0) %>%
                        dplyr::mutate(across(c("g1_amount.sum", dplyr::matches("^l")), ~.x / total_amount.sum * 100, .names = "{stringr::str_remove(.col, 'amount.sum')}pct")) %>%
                        dplyr::filter(total_amount.sum >= 0)

                        nobs_raw <- nrow(revenue_df %>% dplyr::distinct(demarcation.code, revenue_group) %>% dplyr::filter(!stringr::str_detect(revenue_group, "C2")))

                } else{
                debt_totals <- debt %>%
                        dplyr::select(dplyr::any_of(c("customer_group.code", "financial_period.period", "item.code", "demarcation.code", "total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")),
                                        dplyr::matches("^l"), amount_type.code, revenue_group) %>%
                        dplyr::filter(amount_type.code == "AUDA" & 
                                        item.code == itemcode &
                                        !stringr::str_detect(stringr::str_sub(demarcation.code, start = 1, end = 2), "DC")) %>%
                        dplyr::rename(period = financial_period.period) %>%
                        dplyr::group_by(demarcation.code, period) %>%
                        dplyr::summarise(
                                        dplyr::across(c(dplyr::any_of(c("total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")), dplyr::matches("^l")),
                                        ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
                        dplyr::mutate(book = g1_amount.sum + l1_amount.sum + l30_amount.sum + l60_amount.sum +
                        l90_amount.sum + l120_amount.sum + l150_amount.sum + l180_amount.sum) %>%
                        dplyr::filter(book > 0) %>%
                        dplyr::mutate(across(c("g1_amount.sum", dplyr::matches("^l")), ~.x / total_amount.sum * 100, .names = "{stringr::str_remove(.col, 'amount.sum')}pct")) %>%
                        dplyr::filter(total_amount.sum >= 0)

                        nobs_raw <- nrow(revenue_df %>% dplyr::distinct(demarcation.code, revenue_group))
                }

        debt_distribution_df <- debt_totals %>% 
                dplyr::left_join(debt_totals %>%
                                dplyr::transmute(demarcation.code, 
                                                period = period + 1, 
                                                prev_book = book), 
                                        by = c("demarcation.code", "period")) %>% 
                dplyr::mutate(outst_pct = g1_amount.sum / prev_book) %>%
                tidyr::pivot_longer(!c(period, demarcation.code),
                        names_to = "type",
                        values_to = "value") %>%
                dplyr::mutate(type = case_when(type %in% c("l30_pct", "l60_pct", "l90_pct", "l120_pct", "l150_pct", "l180_pct") ~ "l180_pct", 
                                TRUE ~ type)) %>%
                dplyr::filter(type %in% c("l180_pct", "l1_pct", "g1_pct", "badi_pct", "bad_pct", "collectability_i", "collectability_a", "outst_pct")) %>%
                dplyr::filter(period >= 2021 & type == "outst_pct" & is.finite(value)) %>%
                dplyr::mutate(value_raw = value, 
                                value = ifelse(value > 1, 1, value),
                                decile = pmax(floor(value / 0.1), -1))

        count_df <- debt_distribution_df %>%
                dplyr::group_by(period, decile) %>%
                dplyr::summarise(n = dplyr::n()) 

        total_df <- count_df %>% 
                dplyr::group_by(period) %>%
                dplyr::summarise(n_total = sum(n, na.rm = TRUE)) 

        count_df <- dplyr::left_join(count_df, total_df, by = "period")  %>%
                dplyr::mutate(n_pct = n / n_total) 

        aged_debt_df <- debt_distribution_df %>% 
                        dplyr::filter(value_raw < 10 & value_raw > 0) %>%
                        dplyr::group_by(period) %>%
                        dplyr::summarise(raw_mean = mean(value_raw, na.rm = TRUE),
                                        raw_median = median(value_raw, na.rm = TRUE)) %>%
                        dplyr::full_join(count_df, by = "period")

        bins <- sort(unique(aged_debt_df$decile))
        
        aged_debt <- list()

        max_y = max(aged_debt_df$n_pct)

        years_outst[[as.character(itemcode)]] <- debt_distribution_df %>% 
                                                dplyr::filter(value_raw < 10 & value_raw > 0) %>%
                                                dplyr::mutate(itemcode = itemcode)

        for (year in (2022:2025)){

                plot_data <- aged_debt_df %>%
                        dplyr::filter(period == year) 

                fyear <- paste0((year - 1), "/", stringr::str_sub(year, -2, -1))

                mean_x <- round(mean(plot_data$raw_mean), digits = 3) * 10

                median_x <- round(mean(plot_data$raw_median), digits = 3) * 10

                nobs <- paste0(plot_data$n_total[1], "/", nobs_raw)

                debthist_temp <- ggplot(data = plot_data %>% dplyr::filter(decile >= 0), aes(x = decile, y = n_pct)) +
                        geom_col(fill = "#5C8A5A") +
                        geom_vline(xintercept  = pmin(mean_x, 10), 
                                linetype = "solid", 
                                linewidth = 0.75) + 
                        geom_vline(xintercept  = pmin(median_x, 10), 
                                linetype = "dashed", 
                                linewidth = 0.75) +
                        scale_x_continuous(breaks = 0:10,
                                           labels = function(b) ifelse((b + 1) %% 2 == 1, make_bin_labels(b), ""), 
                                           expand = c(0.01, 0.01)) +
                        scale_y_continuous(limits = c(0, max_y), 
                                minor_breaks = scales::breaks_pretty(n = 6),
                                labels  = function(x) ifelse(x == 0, "", x * 100),
                                expand = expansion(mult = c(0, 0.075))) +
                        labs(y = "Percentage of Municipalities",
                                x = "Percentage of debt outstanding for over 1 year", 
                                title = paste0(fyear, " (", nobs, ")")) +
                        annotate("text", 
                                x = c(ifelse(mean_x < 10,mean_x, 10), ifelse(median_x < 10, median_x, 10)), 
                                y = 0.25, 
                                label = c(paste0("Mean: ", ifelse(mean_x < 10, round(mean_x * 10, digits = 2), "> 100"), "%"), 
                                        paste0("Median: ", ifelse(median_x < 10, round(median_x * 10, digits = 2), "> 100"), "%")), 
                                vjust = c(-1, 1), 
                                hjust = c(ifelse(mean_x < 10, -0.1, 1.1), 1.1), 
                                size = 3) +
                        coord_cartesian(xlim = c(-0.5, 10.5), ylim = c(0, max_y)) +
                        substack_theme +
                        theme(panel.grid.major.y = element_blank(), 
                                panel.grid.minor = element_blank(), 
                                plot.title.position = "panel", 
                                plot.title = element_text(colour = "#1F2A24", size = rel(1), hjust = 0.5, face = "plain"),
                                panel.border = element_blank()) 

                aged_debt[[as.character(year)]] <- debthist_temp
        }

        
        combined_aged_debt <- patchwork::wrap_plots(aged_debt, nrow = length(aged_debt)) +
                        patchwork::plot_layout(axis_titles = "collect", guides = "collect", axes = "collect") +
                        patchwork::plot_annotation(title = plot_title,
                                        caption = "substack.com/@onderstromings\nSource: National Treasury\nNote: Municipalities without data excluded.\n          Mean values are represented by solid lines, dashed lines indicate median values.", 
                                        theme = substack_theme + theme(axis.title = element_text(face = "plain")))

        debt_plot <- cowplot::ggdraw(combined_aged_debt) +   
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


        debtcomp_path <- paste0(output_dir, "/", file_id, "_debt_comp.png")

        png(filename = debtcomp_path,
                width = 650,
                height = 1100,
                res = 130)

        print(debt_plot)

        dev.off()

}

k <- years_outst[["1200"]]
####
## G90 debt over revenue - total, 
####


cols <- colnames(revenue_df)
opdebt_cols <- stringr::str_detect(cols, "^g90_amount") & !stringr::str_detect(cols, "_1900$")

lineplot_init_df <- revenue_df %>%
                dplyr::mutate(oprev = rowSums(revenue_df[,  4:28], na.rm = TRUE), 
                                opdebt = rowSums(revenue_df[, opdebt_cols], na.rm = TRUE), 
                                total_ratio = opdebt / oprev, 
                                total_ratio = ifelse(total_ratio < 10, total_ratio, NA_integer_),
                                electricity_ratio = ifelse(service_charges_electricity >= 10000, g90_amount.sum_1300 / service_charges_electricity, NA_integer_ ), 
                                water_ratio = ifelse(service_charges_water >= 10000, g90_amount.sum_1200 / service_charges_water, NA_integer_ ), 
                                waterwaste_ratio = ifelse(service_charges_waste_water_management >= 10000,g90_amount.sum_1500 / service_charges_waste_water_management, NA_integer_ ),
                                waste_ratio = ifelse(service_charges_waste_management >= 10000, g90_amount.sum_1600 / service_charges_waste_management, NA_integer_ )) %>%
                dplyr::filter(amount_type.code == "AUDA") %>%
                dplyr::select(demarcation.code, period, dplyr::matches("|service_charges_|_ratio|1200|1300|1500|1600|oprev|opdebt"), revenue_group) 

electricity_waste <- lineplot_init_df %>%
                dplyr::group_by(period) %>%
                dplyr::summarise(across(dplyr::matches("ratio"), ~ mean(.x, na.rm = TRUE))) %>%
                dplyr::mutate(dplyr::across(dplyr::matches("ratio"), ~ (.x / .x[period == 2021]) * 100, .names = "{sub('_ratio$','_index', .col)}")) %>%
                dplyr::select(period, total_index, electricity_index, waste_index)

water_df <- lineplot_init_df %>%
                dplyr::filter(!stringr::str_detect(revenue_group, "C2")) %>%
                dplyr::group_by(period) %>%
                dplyr::summarise(across(dplyr::matches("ratio"), ~ mean(.x, na.rm = TRUE))) %>%
                dplyr::mutate(dplyr::across(dplyr::matches("ratio"), ~ (.x / .x[period == 2021]) * 100, .names = "{sub('_ratio$','_index', .col)}")) %>%
                dplyr::select(period, water_index, waterwaste_index)

lineplot_df <- left_join(electricity_waste, water_df, by = "period") %>%
                tidyr::pivot_longer(-period, 
                                        names_to = "type", 
                                        values_to = "value") %>%
                dplyr::filter(period >= 2021) %>%
                dplyr::mutate(type = forcats::fct_reorder2(type, period, value))
                
colour_map_services <- c(
  total_index       = "#2E4B3C",
  electricity_index = "#A3B86C",
  waste_index       = "#5C8A5A",
  water_index       = "#C9A66B",
  waterwaste_index  = "#7A6B4F"
)

label_map_services <- c(
  total_index       = "Overall",
  electricity_index = "Electricity",
  waste_index     = "Waste",
  water_index       = "Water",
  waterwaste_index  = "Water Waste"
)

lineplot <- ggplot(data = lineplot_df, mapping = aes(x = period, y = value, colour = type)) +
        geom_line(data = lineplot_df %>% dplyr::filter(type == "total_ratio"),
                 linewidth = 2.25) +
        geom_line(data = lineplot_df %>% dplyr::filter(type != "total_ratio"),
                 linewidth = 1.5) + 
        scale_colour_manual(values = colour_map_services, labels = label_map_services) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 6),
                expand = expansion(mult = c(0.001, 0.05))) +
        scale_x_continuous(labels = function(x) paste0(x-1, "/", stringr::str_sub(x, start = 3, end = 4))) +
        labs(title    = "Aged Debt to Revenue Index,  2020/21 - 2024/25",
        x        = "Financial Year",
        y = "Aged Debt / Revenue Index (2020/21 = 100)",
        caption = "substack.com/@onderstromings\n \nSource: National Treasury\nNote: Aged debt equals the value of receivables outstanding for over 90 days. ") +
        guides(colour = guide_legend(title = NULL, order = 1)) +
        substack_theme + 
        theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "right", 
        legend.direction = "vertical")

lineplot

line_path <- paste0(output_dir, "/service_lines.png")

png(filename = line_path,
        width = 1050,
        height = 700,
        res = 130)
print(lineplot)
dev.off()


electricity_waste_med <- lineplot_init_df %>%
                dplyr::group_by(period) %>%
                dplyr::summarise(across(dplyr::matches("ratio"), ~ median(.x, na.rm = TRUE))) %>%
                dplyr::mutate(dplyr::across(dplyr::matches("ratio"), ~ (.x / .x[period == 2021]) * 100, .names = "{sub('_ratio$','_index', .col)}")) %>%
                dplyr::select(period, total_index, electricity_index, waste_index)

water_df_med <- lineplot_init_df %>%
                dplyr::filter(!stringr::str_detect(revenue_group, "C2")) %>%
                dplyr::group_by(period) %>%
                dplyr::summarise(across(dplyr::matches("ratio"), ~ median(.x, na.rm = TRUE))) %>%
                dplyr::mutate(dplyr::across(dplyr::matches("ratio"), ~ (.x / .x[period == 2021]) * 100, .names = "{sub('_ratio$','_index', .col)}")) %>%
                dplyr::select(period, water_index, waterwaste_index)

lineplot_df_med <- left_join(electricity_waste_med, water_df_med, by = "period") %>%
                tidyr::pivot_longer(-period, 
                                        names_to = "type", 
                                        values_to = "value") %>%
                dplyr::filter(period >= 2021) %>%
                dplyr::mutate(type = forcats::fct_reorder2(type, period, value))


lineplot_med <- ggplot(data = lineplot_df_med, mapping = aes(x = period, y = value, colour = type)) +
        geom_line(data = lineplot_df %>% dplyr::filter(type == "total_ratio"),
                 linewidth = 2.25) +
        geom_line(data = lineplot_df %>% dplyr::filter(type != "total_ratio"),
                 linewidth = 1.5) + 
        scale_colour_manual(values = colour_map_services, labels = label_map_services) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 6),
                expand = expansion(mult = c(0.001, 0.05))) +
        scale_x_continuous(labels = function(x) paste0(x-1, "/", stringr::str_sub(x, start = 3, end = 4))) +
        labs(title    = "Aged Debt to Revenue Index (Medians),  2020/21 - 2024/25",
        x        = "Financial Year",
        y = "Aged Debt / Revenue Index (2020/21 = 100)",
        caption = "substack.com/@onderstromings\n \nSource: National Treasury\nNote: Aged debt equals the value of receivables outstanding for over 90 days.") +
        guides(colour = guide_legend(title = NULL, order = 1)) +
        substack_theme + 
        theme(panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "right", 
        legend.direction = "vertical")


lineplot_med


line_med_path <- paste0(output_dir, "/service_lines_med.png")

png(filename = line_med_path,
        width = 1050,
        height = 700,
        res = 130)
print(lineplot_med)
dev.off()


####
## Outstanding vs written off scatter 
####

scatter_init <- revenue_df %>%
        dplyr::mutate(
                bad_amount.sum_total = rowSums(revenue_df[, stringr::str_detect(colnames(revenue_df), "^bad_amount")], na.rm = TRUE),
                outstanding_total = rowSums(revenue_df[, stringr::str_detect(colnames(revenue_df), "^total_amount")], na.rm = TRUE), 
                g90_amount.sum_total = rowSums(revenue_df[, stringr::str_detect(colnames(revenue_df), "^g90_amount")], na.rm = TRUE)) %>%
        dplyr::filter(amount_type.code == "AUDA")

xvals <- scatter_init %>%
        dplyr::group_by(demarcation.code) %>%
        dplyr::summarise(debt_outstanding = dplyr::first(g90_amount.sum_total[period == 2022]),
                        .groups = "drop") %>%
        dplyr::filter(!is.na(debt_outstanding) & debt_outstanding > 10000 )

yvals <- scatter_init %>%
        dplyr::group_by(demarcation.code) %>%
        dplyr::mutate(irrecoverable_debts_written_off = pmax(irrecoverable_debts_written_off, 0)) %>%
        dplyr::filter(period >= 2023) %>%
        dplyr::summarise(debt_writtenoff = sum(irrecoverable_debts_written_off, na.rm = TRUE)) %>%
        dplyr::filter(!is.na(debt_writtenoff) & debt_writtenoff > 10000)

scatter_df <- dplyr::inner_join(dplyr::inner_join(xvals, yvals, by = "demarcation.code"), munic_list %>% dplyr::select(demarcation.code, demarcation.label, hh_n), by = "demarcation.code") %>%
                dplyr::mutate(outst = log1p(debt_outstanding),
                                wroff = log1p(debt_writtenoff), 
                                ratio = debt_writtenoff / debt_outstanding) 

summary(scatter_df$ratio)

fmt <- scales::label_number(big.mark = " ", accuracy = 1)

ratios <- c(0.01, 0.05, 0.35, 1.2, 3)

scatterplot <- ggplot(data = scatter_df, aes(x = debt_outstanding, y = debt_writtenoff,  
                        text = paste0(demarcation.label, " (", demarcation.code, ")", 
                        "<br>Subsequent write-offs as % of 2021/22 Receivables : ", scales::percent(ratio, accuracy = 1), 
                        "<br>2021/22 Receivables : R", fmt(debt_outstanding), 
                        "<br>", " 2022/23 - 2024/25 Write-offs : R", fmt(debt_writtenoff)))) +
                geom_point(aes(size = 1.5 * hh_n), 
                        colour = "#5C8A5A",
                        alpha = 0.75,
                        position = position_jitter(width = 0.2, height = 0.3),
                        show.legend = FALSE) +
                geom_abline(intercept = log10(ratios), slope = 1,
                                linetype = 2, colour = "grey65", linewidth = 0.4) +
                scale_x_log10(
                                labels  = function(x) ifelse(x == 0, "", scales::label_number(big.mark = " ")(x)), 
                                breaks = scales::breaks_log(n = 6), 
                                expand = c(0.0125, 0.05)) +
                scale_y_log10(labels  = function(x) ifelse(x == 0, "", scales::label_number(big.mark = " ")(x)), 
                                breaks = scales::breaks_log(n = 6), 
                                expand = c(0.0125, 0.05)) +
                scale_colour_manual(values = "#2E4B3C") +
                labs(title = "Closing Receivables (2021/22) vs Subsequent Debt Write-offs (2022/23 - 2024/25)", 
                x = "Receivables Older than 90 Days at 2021/22 closing", 
                y = "Total Write-offs, 2022/23 - 2024/25 ", 
                caption = "substack.com/@onderstromings\n \nSource: National Treasury\nNote: 147 of 213 municipalities had complete data.\n    Rand values are logged") +
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
print(ratios)

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
        text = "substack.com/@onderstromings<br>Source: National Treasury SA<br>Note: 148 of 213 municipalities had complete data.\n    Closing debt refers to debts older than 90 days at closing, while write-offs include write-offs of debts which arose subsequently",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        x = 0, y = -0.30,
        xanchor = "left", yanchor = "top",
        font = list(size = 10, color = "#8C8C8C")
      )
    )
  )

scatter_hover

hover_path <- paste0(output_dir, "/scatterplot_hover.html")

scatter_html_path <- paste0(output_dir, "/scatterplot.html")
htmlwidgets::saveWidget(scatter_hover, file = scatter_html_path, selfcontained = TRUE)

####
## GeoMap
####


debt_map <- dplyr::bind_rows(years_outst) %>%
                dplyr::filter(period == 2025) %>%
                dplyr::select(demarcation.code, period, type, value_raw, itemcode) %>%
                tidyr::pivot_wider(id_cols = demarcation.code, 
                                names_from = itemcode, 
                                values_from = value_raw)

look <- lineplot_init_df %>%
        dplyr::filter(demarcation.code == "CPT") 

start_year <- 2022
end_year   <- 2025

# Endpoint CAGR with a strict window: both endpoints must exist and be positive
cagr <- function(value, year, start = start_year, end = end_year) {
  v0 <- dplyr::first(value[year == start])
  v1 <- dplyr::first(value[year == end])
  if (is.na(v0) || is.na(v1) || v0 <= 0 || v1 <= 0) return(NA_real_)
  (v1 / v0)^(1 / (end - start)) - 1
}

ratio_cagr <- function(df, ratios) {
  df %>%
    dplyr::group_by(demarcation.code, period) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(ratios), ~ mean(.x, na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(ratios), ~ dplyr::if_else(is.nan(.x), NA_real_, .x))) %>%
    dplyr::group_by(demarcation.code) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(ratios), ~ cagr(.x, period),
                                   .names = "{sub('_ratio$', '_cagr', .col)}"),
                     .groups = "drop")
}

electricity_waste_map <- ratio_cagr(
  lineplot_init_df,
  c("total_ratio", "electricity_ratio", "waste_ratio")
)

water_df_map <- ratio_cagr(
  lineplot_init_df %>% dplyr::filter(!stringr::str_detect(revenue_group, "C2")),
  c("water_ratio", "waterwaste_ratio")
)

lineplot_df_map <- left_join(electricity_waste_map, water_df_map, by = "demarcation.code")

map_data_df <- dplyr::left_join(dplyr::left_join(dplyr::left_join(dplyr::left_join(dplyr::left_join(revenue_df %>% filter(amount_type.code == "AUDA", period == 2025), debt_map, by = "demarcation.code"), lineplot_df_map , by = "demarcation.code"), scatter_df, by = "demarcation.code"), munic_list %>% dplyr::select(demarcation.code, demarcation.label), by = "demarcation.code"), debt_distribution_df_i %>% dplyr::select(period, demarcation.code, value_raw_i) %>% dplyr::filter(period == 2025 & value_raw_i >= 0 & value_raw_i < 10), by = "demarcation.code") 

map_resp <- httr::GET("https://services7.arcgis.com/oeoyTUJC8HEeYsRB/arcgis/rest/services/LocalMunicipalities2018_Final/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")

writeLines(httr::content(map_resp, "text", encoding = "UTF-8"), here::here("data", "munic_map.geojson"))

map <- sf::st_read(here::here("data", "munic_map.geojson")) %>%
                dplyr::select(CAT_B, DATE, Shape__Area, Shape__Length, geometry) %>%
                dplyr::rename(demarcation.code = CAT_B)

map_df <- left_join(map_data_df, map,  by = "demarcation.code") %>%
        dplyr::mutate(flagged = ifelse( !is.na(value_raw), 0, 1))

pal <- colorNumeric(palette = c("#F7F5EF", "#DCE9C4", "#B8C99A", "#8AA870", "#5C8A5A", "#2E4B3C", "#1A2E22"),
                domain = map_df$value_raw[map_df$flagged != 1], 
                na.color = "#9E9E9E")

i_char <- as.character(2025)

year <- paste0("2024/25")

fmt_pct <- function(x, scale = 100) {
  ifelse(is.na(x) | !is.finite(x), "NA",
         scales::percent(x, accuracy = 0.1, scale = scale))
}

ind  <- "&nbsp;&nbsp;&nbsp;"
has_water <- !stringr::str_detect(map_df[["revenue_group.x"]], "C2") &
             !is.na(map_df[["revenue_group.x"]])

water_aged <- ifelse(
  has_water,
  paste0(ind, "Unresolved debt (Water): ", fmt_pct(map_df[["1200"]]), "<br>",
         ind, "Unresolved debt (Waste water): ", fmt_pct(map_df[["1500"]]), "<br>"),
  ""
)

water_cagr <- ifelse(
  has_water,
  paste0(ind, "Change in debt to revenue (Water): ", fmt_pct(map_df[["water_cagr"]], scale = 1), "<br>",
         ind, "Change in debt to revenue (Waste water): ", fmt_pct(map_df[["waterwaste_cagr"]], scale = 1), "<br>"),
  ""
)

datawrapper_table <- map_data_df %>%
                dplyr::select(demarcation.label, `1200`, `1300`, `1400`, `1500`, value_raw_i, electricity_cagr, waste_cagr, water_cagr, waterwaste_cagr, total_cagr, ratio) %>%
                dplyr::rename(`Municipality` = `demarcation.label`,
                                `Aged Debt % (Total)` = `value_raw_i`, 
                                `Aged Debt % (Water)` = `1200`, 
                                `Aged Debt % (Electricity)` = `1300`, 
                                `Aged Debt % (Property Rates)` = `1400`, 
                                `Aged Debt % (Waste Water)` = `1500`, 
                                `Debt-to-Revenue Change (Total)` = total_cagr, 
                                `Debt-to-Revenue Change (Water)` = water_cagr, 
                                `Debt-to-Revenue Change (Electricity)` = electricity_cagr, 
                                `Debt-to-Revenue Change (Waste Water)` = waterwaste_cagr, 
                                `Debt-to-Revenue Change (Waste)` = waste_cagr, 
                                `Write-offs to 2021/22 Debt` = ratio)


csvpath <- paste0(output_dir, "/receivables.csv")
utils::write.csv(datawrapper_table, csvpath, row.names = FALSE)
