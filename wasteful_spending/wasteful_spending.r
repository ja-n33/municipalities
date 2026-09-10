pacman::p_load(dplyr, httr, jsonlite, ggplot2, sf, leaflet, scales, cowplot, magick, patchwork)

output_dir <- here::here("wasteful_spending", "output")

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
if (TRUE){

#### 
##Create Drilldowns
####
drilldown_uifw <- paste(
  "financial_year_end.year",
  "item.label", "item.code",
  "demarcation.label", "demarcation.code",
  sep = "|"
)

drilldown_capital <- paste(
  "amount_type.label", "amount_type.code",
  "financial_period.period", "period_length.length",
  "financial_year_end.year",
  "capital_type.label", "capital_type.code",
  "item.label", "item.code",
  "demarcation.label", "demarcation.code",
  sep = "|"
)

drilldown_incexp <- paste(
  "amount_type.label", "amount_type.code",
  "financial_period.period", "period_length.length",
  "financial_year_end.year",
  "item.label", "item.code",
  "demarcation.label", "demarcation.code",
  sep = "|"
)

drilldown_debtors <- paste(
  "amount_type.label", "amount_type.code",
  "customer_group.code", 
  "financial_period.period",
  "financial_year_end.year",
  "item.label", "item.code",
  "demarcation.label", "demarcation.code",
  sep = "|"
)

####
##UIFW
####

fetch_data_uifw <- function(year, 
                        amount_type = "AUDA", 
                        url = "https://municipaldata.treasury.gov.za/api/cubes/uifwexp/aggregate", 
                        drilldown = drilldown_uifw,
                        pagesize = 5000, 
                        page = 1, 
                        itemcode = "fruitless"){

  base_url <- url
  
  cut <- sprintf("financial_year_end.year:%d|item.code:%s", year, itemcode)

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

nt_data_uifw <- list()

itemcodes <- c("fruitless", "irregular", "unauthorised")

for (yr in (2018:2025)){
        for (item in itemcodes){

                        year_df <- fetch_data_uifw(year = yr, itemcode = item)

                        nt_data_uifw[[item]][[as.character(yr)]] <- year_df

                message(paste0(item, ", ", yr))
                }
}

df_raw <- c()

for (item in itemcodes){

        df_temp <- bind_rows(nt_data_uifw[[item]])

        df_raw <- rbind(df_raw, df_temp) # %>% dplyr::select(-`_count`))
}

df_uifw <- df_raw %>%
        tidyr::pivot_wider(id_cols = c(financial_year_end.year, demarcation.code), names_from = item.code, values_from = amount.sum) %>%
        dplyr::rename(period = financial_year_end.year) %>%
        dplyr::left_join(munic_list %>% dplyr::select(demarcation.code, demarcation.label), by = "demarcation.code")

label_code_lookup <- munic_list %>%
        distinct(demarcation.label, demarcation.code) %>%
        dplyr::mutate(demarcation.label = tolower(demarcation.label))

mfma2425 <- readr::read_csv(here::here("wasteful_spending", "mfma_annexure1_2024-25_clean.csv"), n_max = 270) %>%
  dplyr::filter(auditee_category == "MUN") %>%
  dplyr::rename(demarcation.label = auditee, 
                unauthorised = unauthorised_expenditure_rand_millions, 
                irregular = irregular_expenditure_rand_millions, 
                fruitless = fruitless_wasteful_expenditure_rand_millions) %>%
  dplyr::mutate(dplyr::across(c(fruitless, unauthorised, irregular), 
                ~ dplyr::case_when(is.finite(.x) ~ .x * 1000000, TRUE ~ 0))) %>%
  dplyr::mutate(period = 2025, 
                demarcation.label = case_when(demarcation.label == "Emalahleni" & province == "EC" ~ "Emalahleni (EC)", 
                        demarcation.label == "Emalahleni" & province == "MP" ~ "Emalahleni (MP)", 
                        demarcation.label == "Big 5 Hlabisa" ~ "Hlabisa Big Five", 
                        demarcation.label == "City of Cape Town Metro" ~ "Cape Town",
                        demarcation.label == "Nelson Mandela Bay Metro" ~ "Nelson Mandela Bay",
                        demarcation.label == "City of Johannesburg Metro" ~ "City of Johannesburg",
                        demarcation.label == "eThekwini Metro" ~ "eThekwini",    
                        demarcation.label == "Mangaung Metro" ~ "Mangaung",
                        demarcation.label == "eThekwini Metro" ~ "eThekwini",   
                        demarcation.label == "Buffalo City Metro" ~ "Buffalo City",   
                        demarcation.label == "City of Tshwane Metro" ~ "City of Tshwane", 
                        demarcation.label == "City of Ekurhuleni Metro" ~ "City of Ekurhuleni", 
                        demarcation.label == "Kai !Garib" ~ "!Kai! Garib", 
                        demarcation.label == "Elias Motsoaledi (Greater Groblersdal)" ~ "Elias Motsoaledi", 
                        demarcation.label == "Bela-Bela" ~ "Bela Bela", 
                        demarcation.label == "Dr Beyers Naudé" ~ "Dr Beyers Naude",
                        demarcation.label == "Chief Albert Luthuli" ~ "Albert Luthuli",
                        demarcation.label == "Dr. JS Moroka" ~ "Dr J.S. Moroka",
                        demarcation.label == "Port St. Johns" ~ "Port St Johns",
                        demarcation.label == "Mbombela" ~ "Mbombela",
                        demarcation.label == "West Rand" ~ "West Rand City",
                        demarcation.label == "Bergriver" ~ "Bergrivier",
                        demarcation.label == "Amahlati" ~ "Amahlathi",
                        demarcation.label == "Dr A B Xuma" ~ "Dr. A.B. Xuma",
                        demarcation.label == "Dr Pixley Ka Isaka Seme" ~ "Pixley Ka Seme",
                        demarcation.label == "JB Marks" ~ "J B Marks",
                        demarcation.label == "Modimolle-Mookgophong" ~ "Modimolle-Mookgopong",
                        demarcation.label == "Ingquza Hill" ~ "Ngquza Hills",
                        demarcation.label == "Kou Kamma" ~ "Kou-Kamma",
                        demarcation.label == "Kgetlengriver" ~ "Kgetlengrivier", 
                        demarcation.label == "Mahikeng" ~ "Mafikeng", 
                        demarcation.label == "Matlosana" ~ "City of Matlosana", 
                        demarcation.label == "Naledi" & province == "NW" ~ "Naledi (NW)", 
                        demarcation.label == "Fetakgomo Tubatse" ~ "Tubatse Fetakgomo",
                        demarcation.label == "Pixley Ka Seme" ~ "Pixley Ka Seme (MP)", 
                        demarcation.label == "Lepelle Nkumpi" ~ "Lepelle-Nkumpi",
                        demarcation.label == "Intsika Yethu" ~ "Vuyisile Mini",
                        demarcation.label == "Lekwa Teemane" ~ "Lekwa-Teemane",
                        demarcation.label == "Makhudutamaga" ~ "Makhuduthamaga",
                        TRUE ~ demarcation.label),
                demarcation.label = tolower(demarcation.label)) %>%
  dplyr::select(demarcation.label, period, unauthorised, irregular, fruitless)


mfma2425 <- dplyr::left_join(mfma2425, label_code_lookup, by = "demarcation.label") %>%
        dplyr::filter(!is.na(demarcation.code))

df_uifw <- bind_rows(df_uifw, mfma2425)

####
##Debtor data 
####

fetch_data_debtors <- function(year, 
                        url = "https://municipaldata.treasury.gov.za/api/cubes/aged_debtor_v2/aggregate", 
                        drilldown = drilldown_debtors,
                        pagesize = 5000, 
                        page = 1, 
                        uifw_code = "1820", 
                        amount_type = "AUDA",
                        aggregate = "total_amount.sum"){

  base_url <- url

  
  cut <- sprintf('amount_type.code:"%s"|financial_year_end.year:%d|item.code:"%s"|period_length.length:%s', amount_type, year, uifw_code, "year")

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

for (yr in (2018:2025)){
        if (yr >= 2019){
                aggs <- "total_amount.sum|badi_amount.sum|bad_amount.sum|l30_amount.sum|l60_amount.sum|l90_amount.sum|l120_amount.sum|l150_amount.sum|l180_amount.sum|g1_amount.sum|l1_amount.sum"
                debtor_url <- "https://municipaldata.treasury.gov.za/api/cubes/aged_debtor_v2/aggregate"

                } else {
                aggs <- "total_amount.sum|badi_amount.sum"
                debtor_url <- "https://municipaldata.treasury.gov.za/api/cubes/aged_debtor/aggregate"

                }

                year_df <- fetch_data_debtors(year = yr, url = debtor_url, aggregate = aggs)

                nt_data_debtor[[as.character(yr)]] <- year_df

                message(paste0(yr, " completed"))
                }


debt <- bind_rows(nt_data_debtor)

debt_merge <- debt %>%
        dplyr::select(dplyr::any_of(c("customer_group.code", "financial_period.period", "item.code", "demarcation.code", "total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")),
                        dplyr::matches("^l")) %>%
        dplyr::rename(period = financial_period.period) %>%
        dplyr::group_by(demarcation.code, period) %>%
        dplyr::summarise(
                        dplyr::across(c(dplyr::any_of(c("total_amount.sum", "badi_amount.sum", "bad_amount.sum", "g1_amount.sum")), dplyr::matches("^l")),
                         ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(across(c("g1_amount.sum", dplyr::matches("^l")), ~.x / total_amount.sum * 100, .names = "{stringr::str_remove(.col, 'amount.sum')}pct"))
####
##Total Expenditure 
####

fetch_data_exp <- function(year, 
                        amount_type = "AUDA", 
                        url = "https://municipaldata.treasury.gov.za/api/cubes/capital_v2/aggregate", 
                        drilldown = drilldown_capital,
                        pagesize = 5000, 
                        page = 1){

  base_url <- url

  
  cut <- sprintf("financial_year_end.year:%d|amount_type.code:%s|period_length.length:%s", year, amount_type, "year")

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
                                col_types = readr::cols(item.code = readr::col_character()),
                                show_col_types = FALSE)
        if(nrow(df_temp) == 0) break 

        all_pages[[page]] <- df_temp

        if (nrow(df_temp) < pagesize) break
        page <- page + 1
        Sys.sleep(0.5)  
        
        }
        bind_rows(all_pages)   # <- add this line back

        }



configs <- list(
        capital = list(
                url = "https://municipaldata.treasury.gov.za/api/cubes/capital_v2/aggregate",
                drilldown = drilldown_capital, 
                string_name = "capital",
                selection = c("amount_type.code", "financial_period.period", "capital_type.code", "item.code", "demarcation.code", "amount.sum"),
                df_name = "capital_years"
        ),
        incexp = list(
                url = "https://municipaldata.treasury.gov.za/api/cubes/incexp_v2/aggregate",
                drilldown = drilldown_incexp,
                string_name = "incexp",
                selection = c("amount_type.code", "financial_period.period", "item.code", "demarcation.code", "amount.sum"),
                df_name = "incexp_years"

        )
        )


nt_data_exp <- list()

for (yr in (2018:2025)){
        for (config in names(configs)){
                cfg <- configs[[config]]

                        year_df <- fetch_data_exp(year = yr, url = cfg$url, drilldown = cfg$drilldown)

                        nt_data_exp[[config]][[as.character(yr)]] <- year_df

                message(paste0(config, ", ", yr))
                }
        }

wide_list <- list()

for (config in names(configs)){
                cfg <- configs[[config]]

                df_raw_exp <- bind_rows(nt_data_exp[[cfg$string_name]]) %>%
                         dplyr::select(any_of(cfg$selection))

                if (config == "capital"){
                        df_wide <- df_raw_exp %>%
                                tidyr::pivot_wider(id_cols = c(financial_period.period,  item.code, demarcation.code), names_from = capital_type.code, values_from = amount.sum)
                } else {
                        df_wide <- df_raw_exp %>%
                                tidyr::pivot_wider(id_cols = c(financial_period.period, demarcation.code), names_from = item.code, values_from = amount.sum)
                }

                wide_list[[config]] <- df_wide
}


capital_full <- wide_list$capital

incexp_full <- wide_list$incexp


incexp_df <- incexp_full %>%
        dplyr::mutate(opex = rowSums(across(any_of(c("3000", "3100", "3200", "3300", "3400", "3500",
                      "3600", "3700", "3800", "3900", "4000", "4100",
                      "4200", "4300"))), na.rm = TRUE)) %>%
        dplyr::select(financial_period.period, demarcation.code, opex) %>%
        dplyr::rename(period = financial_period.period) 

capital_df_ungrouped <- capital_full %>%
        mutate(NEW = case_when(is.na(NEW) ~ 0, TRUE ~ NEW),
                RENEWAL = case_when(is.na(RENEWAL) ~ 0, TRUE ~ RENEWAL),
                UPGRADING = case_when(is.na(UPGRADING) ~ 0, TRUE ~ UPGRADING),
                DEPRECIATION = case_when(is.na(DEPRECIATION) ~ 0, TRUE ~ DEPRECIATION),
                REPAIR_MNT = case_when(is.na(REPAIR_MNT) ~ 0, TRUE ~ REPAIR_MNT),
                item_code = as.numeric(item.code), 
                gross = NEW + RENEWAL + UPGRADING + REPAIR_MNT, 
                net = gross - DEPRECIATION,
                new = NEW) %>%
        mutate(Category = case_when(
                item_code >= 120 & item_code <= 190  | item_code == 990 ~ "Road Transport",
                item_code >= 210 & item_code <= 290 ~ "Electricity",
                item_code >= 310 & item_code <= 470 ~ "Water Sanitation", 
                item_code >= 490 & item_code <= 550 ~ "Solid Waste",
                item_code >= 570 & item_code <= 650 ~ "Rail Transport",
                item_code >= 670 & item_code <= 710 ~ "Coastal Infrastructure",
                item_code >= 730 & item_code <= 760 ~ "ICT",
                item_code >= 790 & item_code <= 1040 ~ "Community Facilities",
                item_code >= 1060 & item_code <= 1100 ~ "Heritage",
                TRUE ~ "Other"), 
                category = stringr::str_replace(tolower(Category), " ", "_")) %>%
        dplyr::rename(period = financial_period.period) %>%
        tidyr::complete(period, demarcation.code, category) %>%
        dplyr::group_by(period, demarcation.code) %>%
        dplyr::mutate(gross_pmax = pmax(gross, 0)) %>% ## Chalk off reversals
        dplyr::summarise(gross_pmax = sum(gross_pmax, na.rm = TRUE), 
                        net = sum(net, na.rm = TRUE), 
                        new = sum(new, na.rm = TRUE),
                        .groups = "drop") %>%
        left_join(incexp_df, by = c("demarcation.code", "period")) %>%
        dplyr::filter(stringr::str_detect(demarcation.code, "DC") == FALSE) %>%
        dplyr::mutate(capex = gross_pmax, 
                total_exp = capex + opex) 

capital_df <- capital_df_ungrouped %>%
        dplyr::group_by(period, demarcation.code) %>%
        dplyr::summarise(total_opex = sum(opex, na.rm = TRUE), 
                        total_exp = sum(total_exp, na.rm = TRUE))

flagged_df <- capital_df_ungrouped %>%
        dplyr::filter(period >= 2023) %>%
        dplyr::group_by(demarcation.code) %>%
        dplyr::summarise(total_exp = sum(total_exp, na.rm = TRUE), 
                        total_capex = sum(capex, na.rm = TRUE)) %>%
        dplyr::mutate(capex_ratio = ifelse(total_capex > 0, total_capex / total_exp, NA_integer_ ), 
                        flagged = ifelse(capex_ratio > 0.4 | is.na(capex_ratio), 1, 0)) %>%
        dplyr::select(demarcation.code, flagged, capex_ratio)


####
##Bind four dataframes
####

df_uifw <- left_join(left_join(left_join(df_uifw, munic_list %>% dplyr::select(-demarcation.label), by = "demarcation.code"), capital_df, by = c("demarcation.code", "period")), debt_merge, by = c("demarcation.code", "period")) %>%
        dplyr::filter(stringr::str_detect(demarcation.code, "DC") == FALSE) %>%
        dplyr::mutate(total_uifw = fruitless + irregular + unauthorised, 
                pct_opex = (total_uifw / total_opex) * 100, 
                pct_total = (total_uifw / total_exp) * 100) 

##Keep debt df for later use

}

####################################################################################################################################################################################

                                                        ## CREATE PLOTS ##

####################################################################################################################################################################################
 
####
##Area plot
####

if (TRUE){


uifw_plot_wide <- df_uifw %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(across(c("fruitless", "irregular", "unauthorised", "total_exp"),  ~sum(.x, na.rm = TRUE))) %>% 
        dplyr::mutate(total_uifw = fruitless + irregular + unauthorised, 
                pct = (total_uifw / total_exp) * 100) %>%
        filter(period > 2017)

uifw_plot_long <- uifw_plot_wide %>%
        tidyr::pivot_longer(cols = c("fruitless", "irregular", "unauthorised", "pct"), 
                        names_to = "category", 
                        values_to = "value") %>%
        mutate(category_label = recode(category,
        fruitless = "Fruitless & Wasteful",
        irregular = "Irregular", 
        unauthorised = "Unauthorised", 
        pct = "Percentage of Total Expenditure")) %>%
        dplyr::select(-category) %>%
        dplyr::filter(period > 2019)

axis_fact <- max(uifw_plot_wide[c("fruitless", "irregular", "unauthorised")], na.rm = TRUE)  / 8
print(axis_fact)

area_plot <- ggplot(data = uifw_plot_long, aes(x = as.numeric(period), y = value)) + 
        geom_area(data = uifw_plot_long %>% dplyr::filter(!(category_label == "Percentage of Total Expenditure")),
                linewidth = 4, 
                outline.type = "full",
                aes(fill = category_label)) +
        geom_line(data = uifw_plot_long %>% dplyr::filter(category_label == "Percentage of Total Expenditure"),
                aes(y = value * axis_fact),
                colour = "#F7F5EF",
                linewidth = 1.5, 
                show.legend = FALSE
                ) +
        geom_line(data = uifw_plot_long %>% dplyr::filter(category_label == "Percentage of Total Expenditure"),
                aes(y = value * axis_fact, colour = category_label, linetype = category_label),
                linewidth = 1,
                show.legend = FALSE) +
        scale_fill_manual(values = c("Unauthorised" = "#2E4B3C",
                                        "Irregular" = "#A3B86C",
                                        "Fruitless & Wasteful" = "#5C8A5A")) +
        scale_y_continuous(name = "R billion", 
                breaks = scales::breaks_pretty(n = 6),
                minor_breaks = scales::breaks_pretty(n = 12),
                labels  = function(x) ifelse(x == 0, "", scales::label_number(scale = 1e-9, big.mark = ",")(x)), 
                sec.axis = sec_axis(~. / axis_fact, 
                        name = "Percentage of Total Expenditure", 
                        breaks = scales::breaks_pretty(n = 6), 
                        labels  = function(x) ifelse(x == 0, "", x)), 
                expand = expansion(mult = c(0.001, 0.05))) +
        scale_x_continuous(name = "Financial Year", 
                breaks =  c(2018:2026),
                labels = function(x) paste0(x-1, "/", stringr::str_sub(x, start = 3, end = 4)), 
                expand = expansion(mult = c(0, 0))) + 
        scale_linetype_manual(values = c("Percentage of Total Expenditure" = "solid")) +
        scale_colour_manual(values = c("Percentage of Total Expenditure" = "#C9A66B")) +
        labs(title    = "Unauthorised, Irregular, Fruitless & Wasteful Spending\namong Municipalities, 2019/20 - 2024/25",
        x        = "Financial Year",
        caption = "substack.com/@onderstromings\n \nSource: National Treasury") +
        guides(fill = guide_legend(title = NULL, order = 1)) +
        substack_theme + 
        theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                panel.border = element_blank())


area_plot
area_path <- paste0(output_dir, "/areaplot_update.png")


png(filename = area_path,
        width = 1137,
        height = 650,
        res = 130)
print(area_plot)
dev.off()

print(5648118000/5104819224 - 1)
##currently a 10.6% increase yoy 2024 to 2025 in fruitless,

print(26529790000/23860286443 - 1) ## Irregular
#11.19%

print(29346410000/27618605411 - 1) ## Unauthorised
# 6.26%

print(725171539186/616079532170 - 1) ## Total Exp
# 17.71%

print(61524318000/56583711078 - 1) ## Total UIFWE
# 8.7%

}


####
## Debtor Age Profile
####


if (TRUE){

 
bracket_order <- c("< 90", "91 - 180", "181 - 360", "> 360")

hist_data <- debt_merge %>%
        dplyr::select(period, total_amount.sum, badi_amount.sum, bad_amount.sum, dplyr::matches("^l|g1")) %>%
        dplyr::filter(period >= 2020, !(stringr::str_detect(stringr::str_sub(demarcation.code, 1, 2), "DC"))) %>%
        tidyr::pivot_longer(cols = c(dplyr::matches("pct$"), dplyr::matches("[0-9]_amount.sum")),
                                names_to = c("horison", ".value"),
                                names_pattern = "(.+)_(amount\\.sum|pct)") %>%
        dplyr::mutate(type = case_when(horison %in% c("total_amount.sum", "badi_amount.sum", "bad_amount.sum") ~ "0", 
                TRUE ~ "1"), 
                        label = case_when(horison %in% c("l30", "l60", "l90") ~ "< 90",
                                        horison %in% c("l120", "l150", "l180") ~ "91 - 180",
                                        horison == "l1" ~ "181 - 360",
                                        horison == "g1" ~ "> 360", 
                                        TRUE ~ "Alt"), 
                        label = factor(label, levels = bracket_order)) %>%
        dplyr::filter(amount.sum > 0, !(demarcation.code == "KZN225" & period == 2023))

        ##Exclude Msunduzi 2023, its reported receivable amount was  393166474, nearly 10x greater than any other observation


check <- hist_data %>%
        dplyr::group_by(period, horison) %>%
        dplyr::summarise(amount = sum(amount.sum)) %>%
        dplyr::filter(period >= 2022)

max <- hist_data %>%
                dplyr::group_by(label, period) %>%
                dplyr::summarise(amount = sum(amount.sum, na.rm = TRUE)) %>%
                arrange(desc(amount)) %>%
                head(1) %>%
                pull(amount)

y_breaks <- scales::breaks_pretty(n = 6)(c(0, 50000000))
##ymax = 26535310, use 30000000
print(max)

debthorisons <- list()
for (year in c(2023:2025)){

        plot_data <- hist_data %>%
                dplyr::filter(period == year) %>%
                dplyr::group_by(label) %>%
                dplyr::summarise(amount = sum(amount.sum, na.rm = TRUE), .groups = "drop")
        fyear <- paste0((year - 1), "/", stringr::str_sub(year, -2, -1))
        debthist_temp <- ggplot(data = plot_data, aes(x = label, y = amount)) +
                geom_col(fill = "#5C8A5A") +
                labs(y = "R Million",
                        x = "Days Outstanding", 
                        title = fyear) +
                scale_y_continuous(breaks = y_breaks,
                        minor_breaks = scales::breaks_pretty(n = 12),
                        labels  = function(x) ifelse(x == 0, "", scales::label_number(scale = 1e-6, big.mark = ",")(x)),
                        expand = expansion(mult = c(0, 0))) +
                coord_cartesian(ylim = c(0, max * 1.05)) +
                substack_theme +
                theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), 
                        plot.title.position = "panel", 
                        plot.title = element_text(colour = "#1F2A24", size = rel(1), hjust = 0.5, face = "plain"),
                        panel.border = element_blank()) 

        debthorisons[[as.character(year)]] <- debthist_temp
}

# Check recovery % after a year

period_totals <- hist_data %>%
  dplyr::filter(demarcation.code != "KZN225") %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(total_amount = sum(amount.sum, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(period) %>%
  dplyr::mutate(prev_total = dplyr::lag(total_amount, order_by = period))

g1_by_period <- hist_data %>%
  dplyr::filter(horison == "g1", demarcation.code != "KZN225") %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(g1_amount = sum(amount.sum, na.rm = TRUE), .groups = "drop")

debt_roll <- g1_by_period %>%
  dplyr::left_join(period_totals, by = "period") %>%
  dplyr::mutate(carried = g1_amount / prev_total)


combined_plot <- patchwork::wrap_plots(debthorisons, nrow = length(debthorisons)) +
                patchwork::plot_layout(axis_titles = "collect", guides = "collect", axes = "collect") +
                patchwork::plot_annotation(title = "Age of Recoverable UIFWE Debt, 2022/23 - 2024/25",
                                caption = "substack.com/@onderstromings\nSource: National Treasury\nNote: Msunduzi (KZN225) was dropped in 2023 as total value was 10x higher than next highest amount.", 
                                theme = substack_theme + theme(axis.title = element_text(face = "plain")))


debt_plot <- cowplot::ggdraw(combined_plot) +   
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

debthist_path <- paste0(output_dir, "/debthist_update.png")

png(filename = debthist_path,
        width = 1137,
        height = 800,
        res = 190)
print(debt_plot)
dev.off()
}
####
## Debtor ID 
####
if (TRUE){
piechart_df <- debt %>%
        dplyr::filter(financial_year_end.year == 2025 & !(stringr::str_detect(stringr::str_sub(demarcation.code, start = 1, end = 2), "DC")) & total_amount.sum >= 0 & demarcation.code != "KZN225") %>%
        dplyr::select(total_amount.sum, demarcation.label, demarcation.code, customer_group.code) %>%
        dplyr::mutate(customer.code = case_when(stringr::str_detect(stringr::str_sub(customer_group.code, start = 1, end = 2), "22") ~ "Organs of the State", 
                                stringr::str_detect(stringr::str_sub(customer_group.code, start = 1, end = 2), "23") ~ "Commercial",
                                stringr::str_detect(stringr::str_sub(customer_group.code, start = 1, end = 2), "24") ~ "Households",
                                stringr::str_detect(stringr::str_sub(customer_group.code, start = 1, end = 2), "25") ~ "Other",
                                TRUE ~ "NA")) %>% 
        dplyr::group_by(customer.code) %>%
        dplyr::summarise(group.sum = sum(total_amount.sum, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(total.sum = sum(group.sum, na.rm = TRUE),
                        pct = (group.sum  / total.sum) * 100)

piechart <- ggplot(piechart_df, aes(x = "", y = pct, fill = customer.code)) +
                geom_col(width = 1) +
                scale_fill_manual(values = c("Households" = "#2E4B3C", "Commercial" = "#5C8A5A", "Organs of the State" = "#A3B86C", "NA" = "#9E9E9E")) +
                coord_polar(theta = "y") +
                geom_text(aes(label = ifelse(pct > 5, paste0(round(pct), "%"), "")), 
                        position = position_stack(vjust = 0.5), 
                        color = "black", 
                        size = 5,
                        fontface = "plain") +
                labs(title = "Receivable UIFWE Composition by Customer\nCategory, 2024/25",
                        fill = "Customer Group",
                        x = "", 
                        y = ""
                #caption = "substack.com/@onderstromings\nSource: National Treasury\nNote: Negative entries and Msunduzi (KZN225) were dropped."
                ) +
                guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
                substack_theme + 
                theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), 
                        plot.title.position = "panel", 
                        panel.border = element_blank(), 
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title = element_blank(),
                        plot.title = element_text(face = "plain"))


age_header <- ggplot() + 
  labs(title = "Age of Recoverable UIFWE Debt, 2022/23 - 2024/25") +
  substack_theme + 
  theme(plot.title = element_text(face = "plain"))

combined_with_title <- age_header / combined_plot +
  patchwork::plot_layout(heights = c(1, 15)) 


pie_cols <- patchwork::wrap_plots(list(combined_with_title, piechart), nrow = 1) +
                patchwork::plot_layout(axis_titles = "collect", axes = "collect") +
                patchwork::plot_annotation(title = "UIFWE Receivables Profile",
                                caption = "substack.com/@onderstromings\n \nSource: National Treasury\nNote: Negative entries and Msunduzi (KZN225) were dropped.", 
                                theme = substack_theme + theme(plot.title = element_text(size = rel(1.3), hjust = 0.5)))

pie_cols



combined_path <- paste0(output_dir, "/combined_update.png")

png(filename = combined_path,
        width = 1137,
        height = 800,
        res = 110)
print(pie_cols)
dev.off()
}

####
## By Metros
####

if (TRUE){

barplot_df <- df_uifw %>%
        dplyr::filter(period >= 2023 & period <= 2024, municipality.miif_category == "A") %>%
        dplyr::group_by(demarcation.code) %>%
        dplyr::summarise(dplyr::across(c(fruitless, irregular, unauthorised, total_exp),
                        ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(uifw = fruitless + irregular + unauthorised, 
                        dplyr::across(c(fruitless, irregular, unauthorised, uifw), 
                        ~.x / total_exp * 100, 
                        .names = "{.col}_pct")
                        ) %>%
        dplyr::select(demarcation.code, fruitless_pct, irregular_pct, unauthorised_pct, uifw_pct) %>%
        tidyr::pivot_longer(
                cols      = ends_with("_pct"),
                names_to  = "category",
                values_to = "value"
                ) %>%
        dplyr::mutate(
                category_label = recode(category,
                        fruitless_pct      = "Fruitless & Wasteful",
                        unauthorised_pct      = "Unauthorised",
                        irregular_pct = "Irregular",
                        uifw_pct   = "UIFW"),
                category_label = factor(category_label, levels = c("Fruitless & Wasteful", "Irregular", "Unauthorised", "UIFW")), 
                city = recode(demarcation.code,
                        CPT = "Cape Town",      ETH = "eThekwini",
                        JHB = "Johannesburg",   NMA = "Nelson Mandela Bay",
                        TSH = "Tshwane",        BUF = "Buffalo City",
                        MAN = "Mangaung", EKU = "Ekurhuleni")) %>%
        dplyr::group_by(demarcation.code) %>%
        dplyr::mutate(total_value = value[category_label == "UIFW"]) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(demarcation.code = forcats::fct_reorder(demarcation.code, total_value))
                
cat_colours <- c(
  "Unauthorised"        = "#2E4B3C",
  "Irregular" = "#A3B86C",
  "Fruitless & Wasteful"              = "#5C8A5A",
  "UIFW"        = "#C9A66B"
)

barplot <- ggplot(barplot_df, aes(x = value,
                    y = demarcation.code,
                    fill = category_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = cat_colours) +
  scale_x_continuous(
        labels  = function(x) ifelse(x == 0, "", (x)),
        limits = c(0, NA),
        expand = expansion(mult = c(0.001, 0.05)), 
        breaks = scales::breaks_pretty(n = 5)
) +
  labs(
    title    = "UIFWE as a Percentage of Total Expenditure by Metro,\n2022/23 - 2023/24",
    x        = "% of Total Expenditure",
    y        = NULL,
    fill     = NULL,
    caption = "substack.com/@onderstromings\n \nSource: National Treasury"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  substack_theme + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_line(colour = "#DCE0C8", linewidth = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    text               = element_text(colour = "#1F2A24"),
    axis.text          = element_text(colour = "#4A4460"),
    axis.text.x        = element_text(margin = margin(t = 0)),
    legend.position    = "bottom",
    legend.key.size    = unit(0.45, "cm"),
    plot.margin        = margin(12, 16, 8, 12), 
    panel.border = element_blank()
  ) 


barpath <- paste0(output_dir, "/metro_bar_uifwe.png")

png(filename = barpath,
        width = 1137,
        height = 650,
        res = 140)
print(barplot)
dev.off()

}
####
## Flagged Scatter
####
 
if (TRUE){
df_scatter <- dplyr::left_join(flagged_df, df_uifw, by = "demarcation.code") %>%
        dplyr::filter(period >= 2023) %>%
        dplyr::mutate(scatter_label = paste0(demarcation.label, " (", demarcation.code, ")")) %>%
        dplyr::group_by(scatter_label, demarcation.code) %>%
        dplyr::summarise(dplyr::across(c(fruitless, irregular, unauthorised, total_exp, total_opex),
                        ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(uifw = fruitless + irregular + unauthorised, 
                        dplyr::across(c(fruitless, irregular, unauthorised, uifw), 
                        ~.x / total_exp * 100, 
                        .names = "{.col}_pct")
                        ) %>%
        dplyr::select(scatter_label, uifw_pct, demarcation.code) %>%
        dplyr::left_join(flagged_df %>% dplyr::select(demarcation.code, capex_ratio, flagged), by = "demarcation.code") %>%
        dplyr::mutate(capex_ratio = capex_ratio * 100) %>%
        dplyr::filter(demarcation.code != "FS185" & is.finite(uifw_pct)) %>%
        dplyr::select(demarcation.code, flagged, capex_ratio, uifw_pct, scatter_label) 


cor(df_scatter$uifw_pct, df_scatter$capex_ratio, use = "complete.obs", method = "pearson")

scatter <- ggplot(data = df_scatter %>% dplyr::filter(flagged == 1), aes(x = capex_ratio, y = uifw_pct)) +
        geom_point(size = 3, colour =  "#5C8A5A") +
        ggrepel::geom_text_repel(
                aes(label = scatter_label), 
                                size = 2, colour = "#1F2A24", 
                                max.overlaps = Inf) +
        scale_x_continuous(labels  = function(x) ifelse(x == 40, "", (x)),
                        limits = c(40, 85), 
                        expand = expansion(mult = c(0, 0.03)), 
                        breaks = scales::pretty_breaks(n = 4)) +
        scale_y_continuous(
                labels  = function(x) ifelse(x == 0, "", (x)),
                limits = c(0, 60), 
                expand = expansion(mult = c(0, 0.03)), 
                breaks = scales::pretty_breaks(n = 4)
                ) + 
        labs(
                title = "UIFWE and Infrastructure Spending for Flagged\nMunicipalities, 2022/23 - 2023/24", 
                x = "Infrastructure Spending (%)",
                y = "UIFW (%)",
                caption = "substack.com/@onderstromings\n \nSource: National Treasury\nNote: Values are given as a percentage of total expenditure."
                ) +
        geom_hline(yintercept = median(map_df$uifw_pct * 100, na.rm = TRUE),
                        linetype = "dashed",
                        colour = "#C9A66B", 
                        linewidth = 0.75) +
        annotate("text", x = 45, y = (median(map_df$uifw_pct * 100, na.rm = TRUE) + 1),
                        label = "Median UIFWE (%)", 
                        colour = "#C9A66B", 
                        size = 2.75) +
        substack_theme + 
        theme(
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(), 
                panel.border = element_blank()
        )
scatter



scatterpath <- paste0(output_dir, "/scatter_uifwe_updated.png")

png(filename = scatterpath,
        width = 1200,
        height = 1100,
        res = 190)
print(scatter)
dev.off()

}


####
## Geomap
####

if (TRUE){

# Leaves will contain     1) demarcation label,
#                         2) unauthorised (amount and %)
#                         3) irregular (amount and %)
#                         4) fruitless (amount and %)
#                         5) Total (2022/23 & 2023/24) (amount and %)
#                         6) UIFWE receivable (2025) (% > year old)
 


map_resp <- httr::GET("https://services7.arcgis.com/oeoyTUJC8HEeYsRB/arcgis/rest/services/LocalMunicipalities2018_Final/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")

writeLines(httr::content(map_resp, "text", encoding = "UTF-8"), here::here("data", "munic_map.geojson"))

map <- sf::st_read(here::here("data", "munic_map.geojson")) %>%
                dplyr::select(CAT_B, DATE, Shape__Area, Shape__Length, geometry) %>%
                dplyr::rename(demarcation.code = CAT_B)


map_df_data <- df_uifw %>%
        dplyr::filter(period >= 2023) %>%
        dplyr::group_by(demarcation.code, demarcation.label) %>%
        dplyr::summarise(dplyr::across(c(fruitless, irregular, unauthorised, total_exp),
                        ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(uifw = fruitless + irregular + unauthorised, 
                        dplyr::across(c(fruitless, irregular, unauthorised, uifw), 
                        ~.x / total_exp, 
                        .names = "{.col}_pct"), 
                        dplyr::across(c(fruitless_pct, irregular_pct, unauthorised_pct, uifw_pct),
                        ~ case_when(!is.finite(.x) ~ NA_real_ , 
                                TRUE ~ .x))
                        )

pal <- colorNumeric(palette = c("#F7F5EF", "#DCE9C4", "#B8C99A", "#8AA870", "#5C8A5A", "#2E4B3C", "#1A2E22"),
                domain = c(0, 0.4), 
                na.color = "#9E9E9E")

map_df <- dplyr::left_join(dplyr::left_join(map_df_data, debt_merge %>% dplyr::filter(period == 2025) %>% dplyr::select(demarcation.code, total_amount.sum, g1_pct, g1_amount.sum), by = "demarcation.code"), map, by = "demarcation.code")  %>%
                dplyr::mutate(fill_colour = ifelse(!is.finite(uifw_pct), "#9E9E9E", pal(pmin(uifw_pct, 0.4)))) %>%
                dplyr::mutate(g1_pct = ifelse(is.finite(g1_pct), g1_pct / 100, NA_real_))

                        
interact_map <- leaflet(
                map_df %>%
                st_as_sf()
                ) %>%
        addPolygons(
                fillColor = ~fill_colour,
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
  "<b>", demarcation.label, "</b><br>",
  "<b>Total UIFWE - 2022/23 & 2023/24: R",
  paste0(scales::comma(uifw, big.mark = " "),
         ifelse(is.finite(uifw_pct),
                paste0(" (", scales::percent(uifw_pct, accuracy = 0.1), ")"),
                " (NA)")),
 "</b><br>",
  "&nbsp;&nbsp;&nbsp;Unauthorised Spending: R",
  paste0(scales::comma(unauthorised, big.mark = " "),
         ifelse(is.finite(unauthorised_pct),
                paste0(" (", scales::percent(unauthorised_pct, accuracy = 0.1), ")"),
                " (NA)")),
  "<br>",
  "&nbsp;&nbsp;&nbsp;Irregular Spending: R",
  paste0(scales::comma(irregular, big.mark = " "),
         ifelse(is.finite(irregular_pct),
                paste0(" (", scales::percent(irregular_pct, accuracy = 0.1), ")"),
                " (NA)")),
  "<br>",
  "&nbsp;&nbsp;&nbsp;Wasteful & Fruitless Spending: R",
  paste0(scales::comma(fruitless, big.mark = " "),
         ifelse(is.finite(fruitless_pct),
                paste0(" (", scales::percent(fruitless_pct, accuracy = 0.1), ")"),
                " (NA)")),
  "<br>",
  "<b>UIFWE Receivable - 2025 (% less than 1 year old): R",
  ifelse(is.finite(total_amount.sum) & is.finite(g1_pct),
         paste0(scales::comma(total_amount.sum, big.mark = " "), " (", scales::percent(g1_pct), ")"),
         "0 (NA)"),
"</b><br>"
), htmltools::HTML),
        labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "6px 8px"),
        textsize = "13px",
        direction = "auto"
        )
        ) %>%
        addLegend(
                 pal = colorNumeric(palette = c("#F7F5EF", "#DCE9C4", "#B8C99A", "#8AA870", "#5C8A5A", "#2E4B3C", "#1A2E22"),
                domain = c(0, 0.4)),
                values = c(0, 0.4),
                labFormat = function(type, cuts, p) {
                        labs <- paste0(100 * cuts, "%")
                        labs[length(labs)] <- paste0(100 * cuts[length(labs) - 1], "%+")
                        labs
                        },
                title = "UIFWE as % of Total Expenditure",
                position = "bottomright") %>%
        htmlwidgets::onRender("
    function(el, x) {
      this.getContainer().style.background = '#EDE8DA';
    }
  ")
interact_map


cor(map_df$fruitless_pct, map_df$unauthorised_pct, use = "complete.obs", method = "spearman")



filepath_html <- paste0(output_dir, "/uifwe_plot.html")
htmlwidgets::saveWidget(interact_map, file = filepath_html, selfcontained = TRUE)

filepath_png <- paste0(output_dir, "/uifwe_plot.png")
webshot2::webshot(filepath_html, file = filepath_png, cliprect = "viewport")
}

####
## Datawrapper Table
###

if (TRUE){
datawrapper_table <- map_df_data %>% 
        dplyr::ungroup() %>%
        dplyr::left_join(munic_list %>% dplyr::select(demarcation.code, municipality.province_code), by = "demarcation.code") %>%
        dplyr::select(demarcation.label, municipality.province_code, fruitless, irregular, unauthorised, uifw, fruitless_pct, irregular_pct,unauthorised_pct, uifw_pct) %>%
        dplyr::mutate(across(c(fruitless_pct, irregular_pct, unauthorised_pct, uifw_pct), 
        ~.x * 100, .names = "{.col}")) %>%
        dplyr::rename(Municipality = demarcation.label, 
                        Province = municipality.province_code, 
                        `Fruitless & Wasteful (R)` = fruitless,   
                        `Fruitless & Wasteful (%)` = `fruitless_pct`, 
                        `Unauthorised (R)` = unauthorised,
                        `Unauthorised (%)` = unauthorised_pct,
                        `Irregular (R)` = irregular,
                        `Irregular (%)` = irregular_pct,
                        `Total UIFW (R)` = uifw,
                        `Total UIFW (%)` = uifw_pct) 

csvpath <- paste0(output_dir, "/uifwe.csv")
utils::write.csv(datawrapper_table, csvpath, row.names = FALSE)
}


####
## Scatterplot
####

##Use spearman correlation, show rank of irregular on x and rank of fruitless on y

if (TRUE){
df_scatter_cats <- map_df %>%
        dplyr::ungroup() %>%
        dplyr::filter(dplyr::if_all(c(fruitless_pct, unauthorised_pct, irregular_pct), is.finite)) %>%
        dplyr::mutate(fruitless_rank = rank(fruitless_pct, ties.method = "min"), 
                unauthorised_rank = rank(unauthorised_pct, ties.method = "min"), 
                irregular_rank = rank(irregular_pct, ties.method = "min"))

print(((5104819224/3892332640)^(1/4)) - 1)

# fruitless grew by 7%

print(((23860286443/21072725763)^(1/4)) - 1)
# irregular grew by 3.2%

print(((27618605411/19443720742)^(1/4)) - 1)
# unauthorised grew by 9.1%

print(((611505258077/506183611001)^(1/4)) - 1)
# total expenditure grew by 4.8%

max_rank <- max(df_scatter_cats$fruitless_rank, df_scatter_cats$unauthorised_rank, 
                df_scatter_cats$irregular_rank, na.rm = TRUE)

print(median(map_df$uifw_pct, na.rm = TRUE))
print(median(map_df$fruitless_pct, na.rm = TRUE))
print(median(map_df$irregular_pct, na.rm = TRUE))
print(median(map_df$unauthorised_pct, na.rm = TRUE))




rank_levels <- as.character(1:max_rank)

df_scatter_cats <- df_scatter_cats %>%
  dplyr::mutate(
    fruitless_rank    = factor(fruitless_rank, levels = rank_levels),
    unauthorised_rank = factor(unauthorised_rank, levels = rank_levels),
    irregular_rank    = factor(irregular_rank, levels = rank_levels)
  )

scatters <- list(
  fruit_unauth = list(
    x = "unauthorised_rank", y = "fruitless_rank",
    axistitles = c("Unauthorised Rank", "Fruitless & Wasteful Rank")
  ),
  fruit_irreg = list(
    x = "irregular_rank", y = "fruitless_rank",
    axistitles = c("Irregular Rank", "Fruitless & Wasteful Rank")
  )
)

corrscatters <- list()

for (nm in names(scatters)) {
        spec <- scatters[[nm]]


        scatter_plot <- ggplot(
                data = df_scatter_cats,
        aes(x = .data[[spec$x]], y = .data[[spec$y]])
        ) +
        geom_point(size = 3, colour = "#5C8A5A") +
        scale_x_discrete(
        breaks = rank_levels[seq(1, max_rank, length.out = 5) %>% round()],
        expand = expansion(mult = c(0, 0.03)),
        ) +
        scale_y_discrete(
        breaks = rank_levels[seq(1, max_rank, length.out = 5) %>% round()],
        expand = expansion(mult = c(0, 0.03)),
        ) +
        labs(
        x = spec$axistitles[1],
        y = spec$axistitles[2],
        caption = "substack.com/@onderstromings\n \nSource: National Treasury"
        ) +
        substack_theme +
        theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank()
        )

        corrscatters[[nm]] <- scatter_plot
        }

corrscatters[["fruit_irreg"]]

row(df_scatter_cats)
df_scatter_cats %>% dplyr::filter(!is.na(unauthorised_rank), !is.na(fruitless_rank)) %>% nrow()

class(df_scatter_cats$unauthorised_rank)
levels(df_scatter_cats$unauthorised_rank) %>% head()
table(is.na(df_scatter_cats$unauthorised_rank))


scatterpath <- paste0(output_dir, "/scatter_uifwe.png")

png(filename = scatterpath,
        width = 1200,
        height = 1100,
        res = 190)
print(scatter)
dev.off()

}
####
## Dot Plot
####

if (TRUE){

dotplot_df <- df_uifw %>%
        dplyr::filter(period >= 2024 , municipality.miif_category == "A") %>%
        dplyr::mutate(prepost = ifelse(period == 2025, "post", "pre")) %>%
        dplyr::group_by(demarcation.code, prepost, .drop = "") %>%
        dplyr::summarise(dplyr::across(c(fruitless, irregular, unauthorised, total_exp),
                        ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(uifw = fruitless + irregular + unauthorised, 
                        dplyr::across(c(fruitless, irregular, unauthorised, uifw), 
                        ~.x / total_exp * 100, 
                        .names = "{.col}_pct")
                        ) %>%
        dplyr::select(demarcation.code, prepost, fruitless_pct, irregular_pct, unauthorised_pct, uifw_pct) %>%
        tidyr::pivot_longer(
                cols      = ends_with("_pct"),
                names_to  = "category",
                values_to = "value"
                ) %>%
        tidyr::pivot_wider(names_from = prepost, 
                        values_from = value) %>%
        dplyr::mutate(category_label = dplyr::case_when(
                        stringr::str_detect(category, "fruitless_pct")    ~ "Fruitless & Wasteful",
                        stringr::str_detect(category, "unauthorised_pct") ~ "Unauthorised",
                        stringr::str_detect(category, "irregular_pct")    ~ "Irregular",
                        stringr::str_detect(category, "uifw_pct")         ~ "Total UIFW",
                        TRUE ~ category),
                category_label = factor(category_label, levels = c("Fruitless & Wasteful", "Irregular", "Unauthorised", "Total UIFW")), 
                demarcation.code = factor(demarcation.code, levels = c("BUF", "CPT", "EKH", "ETH", "MAN", "NMA", "TSH")))

city_colour_map <- c(
  CPT = "#2E4B3C",
  EKH = "#A3B86C",
  ETH = "#5C8A5A",
  TSH = "#C9A66B",
  NMA = "#7A6B4F",
  MAN = "#8C5E58",
  BUF = "#4A6B7C"
)


## NOTE 2022/23 not included
dotplots <- list()

for (cat_lab in unique(dotplot_df$category_label)){
        plot_title <- cat_lab

        temp_df <- dotplot_df %>% 
                dplyr::filter(category_label == cat_lab & demarcation.code != "JHB")

        cat <- temp_df %>%
                head(1) %>%
                dplyr::pull(category)

        dotplot <- ggplot(temp_df) +
                geom_segment(aes(x = pre, xend = post, y = demarcation.code, yend = demarcation.code, colour = demarcation.code), 
                                size = 1.5) + 
                geom_point(aes(x = pre, y = demarcation.code, colour = demarcation.code , shape = "2023/24"), size = 4) +
                geom_point(aes(x = post, y = demarcation.code, colour = demarcation.code , shape = "2024/25"), size = 4) +
                scale_colour_manual(values = city_colour_map, guide = "none") +
                scale_shape_manual(name = NULL, values = c("2023/24" = 1, "2024/25" = 16)) +
                labs(title = plot_title,
                        x = "% of Total Expenditure", 
                        y = "Metro") +
                guides(shape = guide_legend(override.aes = list(colour = "black"))) +
                substack_theme + 
                theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                text               = element_text(colour = "#1F2A24"),
                axis.text          = element_text(colour = "#4A4460"),
                axis.text.x        = element_text(margin = margin(t = 0)),
                legend.position    = "bottom",
                legend.key.size    = unit(0.45, "cm"),
                plot.margin        = margin(12, 16, 8, 12), 
                plot.title        = element_text(colour = "#1F2A24", face = "plain", size = rel(1), margin = margin(b = 4), hjust = 0.5),
                ) 

        dotplots[[as.character(cat)]] <- dotplot

}

no_y <- c(2, 4)

for (i in no_y){

        dotplots[[i]] <- dotplots[[i]] +

        theme(axis.text.y  = element_blank(),
          axis.title.y = element_blank())

}

combined_dotplot <- patchwork::wrap_plots(dotplots, nrow = 2) +
                patchwork::plot_layout(axis_titles = "collect", guides = "collect", axes = "collect") +
                patchwork::plot_annotation(title = "Change in UIFWE in Metro's from 2023/24 to 2024/5",
                                caption = "substack.com/@onderstromings\n \nSources: Auditor General SA, National Treasury\nNote: Johannesburg's expenditure data was unavailable at the time of writing.", 
                                theme = substack_theme + theme(axis.title = element_text(face = "bold"), plot.title = element_text(size = rel(1.3))))

dotplot_path <- paste0(output_dir, "/metro_dotplot_reduced.png")

combined_dotplot

png(filename = dotplot_path,
        width = 1137,
        height = 800,
        res = 110)
print(combined_dotplot)
dev.off()



}

