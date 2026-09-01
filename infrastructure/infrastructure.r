pacman::p_load(dplyr, httr, jsonlite, ggplot2, sf, leaflet, scales)

output_dir <- here::here("infrastructure", "output")

ifelse(dir.exists(file.path(output_dir)), print("Path Exists"), 
                dir.create(file.path(output_dir), recursive = TRUE))

####################################################################################################################################################################################

                                                                     ## MUNICIPAL BASIC DATA ##

####################################################################################################################################################################################

census_zip <- here::here("infrastructure", "census_sample.zip")

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


# Compare LIM341 vs LIM343, KZN282 vs KZN285, 

for (m in c("LIM341", "LIM343", "KZN282", "KZN285")){
        pop <- munic_census %>% dplyr::filter(Municipality == m)

        print(paste0(m, " : ", pop))
}


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


province <- munic_list %>%
                dplyr::group_by(municipality.province_code) %>%
                dplyr::summarise(socio = mean(scaled_socio))


## 1) FS 2) NC 3) WC 4) GT 5) MP 6) NW 7) KZ 8) EC 6) LP

####################################################################################################################################################################################

                                                        ## MUNICIPAL INVESTMENT DATA ##

####################################################################################################################################################################################



## Classify capital expenditure by function
## From 1360 onwards there are no clear functions, so classified as "Other"

## Notice that the difference between itra-group items is 10 
## and the difference between groups is 20

## Group 790-1040 was divided, "capital spares" for this group barely features it seems, so grouped with "Other"




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


resp_items <- GET("https://municipaldata.treasury.gov.za/api/cubes/incexp_v2/members/item",
                  query = list(pagesize = 500))

items <- fromJSON(content(resp_items, "text", encoding = "UTF-8"))$data

items %>% filter(stringr::str_detect(tolower(item.label), "capital"))

fetch_data <- function(year, 
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

        bind_rows(all_pages)

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


nt_data <- list()

for (yr in (2018:2025)){
        for (config in names(configs)){
                cfg <- configs[[config]]

                        year_df <- fetch_data(year = yr, url = cfg$url, drilldown = cfg$drilldown)

                        nt_data[[config]][[as.character(yr)]] <- year_df

                message(paste0(config, ", ", yr))
                }
        }


wide_list <- list()
for (config in names(configs)){
                cfg <- configs[[config]]

                df_raw <- bind_rows(nt_data[[cfg$string_name]]) %>%
                         dplyr::select(all_of(cfg$selection)) 

                if (config == "capital"){
                        df_wide <- df_raw %>%
                                tidyr::pivot_wider(id_cols = c(financial_period.period,  item.code, demarcation.code), names_from = capital_type.code, values_from = amount.sum)
                } else {
                        df_wide <- df_raw %>%
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

capital_df <- capital_full %>%
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
        dplyr::group_by(period, demarcation.code, category) %>%
        dplyr::summarise(gross_temp = sum(gross, na.rm = TRUE), 
                        net = sum(net, na.rm = TRUE), 
                        new = sum(new, na.rm = TRUE),
                        .groups = "drop") %>%
        dplyr::mutate(gross_pmax = pmax(gross_temp, 0)) %>% ## Chalk off reversals 
        left_join(incexp_df, by = c("demarcation.code", "period")) %>%
        dplyr::filter(period >= 2023 & stringr::str_detect(demarcation.code, "DC") == FALSE) %>%
        dplyr::group_by(period, demarcation.code) %>%
        dplyr::mutate(capex = sum(gross_pmax, na.rm = TRUE)) %>%
        dplyr::group_by(demarcation.code) %>%
        dplyr::mutate(n_periods_dem = n_distinct(period), 
                total_capex = sum(unique(capex), na.rm = TRUE), 
                capex_ratio = ifelse(total_capex > 0, sum(capex, na.rm = TRUE) / (sum(capex, na.rm = TRUE) + sum(opex, na.rm = TRUE)), NA_integer_ )) 



# When using audited data (2022- ), 774 of 852 obs are available, 
# while for the pre-audited, there are 793 observations. 
##2023 onwards, pre-audited gives 593 of 639 (93%) while audited is 580 (90,7)


####################################################################################################################################################################################

                                                                                 ## Create Indicators ##

####################################################################################################################################################################################

df_service_temp <- left_join(munic_list, capital_df, by = "demarcation.code") %>%
                dplyr::select(-Municipality) %>%
                dplyr::mutate(hh_gross = gross_pmax / hh_n, 
                        hh_net = net / hh_n,
                        hh_new = new / hh_n) %>%
                dplyr::group_by(demarcation.code, category) %>%
                dplyr::summarise(hh_gross_full = sum(hh_gross, na.rm = TRUE),
                                 hh_net_full = sum(hh_net, na.rm = TRUE),
                                 hh_new_full = sum(hh_new, na.rm = TRUE))

df_service_infra <- left_join(left_join(munic_list, capital_df, by = "demarcation.code"), df_service_temp, by = c("demarcation.code", "category")) %>%
        dplyr::select(-gross_pmax, -net, -new) %>%
        dplyr::mutate(group_water = case_when(municipality.miif_category == "A" ~ "A",
                                        municipality.miif_category == "B1" ~ "B1",
                                        municipality.miif_category == "B2" & district.miif_category == "C1" ~ "B2/C1",
                                        municipality.miif_category == "B2" & district.miif_category == "C2" ~ "B2/C2",
                                        municipality.miif_category == "B3" & district.miif_category == "C1" ~ "B3/C1",
                                        municipality.miif_category == "B3" & district.miif_category == "C2" ~ "B3/C2",
                                        municipality.miif_category == "B4" & district.miif_category == "C1" ~ "B4/C1",
                                        municipality.miif_category == "B4" & district.miif_category == "C2" ~ "B4/C2"),
                        flagged = ifelse(capex_ratio > 0.4 | is.na(capex_ratio), 1, 0)) %>%
        dplyr::distinct()


##Compare with similar municipalities (for non-metros, by socio-economic index and MIIF category)
comparison_water <- df_service_infra %>%
        dplyr::group_by(group_water, category) %>%
        dplyr::filter(category == "water_sanitation" & flagged != 1) %>%
        dplyr::mutate(n = n_distinct(demarcation.code)) %>%
        dplyr::summarise(grp_gross_avg = mean(hh_gross_full, na.rm = TRUE),
                grp_net_avg = mean(hh_net_full, na.rm = TRUE), 
                grp_new_avg = mean(hh_new_full, na.rm = TRUE), 
                grp_gross_sd = sd(hh_gross_full, na.rm = TRUE),
                grp_net_sd = sd(hh_net_full, na.rm = TRUE), 
                grp_new_sd = sd(hh_new_full, na.rm = TRUE),
                n_water = mean(n))

comparison_other <- df_service_infra %>%
        dplyr::group_by(municipality.miif_category, category) %>%
        dplyr::filter(category != "water_sanitation" & flagged != 1) %>%
        dplyr::mutate(n = n_distinct(demarcation.code)) %>%
        dplyr::summarise(grp_gross_avg = mean(hh_gross_full, na.rm = TRUE),
                grp_net_avg = mean(hh_net_full, na.rm = TRUE), 
                grp_new_avg = mean(hh_new_full, na.rm = TRUE),
                grp_capex_avg = mean(capex_ratio, na.rm = TRUE),
                grp_gross_sd = sd(hh_gross_full, na.rm = TRUE),
                grp_net_sd = sd(hh_net_full, na.rm = TRUE), 
                grp_new_sd = sd(hh_new_full, na.rm = TRUE), 
                grp_capex_sd = sd(capex_ratio, na.rm = TRUE),
                n = mean(n))


df_water <- df_service_infra %>%
  dplyr::filter(category == "water_sanitation") %>%
  dplyr::left_join(comparison_water, by = c("group_water", "category"))

df_other <- df_service_infra %>%
  dplyr::filter(category != "water_sanitation") %>%
  dplyr::left_join(comparison_other, by = c("municipality.miif_category", "category"))


score <- function(x, mean, sd){
        return((x - mean) / sd)
}

nala <- df_service_infra %>%
        dplyr::filter(demarcation.code == "FS185") %>%
        dplyr::mutate(flagged = 1)

# overstrand <- df_service_infra %>%
#         dplyr::filter(demarcation.label == "WC032")


# "Overstrand" %in% df_service_infra$demarcation.label

ntiles <- 10

df_infra <- bind_rows(df_water, df_other, nala, overstrand) %>%
                dplyr::select(-Municipality) %>%
                dplyr::group_by(group_water) %>%
                tidyr::fill(n_water, .direction = "down") %>%
                ungroup() %>%
                group_by(municipality.miif_category) %>%
                tidyr::fill(n, .direction = "downup") %>%
                ungroup() %>%
                dplyr::mutate(gross_score_temp = case_when(is.na(hh_gross_full) | is.infinite(hh_gross_full) | is.nan(hh_gross_full) ~ 0L,
                                                        TRUE ~ hh_gross_full),
                                net_score = score(hh_net_full, grp_net_avg, grp_net_sd),
                                new_score = score(hh_new_full, grp_new_avg, grp_new_sd), 
                                pct_new = hh_new_full / hh_gross_full, 
                                capex_score = score(capex_ratio, grp_capex_avg, grp_capex_sd), 
                                gross_score = suppressWarnings(as.numeric(hh_gross_full))) %>%
                dplyr::group_by(category) %>%
                dplyr::mutate(gross_group = case_when(flagged == 1 & total_capex == NA_integer_ ~ NA_integer_, 
                                                        is.na(hh_gross_full) | is.infinite(hh_gross_full) | is.nan(hh_gross_full) ~ 0L,
                                                        is.na(gross_score_temp) | is.infinite(gross_score_temp) | is.nan(gross_score_temp) ~ 0L,
                                                         TRUE ~ ifelse(gross_score > 0, as.integer(ntile(gross_score_temp, ntiles)), 0))) %>%
                dplyr::ungroup() 

df_groups <- df_infra %>%
                dplyr::group_by(demarcation.code, category) %>%
                dplyr::distinct(demarcation.code, .keep_all = TRUE) %>%
                tidyr::pivot_wider(id_cols = c(demarcation.code, group_water), names_from = category, values_from = c(gross_score, gross_group), names_glue = "{.value}_{category}") %>%
                dplyr::mutate(gross_group_water_sanitation = case_when(stringr::str_detect(group_water, "C2") ~ NA_integer_,
                                                                TRUE ~ gross_group_water_sanitation))


df_data <- left_join(df_infra, df_groups %>% dplyr::select(-group_water), by = "demarcation.code") %>%
                select(demarcation.label, all_of(colnames(df_groups)), capex_ratio, capex_score, hh_n, capex, group_water, total_capex, flagged) %>%
                dplyr::distinct(demarcation.code, .keep_all = TRUE) %>%
                # mutate(across(c(gross_group_electricity, gross_group_solid_waste, gross_group_road_transport,
                #         gross_score_electricity, gross_score_solid_waste, gross_score_road_transport),
                #  ~ dplyr::case_when(is.na(.x) | is.infinite(.x) | is.nan(.x) ~ 0L, TRUE ~ .x))) %>%
                dplyr::mutate(capex_score = rowMeans(across(all_of(c("gross_group_electricity", "gross_group_water_sanitation", "gross_group_road_transport", "gross_group_solid_waste"))), na.rm = TRUE))


# infra_province <- df_data %>%
#                 dplyr::group_by(municipality.province_code) %>%
#                 dplyr::filter(category %in% c("solid_waste", "water_sanitation", "electricity")) %>%
#                 dplyr::summarise(score = mean(gross_score, na.rm = TRUE))


####################################################################################################################################################################################

                                                                                 ## Obtain map from municipal demarcation board ##

####################################################################################################################################################################################

## Use 2021 demarcations

map_resp <- httr::GET("https://services7.arcgis.com/oeoyTUJC8HEeYsRB/arcgis/rest/services/LocalMunicipalities2018_Final/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")

writeLines(httr::content(map_resp, "text", encoding = "UTF-8"), here::here("data", "munic_map.geojson"))

map <- sf::st_read(here::here("data", "munic_map.geojson")) %>%
                dplyr::select(CAT_B, DATE, Shape__Area, Shape__Length, geometry) %>%
                dplyr::rename(demarcation.code = CAT_B)

df_plot <- left_join(df_data, map, by = "demarcation.code") 

pal <- colorNumeric(palette = c("#F7F5EF", "#DCE9C4", "#B8C99A", "#8AA870", "#5C8A5A", "#2E4B3C", "#1A2E22"),
                domain = df_plot$capex_ratio[df_plot$flagged != 1], 
                na.color = "#9E9E9E")

df_plot <- df_plot %>%
  mutate(fill_colour = ifelse(flagged == 1 | is.na(capex_ratio), "#9E9E9E", pal(capex_ratio)))



tag.map.title <- htmltools::tags$style(htmltools::HTML("
                .leaflet-control.map-title {
                transform: translate(50%,50%);
                position: fixed !important;
                left: 10px;
                text-align: left;
                padding-left: 10px;
                padding-right: 10px;
                background: transparent;
                font-weight: bold;
                font-size: 28px;
                }
                "))

title <- htmltools::tags$div(
  tag.map.title, htmltools::HTML("Capital Spending by Municipality", "<br>", "2022-2025"))

gross_cols <- colnames(df_plot[stringr::str_detect(colnames(df_plot), "gross_score_")])
categories <- c("capex_score", gross_cols)
categories <- categories[categories != "gross_score_NA"]

print(categories)
####################################################################################################################################################################################

                                                                                 ## Generate Output ##

####################################################################################################################################################################################


if (TRUE){
interact_map <- leaflet(
                df_plot %>%
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
        "<b>", ifelse(flagged != 1 & demarcation.code != "FS185", paste0(demarcation.label), paste0(demarcation.label, "*")), "</b><br>",
        "Group: ", group_water, "<br>",
        "Households: ", scales::comma(round(hh_n, digits = 0)), "<br>",
        "Infrastructure Spending : ", ifelse(demarcation.code != "FS185" & total_capex != 0,  
                        paste0(scales::percent(capex_ratio, accuracy = 0.1), " (R", scales::comma(total_capex, big.mark = " "), ")"), "NA"),"<br>",
        "Electricity : ", ifelse(demarcation.code != "FS185" & flagged != 1 & total_capex != 0 & !is.na(gross_group_electricity), 
                        paste0("R ", scales::comma(round(gross_score_electricity, 2), big.mark = " "), "/HH (", gross_group_electricity, "/", ntiles, ")"), "NA"),   "<br>",
        "Water & Sanitation : ", ifelse(flagged != 1 & total_capex != 0 & !stringr::str_detect(group_water, "C2"), 
                        paste0("R ", scales::comma(round(gross_score_water_sanitation, 2), big.mark = " "), "/HH (", gross_group_water_sanitation, "/", ntiles, ")"), "NA"),   "<br>",
        "Solid Waste : ", ifelse(demarcation.code != "FS185" & flagged != 1 & total_capex != 0 & !is.na(gross_group_solid_waste), 
                        paste0("R ", scales::comma(round(gross_score_solid_waste, 2), big.mark = " "), "/HH (", gross_group_solid_waste, "/", ntiles, ")"), "NA"),   "<br>",
        "Roads : ", ifelse(demarcation.code != "FS185" & flagged != 1 & total_capex != 0 & !is.na(gross_group_road_transport), 
                        paste0("R ", scales::comma(round(gross_score_road_transport, 2), big.mark = " "), "/HH (", gross_group_road_transport, "/", ntiles, ")"), "NA"),   "<br>",
        "Overall : ", ifelse(demarcation.code != "FS185" & flagged != 1 & total_capex != 0 & !is.na(capex_score), 
                        paste0(round(capex_score, 2), "/", ntiles), "NA"),   "<br>"
        ),
        htmltools::HTML
        ),
        labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "6px 8px"),
        textsize = "13px",
        direction = "auto"
        )
        ) %>%
        addLegend(
                 pal = colorNumeric(palette = c("#F7F5EF", "#DCE9C4", "#B8C99A", "#8AA870", "#5C8A5A", "#2E4B3C", "#1A2E22"),
                domain = df_plot$capex_ratio[df_plot$flagged != 1]),
                values = df_plot$capex_ratio[df_plot$flagged != 1 & df_plot$demarcation.code != "FS185"],
                title = "Infrastructure Spending Ratio",
                position = "bottomright") %>%
        htmlwidgets::onRender("
    function(el, x) {
      this.getContainer().style.background = '#EDE8DA';
    }
  ")
interact_map


filepath_html <- paste0(output_dir, "/capex_plot.html")
htmlwidgets::saveWidget(interact_map, file = filepath_html, selfcontained = TRUE)

filepath_png <- paste0(output_dir, "/capex_plot.png")
webshot2::webshot(filepath_html, file = filepath_png, cliprect = "viewport")

# img <- magick::image_read(filepath_png)

# img_annotated <- magick::image_annotate(img, 
#                                         text = "Infrastructure Spending by Municipality\n2022 - 2025",
#                                         size = 30, 
#                                         gravity = "north", 
#                                         location = "+0+20",
#                                         degrees = 0,
#                                         font = "sans",
#                                         color = "black"
#                                         )

# magick::image_write(img_annotated, path = filepath_png, format = "png")

xl_path <- paste0(output_dir, "/capital_ranks.xlsx")
xl_sheets <- list()

for (cat in categories){

        sheetname <- stringr::str_sub(cat, start = 1, end = 31)
        df_top <- df_plot %>%
                dplyr::filter(flagged == 0) %>%
                dplyr::select(demarcation.label, demarcation.code, dplyr::all_of(cat), gross_group_electricity, gross_group_solid_waste, gross_group_water_sanitation, gross_group_road_transport) %>%
                arrange(desc(.data[[cat]])) %>%
                head(8) %>%
                dplyr::mutate(`Province` = stringr::str_sub(demarcation.code, start = 1, end = -4),
                        index = row_number()) %>%
                dplyr::rename(` ` = index, 
                                `Municipality` = demarcation.label, 
                                `Overall Score` = !!cat, 
                                `Electricity` = gross_group_electricity,
                                `Solid Waste` = gross_group_solid_waste, 
                                `Water & Sanitation` = gross_group_water_sanitation,
                                `Roads` = gross_group_road_transport) %>%
                dplyr::select(` `, `Municipality`, `Province`, `Overall Score`, `Electricity`, `Solid Waste`, `Water & Sanitation`, `Roads`)
        
        df_bottom<- df_plot %>%
                dplyr::filter(flagged == 0) %>%
                dplyr::select(demarcation.label, demarcation.code, dplyr::all_of(cat), gross_group_electricity, gross_group_solid_waste, gross_group_water_sanitation, gross_group_road_transport) %>%
                arrange(desc(.data[[cat]])) %>%
                tail(8) %>%
                dplyr::mutate(`Province` = stringr::str_sub(demarcation.code, start = 1, end = -4),
                        index = row_number()) %>%
                dplyr::rename(` ` = index, 
                                `Municipality` = demarcation.label, 
                                `Overall Score` = !!cat, 
                                `Electricity` = gross_group_electricity,
                                `Solid Waste` = gross_group_solid_waste, 
                                `Water & Sanitation` = gross_group_water_sanitation,
                                `Roads` = gross_group_road_transport) %>%
                dplyr::select(` `, `Municipality`, `Province`, `Overall Score`, `Electricity`, `Solid Waste`, `Water & Sanitation`, `Roads`)
        

        df_rank <- bind_rows(df_top, df_bottom)
        xl_sheets[[sheetname]] <- df_rank
}

writexl::write_xlsx(xl_sheets, path = xl_path)


cat_colours <- c(
  "Electricity"        = "#2E4B3C",
  "Water & Sanitation" = "#A3B86C",
  "Roads"              = "#5C8A5A",
  "Solid Waste"        = "#C9A66B"
)

barplot_df <- df_plot %>%
  dplyr::filter(flagged != 1 & group_water == "A") %>%
  tidyr::pivot_longer(
    cols      = starts_with("gross_score_"),
    names_to  = "category",
    names_prefix = "gross_score_",
    values_to = "gross_score"
  ) %>%
  filter(category %in% c("electricity", "solid_waste", "water_sanitation", "road_transport")) %>%
  mutate(
    category_label = recode(category,
      electricity      = "Electricity",
      solid_waste      = "Solid Waste",
      water_sanitation = "Water & Sanitation",
      road_transport   = "Roads"
    ),
    city = recode(demarcation.code,
      CPT = "Cape Town",      ETH = "eThekwini",
      JHB = "Johannesburg",   NMA = "Nelson Mandela Bay",
      TSH = "Tshwane",        BUF = "Buffalo City",
      MAN = "Mangaung", EKU = "Ekurhuleni"
    )
  )

barplot <- ggplot(barplot_df, aes(x = gross_score,
                    y = reorder(city, gross_score),
                    fill = category_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = cat_colours) +
  scale_x_continuous(
  breaks = scales::breaks_width(1000),
  minor_breaks = scales::breaks_width(250),
  labels = scales::comma,
  limits = c(0, NA),
  expand = expansion(mult = c(0.001, 0.05))
) +
  labs(
    title    = "Infrastructure Spending by Metro, 2023 - 2025",
    x        = "Rands per Household",
    y        = NULL,
    fill     = NULL,
    caption = "Sources: National Treasury, Statistics South Africa"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#F5F3F0", colour = NA),
    panel.background = element_rect(fill = "#F5F3F0", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_line(colour = "##DCE0C8", linewidth = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    text               = element_text(colour = "#1F2A24"),
    axis.text          = element_text(colour = "#4A4460"),
    axis.text.x        = element_text(margin = margin(t = 0)),
    legend.position    = "bottom",
    legend.key.size    = unit(0.45, "cm"),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "#7A8A6F", size = 9),
    plot.margin        = margin(12, 16, 8, 12),
    plot.caption = element_text(hjust = 0, size = 8, colour = "#7A8A6F")
  )

barpath <- paste0(output_dir, "/metro_bar.png")

png(filename = barpath,
        width = 1137,
        height = 650,
        res = 190)
print(barplot)
dev.off()

}

print(mean(df_plot$capex_ratio, na.rm = TRUE))

show <- df_plot[!is.na(df_plot$capex_ratio),] %>%
        dplyr::filter(flagged != 1 & group_water == "A") %>%
        dplyr::group_by(demarcation.code) %>%
        dplyr::select(demarcation.code, capex_ratio, gross_score_electricity, gross_score_road_transport, gross_score_solid_waste, gross_score_water_sanitation)

mean(show$capex_ratio)






check <- capital_df %>%
        group_by(demarcation.code, period) %>%
        dplyr::summarise(gross = sum(gross_pmax, na.rm = TRUE),
                        pct = mean(capex_ratio, na.rm = TRUE)) %>%
        dplyr::filter(demarcation.code == "ETH") %>%
        arrange(period) %>%
        dplyr::mutate(change = (gross - lag(gross))/ lag(gross) * 100)
print(check)


print(nrow(df_plot[(df_plot$capex_ratio < 0.05 & df_plot$flagged == 0), ]))





sum(df_plot[!(stringr::str_detect(df_plot$group_water, "C2") & df_plot$flagged == 0), ]$gross_score_water_sanitation == 0, na.rm = TRUE)

