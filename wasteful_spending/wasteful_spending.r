pacman::p_load(dplyr, httr, jsonlite, ggplot2, sf, leaflet, scales)

output_dir <- here::here("wasteful_spending", "output")

ifelse(dir.exists(file.path(output_dir)), print("Path Exists"), 
                dir.create(file.path(output_dir), recursive = TRUE))

substack_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "#F7F5EF", colour = NA),
    panel.background  = element_rect(fill = "#EDE8DA", colour = NA),
    panel.grid.major  = element_line(colour = "#DCE0C8", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    axis.text         = element_text(colour = "#1F2A24"),
    axis.title        = element_text(colour = "#1F2A24"),
    plot.title        = element_text(colour = "#1F2A24", face = "bold"),
    plot.caption      = element_text(colour = "#7A8A6F", size = 9),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    legend.text       = element_text(colour = "#1F2A24"),
    legend.background = element_rect(fill = "#F7F5EF", colour = NA),
    legend.key        = element_rect(fill = "#F7F5EF", colour = NA)
  )

####################################################################################################################################################################################

                                                                     ## MUNICIPAL BASIC DATA ##

####################################################################################################################################################################################
if (TRUE){
census_zip <- here::here("census_sample.zip")

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

rm(hh_wgt, hh_sz, hh_df)
}

####################################################################################################################################################################################

                                                        ## MUNICIPAL FINANCIAL DATA ##

####################################################################################################################################################################################
if (TRUE){
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
        dplyr::rename(period = financial_year_end.year)


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
                aggs <- "total_amount.sum|l30_amount.sum|l60_amount.sum|l90_amount.sum|l120_amount.sum|l150_amount.sum|l180_amount.sum|g1_amount.sum"
                debtor_url <- "https://municipaldata.treasury.gov.za/api/cubes/aged_debtor_v2/aggregate"

                } else {
                aggs <- "total_amount.sum"
                debtor_url <- "https://municipaldata.treasury.gov.za/api/cubes/aged_debtor/aggregate"

                }

                year_df <- fetch_data_debtors(year = yr, url = debtor_url, aggregate = aggs)

                nt_data_debtor[[as.character(yr)]] <- year_df

                message(paste0(yr, " completed"))
                }
}

debt <- bind_rows(nt_data_debtor)

debt <- debt %>%
        dplyr::select(dplyr::any_of(c("customer_group.code", "financial_period.period", "item.code", "demarcation.code", "total_amount.sum")),
                        dplyr::matches("^l"))

model <- httr::GET("https://municipaldata.treasury.gov.za/api/cubes/aged_debtor_v2/model") %>%
  httr::content("text", encoding = "UTF-8") %>%
  jsonlite::fromJSON()

print(names(model))

model$model$

recovery <- pivo
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
        }


wide_list <- list()

for (config in names(configs)){
                cfg <- configs[[config]]

                df_raw_exp <- bind_rows(nt_data_exp[[cfg$string_name]]) %>%
                         dplyr::select(all_of(cfg$selection)) 

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

rm(nt_data_exp, wide_list, df_raw_exp)

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
        dplyr::group_by(period, demarcation.code) %>%
        dplyr::summarise(gross_temp = sum(gross, na.rm = TRUE), 
                        net = sum(net, na.rm = TRUE), 
                        new = sum(new, na.rm = TRUE),
                        .groups = "drop") %>%
        left_join(incexp_df, by = c("demarcation.code", "period")) %>%
        dplyr::filter(stringr::str_detect(demarcation.code, "DC") == FALSE) %>%
        dplyr::mutate(capex = gross_temp, 
                total_exp = capex + opex) %>%
        dplyr::group_by(period, demarcation.code) %>%
        dplyr::summarise(total_exp = sum(total_exp))

df_uifw <- left_join(left_join(df_uifw, munic_list, by = "demarcation.code"), capital_df, by = c("period", "demarcation.code")) %>%
        dplyr::filter(stringr::str_detect(demarcation.code, "DC") == FALSE)
rm(capital_df, capital_full, incexp_full)
}


####################################################################################################################################################################################

                                                        ## CREATE PLOTS ##

####################################################################################################################################################################################
 
uifw_plot_wide <- df_uifw %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(fruitless = sum(fruitless, na.rm = TRUE),
                        irregular = sum(irregular, na.rm = TRUE), 
                        unauthorised = sum(unauthorised, na.rm = TRUE),
                        total_exp = sum(total_exp, na.rm = TRUE)) %>%
        dplyr::mutate(pct = ((fruitless + irregular + unauthorised) / total_exp) * 100) %>%
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
        dplyr::select(-category)

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
                linewidth = 3.2, 
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
                breaks =  c(2018:2025),
                labels = function(x) paste0(x-1, "/", stringr::str_sub(x, start = 3, end = 4)), 
                expand = expansion(mult = c(0, 0))) + 
        scale_linetype_manual(values = c("Percentage of Total Expenditure" = "solid")) +
        scale_colour_manual(values = c("Percentage of Total Expenditure" = "#C9A66B")) +
        labs(title    = "Unauthorised, Irregular, Fruitless & Wasteful Spending\namong Municipalities, 2018 - 2025",
        x        = "Financial Year",
        caption = "Sources: National Treasury") +
        guides(fill = guide_legend(title = NULL, order = 1)) +
        substack_theme
area_plot
## WHAT TO DO ABOUT THE MUNICIPALITIES WHO SAW FUNDING WITHHELD?
