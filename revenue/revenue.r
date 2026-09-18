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


# look <- fetch_data_op_rev(year = 2024, itemcode = "0400")

resp_rev <- httr::GET("https://municipaldata.treasury.gov.za/api/cubes/incexp_v2/members/item")

items_rev <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))

itemcodes_rev <- items$data %>%
        dplyr::select(item.code) %>%
        dplyr::filter(as.integer(item.code) < 2900) %>%
        dplyr::pull(item.code) 

itemcodes__rev_vect <- paste(itemcodes, collapse = '";"')

nt_revenue <- list()

for (year in c(2021:2025)){
        for (budget in c("ORGB", "AUDA")){
            temp_df <- fetch_data_op_rev(year = year,
                                            amount_type = budget, 
                                            itemcode = itemcodes_vect
                                            )

            nt_revenue[[as.character(year)]][[budget]] <- temp_df
            message(paste0(year, ", ", budget))
                                    }
}

df_raw <- c()

for (year in c(2021:2025)){
            temp_df <- bind_rows(nt_revenue[[as.character(year)]])

            df_raw <- rbind(df_raw, temp_df)
}

oprev_df <- df_raw %>%
    dplyr::rename(period = financial_year_end.year) %>%
    tidyr::pivot_wider(id_cols = c(demarcation.code, period), 
                        names_from = c(amount_type.code, item.code),
                        values_from = amount.sum, 
                        values_fn = )


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
        dplyr::filter(as.numeric(item.code) < 2000) %>%
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

for (yr in c(2021:2025)){
                aggs <- "total_amount.sum|badi_amount.sum|bad_amount.sum|l30_amount.sum|l60_amount.sum|l90_amount.sum|l120_amount.sum|l150_amount.sum|l180_amount.sum|g1_amount.sum|l1_amount.sum"
                debtor_url <- "https://municipaldata.treasury.gov.za/api/cubes/aged_debtor_v2/aggregate"

                year_df <- fetch_data_debtors(year = yr, url = debtor_url, aggregate = aggs)

                nt_data_debtor[[as.character(yr)]] <- year_df

                message(paste0(yr, " completed"))
                }


debt <- bind_rows(nt_data_debtor)
colnames(debt)

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
## Join Datasets
###

revenue_df <- dplyr::left_join(left_join(oprev_df, munic_list %>% dplyr::select(demarcation.code, demarcation.label), by = "demarcation.code"), debt_merge, by = c("period", "demarcation.code")) 
