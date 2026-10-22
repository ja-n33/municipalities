pacman::p_load(dplyr, httr, jsonlite, ggplot2, sf, leaflet, scales, cowplot, magick, patchwork)

output_dir <- here::here("grants", "output")

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
        dplyr::select(QID, HH_WGT, H05_WATERPIPED, H09_ENERGY_COOKING, H01_QUARTERS) %>%
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

rm(districts_df, hh_df, hh_sz, hh_wgt, munic_labels, munic_meta, munic_names_temp, munic_qid)


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

}


####################################################################################################################################################################################

                                                        ## MUNICIPAL FINANCIAL DATA ##

####################################################################################################################################################################################

if (TRUE){

drilldown_grants <- paste("amount_type.code", 
                        "financial_year_end.year",
                        # "function.category_label",
                        # "function.code",
                        # "function.label", ##the  function breakdown adds a lot of additinal complexity
                        "grant.code", 
                        "grant.label",
                        "demarcation.code",
                        "period_length.length",  sep  = "|")

drilldown_grants_act <- paste("amount_type.code",
                          "financial_year_end.year",
                          "grant.code",
                          "grant.label",
                          "demarcation.code", sep = "|")


fetch_data_grants <- function(year = 2025, 
                        amount_type = "AUDA", 
                        url = "https://municipaldata.treasury.gov.za/api/cubes/grants_v2/aggregate", 
                        drilldown = drilldown_grants,
                        pagesize = 5000, 
                        period_length = "year", 
                        page = 1, 
                        grantcode = item 
                        ){

  base_url <- url
  
  cut <- sprintf('financial_year_end.year:%d|period_length.length:"%s"|grant.code:"%s"|amount_type.code:"%s"', year, period_length, grantcode, amount_type)

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
                                col_types = readr::cols(grant.code = readr::col_character(),
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


resp_grants <- httr::GET("https://municipaldata.treasury.gov.za/api/cubes/grants_v2/members/grant")

items_grants <- jsonlite::fromJSON(httr::content(resp_grants, "text", encoding = "UTF-8"))

grantnames <- items_grants$data[, c("grant.code", "grant.label")]

itemcodes_grants <- items_grants$data %>%
        dplyr::select(grant.code) %>%
        dplyr::filter(!(grant.code %in% c("ACNG", "AHCG", "WCHG", "CSG"))) %>%
        dplyr::pull(grant.code) 

itemcodes_grants_vect <- paste(itemcodes_grants, collapse = '";"')

nt_grants <- lapply(setNames(nm = as.character(2021:2025)),
                    function(y) list(ORGB = NULL, ADJB = NULL, ACT = NULL))

for (year in c(2021:2024)){
        for (budget in c("ORGB", "ADJB")){
            temp_df <- fetch_data_grants(year = year,
                                        period_length = if (budget != "ACT") "year" else "month",
                                        drilldown = if (budget != "ACT") drilldown_grants else drilldown_grants_act,
                                        amount_type = budget, 
                                        grantcode = itemcodes_grants_vect)

                temp_df <- temp_df %>%
                        dplyr::rename(period = financial_year_end.year) %>%
                        dplyr::mutate(period = as.character(period)) %>%
                        dplyr::select(-c("grant.code"))

            nt_grants[[as.character(year)]][[budget]] <- temp_df
            message(paste0(year, ", ", budget))
                                    }
}

drop_rows <- c("Total", "R thousands", "Summary by Category of Municipality",
               "Category classification", "Category A", "Category B", "Category C",
               "Unallocated", "Summary by Provincial Departments",
               "Transfers by Provincial Departments to Municipalities( Agency services)")


data_dir <- "/Users/jansmuts/Desktop/PersonalProjects/municipalities/data/"
msoff    <- "/opt/anaconda3/bin/msoffcrypto-tool"

prep_file <- function(path) {
  readable <- tryCatch({ readxl::excel_sheets(path); TRUE }, error = function(e) FALSE)
  if (readable) return(path)

  tmp <- tempfile()
  system2(msoff, c("-p", "VelvetSweatshop", shQuote(path), shQuote(tmp)))
  src <- if (file.exists(tmp)) tmp else path   # not encrypted, just wrong extension
  out <- paste0(tools::file_path_sans_ext(path), "_dec.",
                readxl::format_from_signature(src))
  file.copy(src, out, overwrite = TRUE)
  out
}

for (period in c("2021.xls", "2122.xlsx", "2223.xlsx", "2324.xlsx", "2425.xlsx")){

        for (demcode in unique(munic_list$demarcation.code)){

                fy_period <- paste0("20", stringr::str_sub(period, start = 3, end = 4))

                raw_path <- paste0("/Users/jansmuts/Desktop/PersonalProjects/municipalities/data/ccg_", period)

                xl_path <- prep_file(raw_path)

                xl_file <- readxl::read_excel(path = xl_path, sheet = demcode, range = "A7:Q96") #%>%
                                
                xl_file$demarcation.code <-  demcode
        
                xl_df <- xl_file %>%
                        dplyr::rename(grant.label = `R thousands`, 
                                        amount_ORGB = dplyr::starts_with("Division of revenue Act"), 
                                        amount_ADJB = dplyr::starts_with("Total Available"), 
                                        amount_ACT = 17) %>%
                        dplyr::select(demarcation.code, grant.label, amount_ORGB, amount_ADJB, amount_ACT) %>%
                        dplyr::mutate(grant.label = dplyr::case_when(
                                        grant.label == "Programme and Project Preperation Support Grant"      ~ "Programme and project preparation support grant",
                                        grant.label == "Smart Meter Grant (Schedule 6B)"                      ~ "Smart Meter Grant",
                                        grant.label == "Municipal Systems Improvement Grant (Schedule 5B)"    ~ "Municipal Systems Improvement Grant",
                                        grant.label == "Municipal Systems Improvement Grant (Schedule 6B)"    ~ "Municipal Systems Improvement Grant (Sched 6)",
                                        grant.label == "Water Services Infrastructure Grant (Schedule 5B)"    ~ "Water Services Infrastructure Grant",
                                        grant.label == "Water Services Infrastructure Grant (Schedule 6B)"    ~ "Water Services Infrastructure Grant (Sched 6)",
                                        grant.label == "Municipal Infrastructure Grant (Schedule 6B)"         ~ "Municipal Infrastructure Grant (Indirect)",
                                        grant.label == "Informal Settlements Upgrading Partnership Grant (Schedule 5B)" ~ "Urban Settlement Development Grant",
                                        stringr::str_detect(grant.label, "^Regional Bulk Infrastructure Grant")     ~ "Regional Bulk Infrastructure Grant",
                                        stringr::str_detect(grant.label, "^Municipal Demarcation Transition Grant") ~ "Municipal Demarcation Transition Grant",
                                        grant.label %in% drop_rows ~ NA_character_,
                                        TRUE ~ grant.label)) %>%
                        dplyr::filter(!is.na(grant.label), !(stringr::str_detect(grant.label, "Sub-Total|\\(Vote|NA"))) %>%
                        dplyr::group_by(grant.label, demarcation.code) %>%
                        dplyr::summarise(dplyr::across(c(amount_ORGB, amount_ADJB, amount_ACT), 
                                                ~ sum(as.numeric(.x), na.rm = TRUE)), 
                                                .groups = "drop")

                xl_df$period <-  fy_period

                nt_grants[[fy_period]][["ACT"]] <- rbind(nt_grants[[fy_period]][["ACT"]], xl_df)
                message(paste0("Year: ", fy_period, " DC: ", demcode))
        }
}


nt_grants[["2024"]][["ACT"]]

df_grants <- c()

for (year in names(nt_grants)){

        if (year != "2025"){
        orgb <- nt_grants[[year]][["ORGB"]] %>%
                dplyr::summarise(amount_ORGB = sum(as.numeric(amount.sum), na.rm = TRUE), .by = c("demarcation.code", "grant.label"))

        adjb <- nt_grants[[year]][["ADJB"]] %>%
                dplyr::summarise(amount_ADJB = sum(as.numeric(amount.sum), na.rm = TRUE), .by = c("demarcation.code", "grant.label"))

        act  <- nt_grants[[year]][["ACT"]] %>%
                dplyr::select(c("demarcation.code", "grant.label"), amount_ACT)

        temp_df <- orgb %>%
                dplyr::full_join(adjb, by = c("demarcation.code", "grant.label")) %>%
                dplyr::full_join(act,  by = c("demarcation.code", "grant.label")) %>%
                dplyr::mutate(period = as.character(year))

        temp_df <- temp_df %>%
                dplyr::mutate(grant.label = dplyr::case_when(
                                stringr::str_detect(grant.label, "^Regional Bulk Infrastructure Grant")     ~ "Regional Bulk Infrastructure Grant",
                                stringr::str_detect(grant.label, "^Municipal Demarcation Transition Grant") ~ "Municipal Demarcation Transition Grant",
                                TRUE ~ grant.label
                )) %>%
                dplyr::filter(demarcation.code %in% munic_list$demarcation.code) %>%
                dplyr::group_by(demarcation.code, grant.label, period) %>%
                dplyr::summarise(dplyr::across(c(amount_ORGB, amount_ADJB, amount_ACT), 
                                        ~ sum(as.numeric(.x), na.rm = TRUE)), 
                                        .groups = "drop")
        } else {
                temp_df  <- nt_grants[[year]][["ACT"]] %>%
                        dplyr::select(demarcation.code, grant.label, period, amount_ORGB, amount_ADJB, amount_ACT) 

                lges_df <-  readr::read_csv(here::here("data", "lges_2025.csv")) 

                lges_df$`grant.label` <- "Local Government Equitable Share"

                lges_df <- lges_df %>%
                                dplyr::mutate(amount_ORGB = amount_ADJB,
                                        amount_ACT = amount_ADJB, 
                                        period = as.character(year)) 

                temp_df <- dplyr::bind_rows(temp_df, lges_df)

                temp_df <- temp_df %>%
                        dplyr::filter(demarcation.code %in% munic_list$demarcation.code) 

        }

        df_grants <- dplyr::bind_rows(df_grants, temp_df)
}


departments <- c("Health",
                "Education", 
                "Social Development", 
                "Public Works, Roads and Transport",
                "Agriculture",
                "Sport, Arts and Culture", 
                "Housing and Local Government", 
                "Office of the Premier", 
                "Other Departments")

totals_df <- df_grants %>%
        dplyr::filter(!(grant.label %in% departments)) %>%
        dplyr::group_by(demarcation.code, period) %>%
        dplyr::summarise(dplyr::across(c(amount_ORGB, amount_ADJB, amount_ACT), 
                        ~sum(.x, na.rm = TRUE))) %>%
        dplyr::mutate(grant.label = "total_grants")

df_grants <- rbind(df_grants, totals_df)

####
## Revenue Data
####


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
items_rev$data
itemcodes_rev <- items_rev$data %>%
        dplyr::select(item.code) %>%
        dplyr::filter(as.integer(item.code) %in% c(2200, 2900, 3900, 4600, 4700)) %>%
        dplyr::pull(item.code) 

itemcodes_rev_full <- items_rev$data %>%
        dplyr::select(item.code) %>%
        dplyr::filter(as.integer(item.code) < 2900 | as.integer(item.code) %in% c(3900, 4600, 4700)) %>%
        dplyr::pull(item.code) 


collapsed <- lapply(list(itemcodes_rev = itemcodes_rev, 
                        itemcodes_rev_full = itemcodes_rev_full), function(x) paste(x, collapse = '";"'))

print(collapsed$itemcodes_rev)

code_cols <- c("1550","1570", "1590", stringr::str_pad(as.integer(seq(0, 2800, by = 100)), width = 4, pad = "0"))

nt_revenue <- list()

for (year in c(2021:2025)){

        # if (year != 2021){
        #     temp_df <- fetch_data_op_rev(year = year,
        #                                     amount_type = "AUDA", 
        #                                     itemcode = collapsed$itemcodes_rev
        #                                     )
        # }
        # else{
              temp_df <- fetch_data_op_rev(year = year,
                                            amount_type = "AUDA", 
                                            itemcode = collapsed$itemcodes_rev_full
                                            ) %>%
                        tidyr::pivot_wider(id_cols = c(demarcation.code, financial_year_end.year),
                                        names_from = item.code, 
                                        values_from = amount.sum) %>%
                        dplyr::mutate(`2900` = rowSums(dplyr::across(dplyr::any_of(code_cols)), na.rm = TRUE)) %>%
                        dplyr::select(demarcation.code, financial_year_end.year, `2200`, `2900`, `4600`, `4700`) %>%
                        tidyr::pivot_longer(!c(demarcation.code, financial_year_end.year), 
                                        names_to = "item.code", 
                                        values_to = "amount.sum") %>%
                        dplyr::left_join(items_rev$data %>% dplyr::select(item.code, item.label), by = "item.code") #%>%
                        # tidyr::pivot_wider(id_cols = c(demarcation.code, financial_year_end.year),
                        #                 names_from = item.label, 
                        #                 values_from = amount.sum)

        # }
            nt_revenue[[as.character(year)]] <- temp_df
            message(paste0(year))
                                    }

temp_rev <- dplyr::bind_rows(nt_revenue) 
 
check <- nt_revenue[["2021"]]

rev_df <- temp_rev %>%
        dplyr::rename(period = financial_year_end.year) %>%
        dplyr::mutate(period = as.character(period)) %>%
        dplyr::filter(demarcation.code %in% munic_list$demarcation.code) %>%
        tidyr::pivot_wider(id_cols = c(demarcation.code, period), 
                                names_from = item.label,
                                values_from = amount.sum) %>%
        dplyr::rename_with( ~ janitor::make_clean_names(.x), .cols = 2:last_col()) %>%
        dplyr::mutate(total_revenue =  dplyr::coalesce(total_revenue_excluding_capital_transfers_and_contributions, 0) +
                                        dplyr::coalesce(transfers_and_subsidies_capital_monetary_allocations, 0) +
                                        dplyr::coalesce(transfers_and_subsidies_capital_in_kind, 0)) %>%
        dplyr::select(demarcation.code, period, total_revenue, total_revenue_excluding_capital_transfers_and_contributions) %>%
        tidyr::pivot_longer(!c(demarcation.code, period),
                        names_to = "label", 
                        values_to = "amount_ADJB") 

grants_df <- dplyr::bind_rows(df_grants %>% dplyr::rename(label = grant.label) %>% dplyr::mutate(period = as.character(period)), rev_df) %>%
                dplyr::filter(as.numeric(period) >= 2021) %>%
                dplyr::left_join(dplyr::left_join(munic_list %>% dplyr::select(demarcation.code, demarcation.label, municipality.miif_category, district.miif_category, hh_n, hh_socio), by = "demarcation.code"), df_cpi %>% dplyr::mutate(period = as.character(period)), by = "period")  %>%
                dplyr::mutate(revenue_group = case_when(municipality.miif_category == "A" ~ "A",
                                        municipality.miif_category == "B1" ~ "B1",
                                        municipality.miif_category == "B2" & district.miif_category == "C1" ~ "B2/C1",
                                        municipality.miif_category == "B2" & district.miif_category == "C2" ~ "B2/C2",
                                        municipality.miif_category == "B3" & district.miif_category == "C1" ~ "B3/C1",
                                        municipality.miif_category == "B3" & district.miif_category == "C2" ~ "B3/C2",
                                        municipality.miif_category == "B4" & district.miif_category == "C1" ~ "B4/C1",
                                        municipality.miif_category == "B4" & district.miif_category == "C2" ~ "B4/C2"), 
                                hh_pov = hh_n - hh_socio) %>%
                dplyr::select(!c(municipality.miif_category, district.miif_category))

utils::write.csv(grants_df, here::here("data", "grants_df.csv"))

}

##for 24/25, use the DoRA column as the original amount and total available as the adjusted

####################################################################################################################################################################################

                                                        ## PLOT DATA ##

####################################################################################################################################################################################

grants_df <- readr::read_csv(here::here("data", "grants_df.csv"))

print(67895600000 / 104236806000)

print( (4515194000 +10530141000) / (119036925000 - 80346307000))

bar_df <- grants_df %>%
        dplyr::mutate(period = as.character(period)) %>%
        dplyr::filter(!(label %in% c("total_revenue_excluding_capital_transfers_and_contributions", "total_transfers", "share", "total_grants", departments, "total_revenue"))) %>%
        dplyr::select(period, amount_ADJB, label) %>%
        dplyr::group_by(period, label) %>%
        dplyr::summarise(amount_ADJB = sum(amount_ADJB, na.rm = TRUE)) %>%
        dplyr::rename(type = label,
                value = amount_ADJB) 

barsum_df <- bar_df %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(total_transfers = sum(value, na.rm = TRUE)) %>%
        dplyr::left_join(df_cpi %>% dplyr::mutate(period = as.character(period)), by = "period") %>%
        dplyr::mutate(transfers_inf = total_transfers / cpi_indexed, 
                        transfers_indexed = transfers_inf / transfers_inf[period == "2021"] * 100) %>%
        dplyr::select(period, transfers_indexed) %>%
        tidyr::pivot_longer(-period, 
                            names_to = "type", 
                            values_to = "value")

top_items <- bar_df %>%
        dplyr::filter(period == 2025) %>%
        dplyr::arrange(desc(value)) %>%
        head(4) %>%
        pull(type)

items <- c(unlist(top_items), "Other")

print(top_items)

item_colours <- c(
                "#2E4B3C", 
                "#A3B86C",
                "#5C8A5A",
                "#C9A66B",
                "#7A6B4F"
)

colour_map <- setNames(item_colours, items)

bar_df <- bar_df %>%
        dplyr::mutate(type = ifelse(type %in% top_items, type, "Other"), 
                        period = as.numeric(period)) %>%
        dplyr::group_by(period, type) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(type = factor(type, levels = items)) %>%
        dplyr::bind_rows(barsum_df %>% dplyr::mutate(period = as.numeric(period))) %>%
        dplyr::arrange(period) %>%
        dplyr::group_by(type) 

primary_lo <- 0   # position for index = 25
primary_hi <- 125e9   # position for index = 125

axis_fact  <- (primary_hi - primary_lo) / (110 - 80)
axis_offset <- primary_lo - axis_fact * 80

area_plot <- ggplot(data = bar_df, aes(x = period, y = value, fill = type)) + 
        geom_area(linewidth = 4, 
                outline.type = "full",
                position = position_stack(reverse = TRUE), 
                aes(fill = type)) +
        geom_line(data = bar_df %>% dplyr::filter(type == "transfers_indexed"), 
                    aes(x = period, y = axis_offset + value * axis_fact), 
                    linewidth = 1.5, 
                    linetype = "dashed") + 
        scale_fill_manual(values = colour_map) +
        scale_y_continuous(name = "Amount (R billion)", 
                breaks = scales::breaks_pretty(n = 6),
                labels  = function(x) ifelse(x == 0, "", scales::label_number(scale = 1e-9, big.mark = ",")(x)), 
                expand = expansion(mult = c(0.001, 0.05)), 
                sec.axis = sec_axis(~ (. - axis_offset) / axis_fact, 
                        name = "Transfers Index (2020/21 = 100)", 
                        breaks = seq(80, 110, by = 5),
                        labels  = function(x) ifelse(x == 80, "", x))) +
        scale_x_continuous(name = "Financial Year", 
                breaks =  c(2020:2025),
                labels = function(x) paste0(x-1, "/", stringr::str_sub(x, start = 3, end = 4)), 
                expand = expansion(mult = c(0, 0.025))) + 
        labs(title    = "Municipal Transfers,  2020/21 - 2024/25",
        x        = "Financial Year",
        caption = "substack.com/@onderstromings\n \nSource: EconData, National Treasury") +
        guides(fill = guide_legend(title = NULL, order = 1, nrow = 2)) +
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

####
## dumbbell plots showing change across categories
###

dumbbell_df <- grants_df %>%
        dplyr::select(demarcation.code, period, revenue_group, label,amount_ADJB) %>%
        dplyr::filter(label %in% c("total_grants", "total_revenue", "Local Government Equitable Share")) %>%
        tidyr::pivot_wider(id_cols = c(revenue_group, demarcation.code, period), 
                                names_from = label, 
                                values_from = amount_ADJB) %>%
        dplyr::group_by(demarcation.code, revenue_group, period) %>%
        dplyr::summarise(dplyr::across(c(total_grants, total_revenue, `Local Government Equitable Share`), ~sum(.x, na.rm = TRUE))) %>%
        dplyr::filter(total_revenue > total_grants &  total_revenue > `Local Government Equitable Share` & dplyr::if_all(c(total_grants, total_revenue, `Local Government Equitable Share`), ~ !is.na(.x) & .x > 0)) %>%
        dplyr::mutate(share_total = total_grants / total_revenue, 
                        share_lges = `Local Government Equitable Share` / total_revenue) %>%
        
        dplyr::filter(period %in% c("2021", "2025")) %>%
        dplyr::group_by(demarcation.code) %>%
        dplyr::filter(all(c("2021", "2025") %in% period[!is.na(share_total) & !is.na(share_lges)])) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(revenue_group, period) %>%
        dplyr::summarise(dplyr::across(c(share_total, share_lges),  ~mean(.x, na.rm = TRUE))) %>%
        tidyr::pivot_wider(id_cols = revenue_group, 
                                names_from = period, 
                                values_from = c(share_total, share_lges),
                                names_sep = "_") %>%
        dplyr::mutate(change_total = share_total_2025 - share_total_2021, 
                        change_lges = share_lges_2025 - share_lges_2021)

dumbbell_plot <-  ggplot() +
                geom_segment(data = dumbbell_df, mapping = aes(x = share_total_2021, xend = share_total_2025, y = revenue_group, yend = revenue_group, colour = change_total > 0), size = 2) + 
                geom_point(data = dumbbell_df, aes(x = share_total_2021, y = revenue_group, colour = change_total > 0, shape = "2020/21"), size = 4) +
                geom_point(data = dumbbell_df, aes(x = share_total_2025, y = revenue_group, colour = change_total > 0, shape = "2024/25"), size = 4) +
                geom_text(data = dumbbell_df,
                            aes(x = share_total_2025, y = revenue_group, label = scales::percent(share_total_2025, accuracy = 0.1), colour = change_total > 0, vjust = ifelse(change_total > 0, -1.5, 1.5)),
                            size = 3, show.legend = FALSE) +
                geom_text(data = dumbbell_df, 
                            aes(x = share_total_2021, y = revenue_group, label = scales::percent(share_total_2021, accuracy = 0.1), colour = change_total > 0, vjust = ifelse(change_total > 0, 0.75, -0.75)),
                            size = 3, show.legend = FALSE) +           
                scale_colour_manual(values = c("TRUE" = "#8C5E58", "FALSE" = "#2E4B3C")) +
                scale_shape_manual(name = NULL, values = setNames(c(1, 16), c("2020/21", "2024/25"))) +
                labs(title = "Share of Revenue from Grants by Municipality\nGroup, 2020/21 - 2024/25",
                        x = "Share of Total Revenue", 
                        y = "MIIF Category",
                        caption = "substack.com/@onderstromings\n \nSource: National Treasury") +
                scale_x_continuous(limits = c(0, 1),
                                labels = scales::label_percent(), 
                                expand = c(0, 0)) +
                scale_y_discrete( expand = expansion(mult = c(0.14, 0.14))) +
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

dumbbell_plot

dumbbell_path <- paste0(output_dir, "/dumbbell_plot.png")

png(filename = dumbbell_path,
        width = 1150,
        height = 650,
        res = 130)
print(dumbbell_plot)
dev.off()

dumbbell_lges_plot <- ggplot() +
  geom_segment(data = dumbbell_df,
               mapping = aes(x = share_lges_2021, xend = share_lges_2025,
                             y = revenue_group, yend = revenue_group,
                             colour = change_lges > 0),
               size = 2) +
  geom_point(data = dumbbell_df,
             aes(x = share_lges_2021, y = revenue_group,
                 colour = change_lges > 0, shape = "2020/21"),
             size = 4) +
  geom_point(data = dumbbell_df,
             aes(x = share_lges_2025, y = revenue_group,
                 colour = change_lges > 0, shape = "2024/25"),
             size = 4) +
  geom_text(data = dumbbell_df,
            aes(x = share_lges_2025, y = revenue_group,
                label = scales::percent(share_lges_2025, accuracy = 0.1),
                colour = change_lges > 0,
                vjust = ifelse(change_lges > 0, -1.5, 1.5)),
            size = 3, show.legend = FALSE) +
  geom_text(data = dumbbell_df,
            aes(x = share_lges_2021, y = revenue_group,
                label = scales::percent(share_lges_2021, accuracy = 0.1),
                colour = change_lges > 0,
                vjust = ifelse(change_lges > 0, 0.75, -0.75)),
            size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("TRUE" = "#8C5E58", "FALSE" = "#2E4B3C")) +
  scale_shape_manual(name = NULL,
                     values = setNames(c(1, 16), c("2020/21", "2024/25"))) +
  labs(title = "Share of Revenue from the Local Government Equitable Share\nby Municipality Group, 2020/21 - 2024/25",
       x = "Share of Total Revenue",
       y = "MIIF Category",
       caption = "substack.com/@onderstromings\n \nSource: National Treasury") +
  scale_x_continuous(limits = c(0, 1),
                     labels = scales::label_percent(),
                     expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0.14, 0.14))) +
  guides(shape = guide_legend(override.aes = list(colour = "black")),
         colour = "none") +
  substack_theme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    text        = element_text(colour = "#1F2A24"),
    axis.text   = element_text(colour = "#4A4460"),
    axis.text.x = element_text(colour = "#4A4460", angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom",
    legend.key.size = unit(0.45, "cm"),
    plot.margin = margin(12, 16, 8, 12),
    plot.title  = element_text(colour = "#1F2A24", face = "plain", size = rel(1),
                               margin = margin(b = 4), hjust = 0.5)
  )

dumbbell_lges_plot

dumbbell_lges_path <- paste0(output_dir, "/dumbbell_lges_plot.png")

png(filename = dumbbell_lges_path, width = 1150, height = 650, res = 130)
print(dumbbell_lges_plot)
dev.off()
##mention that the pattern exists across LGESG as well

####
## Scatterplot comparing 2020/21 with 2024/25 across municipalities. 
#### 

scatter_df <- grants_df %>%
        dplyr::select(demarcation.label, period, label,amount_ADJB) %>%
        dplyr::filter(label %in% c("total_grants", "total_revenue", "Local Government Equitable Share")) %>%
        tidyr::pivot_wider(id_cols = c(demarcation.label, period), 
                                names_from = label, 
                                values_from = amount_ADJB) %>%
        dplyr::group_by(demarcation.label, period) %>%
        dplyr::summarise(dplyr::across(c(total_grants, total_revenue, `Local Government Equitable Share`), ~sum(.x, na.rm = TRUE))) %>%
        dplyr::filter(total_revenue > total_grants &  total_revenue > `Local Government Equitable Share` & dplyr::if_all(c(total_grants, total_revenue, `Local Government Equitable Share`), ~ !is.na(.x) & .x > 0)) %>%
        dplyr::mutate(share_total = total_grants / total_revenue, 
                        share_lges = `Local Government Equitable Share` / total_revenue) %>%
        dplyr::group_by(demarcation.label, period) %>%
        dplyr::summarise(dplyr::across(c(share_total, share_lges, total_grants, total_revenue, `Local Government Equitable Share`),  ~mean(.x, na.rm = TRUE))) %>%
        dplyr::filter(period %in% c("2021", "2025")) %>%
        tidyr::pivot_wider(id_cols = demarcation.label, 
                                names_from = period, 
                                values_from = c(share_total, share_lges, total_grants, total_revenue, `Local Government Equitable Share`),
                                names_sep = "_")  %>%
        dplyr::filter(dplyr::if_all(dplyr::matches("(_2021|_2025)$"),
                        ~ .x > 0) & share_total_2021 < 1 & share_total_2025 < 1) %>%
        dplyr::mutate(change_total = share_total_2025 - share_total_2021, 
                        change_lges = share_lges_2025 - share_lges_2021) %>%
        dplyr::left_join(munic_list %>% dplyr::select(demarcation.label, municipality.province_code), by = "demarcation.label") %>%
        dplyr::mutate(text = paste0("<b>", demarcation.label, " (", municipality.province_code, ")" , "</b><br>",
        "<b> 2020/21 </b><br>",
        "&nbsp;&nbsp;&nbsp;Total Revenue: R", scales::number(total_revenue_2021, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), "<br>",
        "&nbsp;&nbsp;&nbsp;Total Grants: R", scales::number(total_grants_2021, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), " (", scales::percent(share_total_2021, accuracy = 0.1), ")" , "<br>",
        "&nbsp;&nbsp;&nbsp;LG ESG: R", scales::number(`Local Government Equitable Share_2021`, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), " (", scales::percent(share_lges_2021, accuracy = 0.1), ")" , "<br>",
                paste0(
        "<b> 2024/25 </b><br>",
        "&nbsp;&nbsp;&nbsp;Total Revenue: R", scales::number(total_revenue_2025, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), "<br>",
        "&nbsp;&nbsp;&nbsp;Total Grants: R", scales::number(total_grants_2025, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), " (", scales::percent(share_total_2025, accuracy = 0.1), ")" , "<br>",
        "&nbsp;&nbsp;&nbsp;LG ESG: R", scales::number(`Local Government Equitable Share_2025`, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), " (", scales::percent(share_lges_2025, accuracy = 0.1), ")" , "<br>"))) 

scatterplot <- ggplot(data = scatter_df, aes(x = share_total_2021, y = share_total_2025, colour = change_total > 0, text = text)) +
                geom_abline(intercept = 0, linewidth = 0.75, slope = 1, colour = "#2E4B3C", linetype = "dashed") +
                geom_point(aes(size = total_revenue_2025, colour = change_total > 0), alpha = 0.75, show.legend = FALSE) +
                scale_size(range = c(1, 14), trans = "sqrt") +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 6), 
                                expand = c(0.025, 0.025)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 6), 
                                expand = c(0.025, 0.025)) +
                scale_colour_manual(values = c("TRUE" = "#8C5E58", "FALSE" = "#2E4B3C"), labels = c("TRUE" = "Less Dependent", "FALSE" = "More Dependent")) +
                labs(title = "Share of Revenue from Grants by Municipality,\n2020/21 vs 2024/25", 
                x = "Share (2020/21)", 
                y = "Share (2024/25)", 
                caption = "substack.com/@onderstromings\n \nSource: National Treasury") +
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
        text = "  substack.com/@onderstromings<br>Source: National Treasury",
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

print(unique(grants_df$label))

####
## Difference between original and adjusted amounts (remember to exclude LSEG) by financial year
####
 
col_df <- grants_df %>%
        dplyr::select(demarcation.code, period, amount_ORGB, amount_ADJB, amount_ACT, label, revenue_group) %>%
        dplyr::filter(label %in% c("total_grants", "Local Government Equitable Share", "Municipal Infrastructure Grant", "Urban Settlement Development Grant")) %>%
        tidyr::pivot_wider(id_cols = c(demarcation.code, period, revenue_group), 
                        names_from = label, 
                        values_from = c(amount_ORGB, amount_ADJB, amount_ACT)) %>%
        dplyr::mutate(ORGB_infrastructure = ifelse(revenue_group != "A", `amount_ORGB_Municipal Infrastructure Grant`, `amount_ORGB_Urban Settlement Development Grant`), 
                        ADJB_infrastructure = ifelse(revenue_group != "A", `amount_ADJB_Municipal Infrastructure Grant`, `amount_ADJB_Urban Settlement Development Grant`), 
                        ACT_infrastructure = ifelse(revenue_group != "A",`amount_ACT_Municipal Infrastructure Grant`, `amount_ACT_Urban Settlement Development Grant`),
                        ORGB_conditional = amount_ORGB_total_grants - `amount_ORGB_Local Government Equitable Share`, 
                        ADJB_conditional = amount_ADJB_total_grants - `amount_ADJB_Local Government Equitable Share`, 
                        ADJB_conditional_A = ifelse(revenue_group == "A", `amount_ADJB_Municipal Infrastructure Grant` + `amount_ADJB_Urban Settlement Development Grant`, 0), 
                        ACT_conditional = amount_ACT_total_grants - `amount_ACT_Local Government Equitable Share`,
                        difference = ADJB_conditional - ADJB_conditional, 
                        difference_ACT = ifelse(dplyr::if_all(c("ACT_conditional", "ADJB_conditional"), ~.x > 0), ADJB_conditional - ACT_conditional, NA_integer_ ), 
                        difference_infrastructure = ifelse(dplyr::if_all(c("ADJB_infrastructure", "ORGB_infrastructure"), ~.x > 0), ORGB_infrastructure - ADJB_infrastructure, NA_integer_), 
                        difference_infrastructure_act = ifelse(dplyr::if_all(c("ACT_infrastructure", "ADJB_infrastructure"), ~.x > 0), ADJB_infrastructure - ACT_infrastructure, NA_integer_)) %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(dplyr::across(dplyr::matches("difference|ORGB_conditional|difference_infrastructure|ADJB|adjb|ORGB_infrastructure|ACT|act"), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        dplyr::mutate(pct = ADJB_conditional / ORGB_conditional * 100, 
                        pct_act = ACT_conditional / (ADJB_conditional - ADJB_conditional_A) * 100, 
                        pct_infrastructure = ADJB_infrastructure / ORGB_infrastructure * 100, 
                        pct_infrastructure_act = ACT_infrastructure / ADJB_infrastructure * 100, 
                        period = as.numeric(period))

pct_min <- 0
primary_min <- 0

pct_max <- 100
primary_max <- max(col_df$difference_ACT) * 1.1 

pct_scale  <- (primary_max - primary_min) / (pct_max - pct_min)
pct_offset <- primary_min - pct_scale * pct_min

# data = col_df, aes(x = period, y = difference_ACT)
year_cols <- ggplot() +
            geom_col(data = col_df, aes(x = period, y = difference_ACT, fill = difference_ACT > 0)) +
            geom_line(data = col_df, aes(x = period, y = pct_act * pct_scale), linewidth = 1.5, linetype = "dashed") +
            scale_fill_manual(values = c("TRUE" = "#5C8A5A",  "FALSE" = "#8C5E58")) +
            scale_y_continuous(name = "Difference (R Million)",
                                labels  = function(x) ifelse(x == 0, "", scales::number(x, scale = 1e-6, big.mark = " ")), 
                                breaks = scales::pretty_breaks(n = 6), 
                                expand = expansion(mult = c(0, 0.11)),
                                sec.axis = sec_axis(~ (. - pct_offset) / pct_scale, 
                                            name = "% of Adjusted Budget", 
                                            breaks = seq(0, 100, by = 20), 
                                        labels  = function(x) ifelse(x == 0, "", x)), 
                                            ) + 
            scale_x_continuous(name = "Financial Year", 
                breaks =  2021:2025,
                labels = function(x) paste0(as.integer(x)-1, "/", stringr::str_sub(x, start = 3, end = 4)), 
                expand = 0.05) + 
            labs(title = "Difference between Adjusted Conditional Grants Budgets and\nReported Spending, 2021/22 - 2024/25",
                    caption = "substack.com/@onderstromings\n \nSource: National Treasury\nNote: Municipalities reporting zero expenditure were removed.\n       Urban Settlement Development Grants expenditure are not available.") +
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


####
## Difference between original and adjusted amounts (remember to exclude LSEG) by revenue group
####

col_rev_df <- grants_df %>%
        dplyr::select(demarcation.code, period, amount_ORGB, amount_ADJB, amount_ACT, label, revenue_group) %>%
        dplyr::filter(label %in% c("total_grants", "Local Government Equitable Share", "Municipal Infrastructure Grant", "Urban Settlement Development Grant")) %>%
        tidyr::pivot_wider(id_cols = c(demarcation.code, period, revenue_group), 
                        names_from = label, 
                        values_from = c(amount_ORGB, amount_ADJB, amount_ACT)) %>%
        dplyr::mutate(ORGB_infrastructure = pmax(`amount_ORGB_Municipal Infrastructure Grant`, `amount_ORGB_Urban Settlement Development Grant`), 
                        ADJB_infrastructure = pmax(`amount_ADJB_Municipal Infrastructure Grant`, `amount_ADJB_Urban Settlement Development Grant`), 
                        ACT_infrastructure = pmax(`amount_ACT_Municipal Infrastructure Grant`, `amount_ACT_Urban Settlement Development Grant`),
                        ORGB_conditional = amount_ORGB_total_grants - `amount_ORGB_Local Government Equitable Share`, 
                        ADJB_conditional = amount_ADJB_total_grants - `amount_ADJB_Local Government Equitable Share`, 
                        ADJB_conditional_A = ifelse(revenue_group == "A", `amount_ADJB_Municipal Infrastructure Grant` + `amount_ADJB_Urban Settlement Development Grant`, 0), 
                        ACT_conditional = amount_ACT_total_grants - `amount_ACT_Local Government Equitable Share`,
                        difference = ADJB_conditional - ADJB_conditional, 
                        difference_ACT = ifelse(dplyr::if_all(c("ACT_conditional", "ADJB_conditional"), ~.x > 0), ADJB_conditional - ACT_conditional, NA_integer_ ), 
                        difference_infrastructure = ifelse(dplyr::if_all(c("ADJB_infrastructure", "ORGB_infrastructure"), ~.x > 0), ORGB_infrastructure - ADJB_infrastructure, NA_integer_), 
                        difference_infrastructure_act = ifelse(dplyr::if_all(c("ACT_infrastructure", "ADJB_infrastructure"), ~.x > 0), ADJB_infrastructure - ACT_infrastructure, NA_integer_)) %>%
        dplyr::group_by(revenue_group) %>%
        dplyr::summarise(dplyr::across(dplyr::matches("amount_ADJB_Municipal Infrastructure Grant|difference|ORGB_conditional|amount_ADJB_Urban Settlement Development Grant|difference_infrastructure|ADJB|adjb|ORGB_infrastructure|ACT|act"), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        dplyr::mutate(pct = ADJB_conditional / ORGB_conditional * 100, 
                        pct_act = ifelse(revenue_group == "A", ACT_conditional / (ADJB_conditional - `amount_ADJB_Urban Settlement Development Grant` - `amount_ADJB_Municipal Infrastructure Grant`) * 100, 
                        ACT_conditional / ADJB_conditional * 100), 
                        pct_infrastructure = ADJB_infrastructure / ORGB_infrastructure * 100, 
                        pct_infrastructure_act = ACT_infrastructure / ADJB_infrastructure * 100)

rev_cols <- ggplot() +
            geom_col(data = col_rev_df, aes(x = revenue_group, y = pct_act, fill = difference_ACT > 0)) +
            scale_fill_manual(values = c("TRUE" = "#5C8A5A",  "FALSE" = "#8C5E58")) +
            scale_y_continuous(name = "% of Adjusted Budget",
                                labels  = function(x) ifelse(x == 0, "", scales::percent(x / 100)), 
                                breaks = scales::pretty_breaks(n = 6), 
                                limits = c(0, 100), 
                                expand = c(0, 0)) + 
            scale_x_discrete(name = "MIIF Category", 
                expand = 0.05) + 
            labs(title = "Difference between Adjusted Conditional Grants Budgets and\nReported Spending by MIIF Category, 2021/22 - 2024/25",
                    caption = "substack.com/@onderstromings\n \nSource: National Treasury\nNote: Municipalities reporting zero expenditure were removed.\n       Urban Settlement Development Grants expenditure are not available.") +
            substack_theme +
            theme(legend.position = "none", 
                    panel.grid.major.x  = element_blank()
                    )



rev_cols_path <- paste0(output_dir, "/rev_cols.png")

png(filename = rev_cols_path,
        width = 1100,
        height = 650,
        res = 110)

print(rev_cols)

dev.off()


####
## GeoMap
####

map <- sf::st_read(here::here("data", "munic_map.geojson")) %>%
                dplyr::select(CAT_B, DATE, Shape__Area, Shape__Length, geometry) %>%
                dplyr::rename(demarcation.code = CAT_B)

# Leaves will be coloured by LGES as % of revenue and contain
#                         1) Municipality Name
#                         2) LGES amount (per household)
#                         3)  amount as % of revenue  (change since 20/21)

#                         4) Conditional Amount (per household)
#                         5) amount as % of revenue  (change since 20/21)
#                         5) % Spent

idcols <- c("demarcation.code", 
                "period", 
                "revenue_group", 
                "hh_n", 
                "demarcation.label")

col_dem_df <- grants_df %>%
        dplyr::select(demarcation.code, period, amount_ORGB, amount_ADJB, amount_ACT, label, revenue_group) %>%
        dplyr::filter(label %in% c("total_grants", "Local Government Equitable Share", "Municipal Infrastructure Grant", "Urban Settlement Development Grant", "total_revenue")) %>%
        tidyr::pivot_wider(id_cols = c(demarcation.code, period), 
                        names_from = label, 
                        values_from = c(amount_ORGB, amount_ADJB, amount_ACT)) %>%
        dplyr::mutate(ORGB_infrastructure = pmax(`amount_ORGB_Municipal Infrastructure Grant`, `amount_ORGB_Urban Settlement Development Grant`), 
                        ADJB_infrastructure = pmax(`amount_ADJB_Municipal Infrastructure Grant`, `amount_ADJB_Urban Settlement Development Grant`), 
                        ACT_infrastructure = pmax(`amount_ACT_Municipal Infrastructure Grant`, `amount_ACT_Urban Settlement Development Grant`),
                        ORGB_conditional = amount_ORGB_total_grants - `amount_ORGB_Local Government Equitable Share`, 
                        ADJB_conditional = amount_ADJB_total_grants - `amount_ADJB_Local Government Equitable Share`, 
                        ACT_conditional = amount_ACT_total_grants - `amount_ACT_Local Government Equitable Share`,
                        difference = ADJB_conditional - ADJB_conditional, 
                        difference_ACT = ifelse(dplyr::if_all(c("ACT_conditional", "ADJB_conditional"), ~.x > 0), ADJB_conditional - ACT_conditional, NA_integer_ ), 
                        difference_infrastructure = ifelse(dplyr::if_all(c("ADJB_infrastructure", "ORGB_infrastructure"), ~.x > 0), ORGB_infrastructure - ADJB_infrastructure, NA_integer_), 
                        difference_infrastructure_act = ifelse(dplyr::if_all(c("ACT_infrastructure", "ADJB_infrastructure"), ~.x > 0), ADJB_infrastructure - ACT_infrastructure, NA_integer_)) %>%
        dplyr::group_by(demarcation.code, period) %>%
        dplyr::filter(period %in% c("2021", "2025")) %>%
        dplyr::summarise(dplyr::across(dplyr::matches("difference|ORGB_conditional|difference_infrastructure|ADJB|adjb|ORGB_infrastructure|ACT|act|total_revenue"), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        dplyr::mutate(pct = ADJB_conditional / ORGB_conditional * 100, 
                        pct_act = ACT_conditional / ADJB_conditional * 100, 
                        pct_infrastructure = ADJB_infrastructure / ORGB_infrastructure * 100, 
                        pct_infrastructure_act = ACT_infrastructure / ADJB_infrastructure * 100, 
                        share_conditional = ADJB_conditional / amount_ADJB_total_revenue * 100)  %>%
        tidyr::pivot_wider(id_cols = demarcation.code, 
                        names_from = period, 
                        values_from = c(pct_act, ADJB_conditional, share_conditional)) %>%
        dplyr::mutate(change_conditional = share_conditional_2025 - share_conditional_2021) %>%
        dplyr::select(demarcation.code, pct_act_2025, ADJB_conditional_2025, share_conditional_2025, change_conditional)

scatter_map <-  grants_df %>%
        dplyr::select(demarcation.code, period, label,amount_ADJB) %>%
        dplyr::filter(label %in% c("total_grants", "total_revenue", "Local Government Equitable Share")) %>%
        tidyr::pivot_wider(id_cols = c(demarcation.code, period), 
                                names_from = label, 
                                values_from = amount_ADJB) %>%
        dplyr::group_by(demarcation.code, period) %>%
        dplyr::summarise(dplyr::across(c(total_grants, total_revenue, `Local Government Equitable Share`), ~sum(.x, na.rm = TRUE))) %>%
        # dplyr::filter(total_revenue > total_grants &  total_revenue > `Local Government Equitable Share` & dplyr::if_all(c(total_grants, total_revenue, `Local Government Equitable Share`), ~ !is.na(.x) & .x > 0)) %>%
        dplyr::mutate(share_total = ifelse(total_grants < total_revenue & dplyr::if_all(c(total_grants, total_revenue), ~ !is.na(.x) & .x > 0), total_grants / total_revenue, NA_integer_), 
                        share_lges =ifelse(`Local Government Equitable Share` < total_revenue & dplyr::if_all(c(`Local Government Equitable Share`, total_revenue), ~ !is.na(.x) & .x > 0), `Local Government Equitable Share` / total_revenue, NA_integer_)) %>%
        dplyr::group_by(demarcation.code, period) %>%
        dplyr::summarise(dplyr::across(c(share_total, share_lges, total_grants, total_revenue, `Local Government Equitable Share`),  ~mean(.x, na.rm = TRUE))) %>%
        dplyr::filter(period %in% c("2021", "2025")) %>%
        tidyr::pivot_wider(id_cols = demarcation.code, 
                                names_from = period, 
                                values_from = c(share_total, share_lges, total_grants, total_revenue, `Local Government Equitable Share`),
                                names_sep = "_")  %>%
        dplyr::mutate(change_total = share_total_2025 - share_total_2021, 
                        change_lges = share_lges_2025 - share_lges_2021) %>%
        dplyr::select(demarcation.code, `Local Government Equitable Share_2025`, share_lges_2025, change_lges)

map_df <- dplyr::left_join(dplyr::left_join(dplyr::full_join(scatter_map, col_dem_df, by = "demarcation.code"), munic_list %>% dplyr::select(demarcation.code, hh_n, municipality.province_code, demarcation.label), by = "demarcation.code"), map, by = "demarcation.code") %>%
        dplyr::mutate(lges_hh = `Local Government Equitable Share_2025` / hh_n, 
                        cg_hh = `ADJB_conditional_2025` / hh_n, 
                        act_spend = ADJB_conditional_2025 * pct_act_2025, 
                        leaf = paste0("<b>", demarcation.label, " (", municipality.province_code, ")" , "</b><br>",
        "<b> Equitable Share Grant </b><br>",
        "&nbsp;&nbsp;&nbsp;Amount (per Household): R", scales::number(`Local Government Equitable Share_2025`, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), " (R",  scales::number(`lges_hh` , accuracy = 1, big.mark = " ", suffix = "/hh"), ")", "<br>",
        "&nbsp;&nbsp;&nbsp;Share of Revenue (Change since 2020/21): ", scales::percent(share_lges_2025, accuracy = 0.1, big.mark = " ", suffix = "%"), " (", scales::percent(change_lges, accuracy = 0.1, suffix = "%"), ")" , "<br>",
        "<b> Conditional Grants </b><br>",
        "&nbsp;&nbsp;&nbsp;Amount (per Household): R", scales::number(`ADJB_conditional_2025`, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), " (R",  scales::number(`cg_hh` , accuracy = 1, big.mark = " ", suffix = "/hh"), ")", "<br>",
        "&nbsp;&nbsp;&nbsp;Share of Revenue (Change since 2020/21): ", scales::percent(share_conditional_2025 / 100, accuracy = 0.1, big.mark = " ", suffix = "%"), " (", scales::percent(change_conditional / 100, accuracy = 0.1, suffix = "%"), ")" , "<br>",
        "&nbsp;&nbsp;&nbsp;Recorded Expenditure (Percentage of Adjusted Budget): R", scales::number(act_spend, scale = 1e-6, accuracy = 1, big.mark = " ", suffix = "m"), " (", scales::percent(pct_act_2025 / 100, accuracy = 0.1, suffix = "%"), ")" , "<br>"))

pal <- colorNumeric(palette = c("#F7F5EF", "#DCE9C4", "#B8C99A", "#8AA870", "#5C8A5A", "#2E4B3C", "#1A2E22"),
                domain = log1p(map_df$lges_hh))

interact_map <- leaflet(
                map_df %>%
                st_as_sf()
                ) %>%
        addPolygons(
                fillColor = ~pal(log1p(lges_hh)),
                weight = 1, 
                color = "white",
                fillOpacity = 0.75,
                highlightOptions = highlightOptions(
                        weight = 2,
                        color = "black",
                        fillOpacity = 0.9, 
                        bringToFront = TRUE
                ),
                label = ~lapply(leaf, htmltools::HTML),
        labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "6px 8px"),
        textsize = "13px",
        direction = "auto"
        )
        ) %>%
        addLegend(
                 pal = pal,
                values = log1p(map_df$lges_hh),
                title = "ESG per Household",
                position = "bottomright",
                labFormat = labelFormat(
                prefix    = "R",
                big.mark  = " ",
                digits    = 0,
                transform = function(x) expm1(x)          # back to rand values for the labels
                )) %>%
        htmlwidgets::onRender("
    function(el, x) {
      this.getContainer().style.background = '#EDE8DA';
    }
  ")

interact_map

filepath_html <- paste0(output_dir, "/grant_map.html")
htmlwidgets::saveWidget(interact_map, file = filepath_html, selfcontained = TRUE)

####
## Table
###

table_df <- grants_df %>%
        dplyr::mutate(period = as.character(period)) %>%
        dplyr::filter(!(label %in% c("total_revenue_excluding_capital_transfers_and_contributions", "total_transfers", "share", departments, "total_grants", "total_revenue")), 
                period == "2025") %>%
        dplyr::select(demarcation.code, amount_ADJB, label) %>%
        dplyr::group_by(demarcation.code, label) %>%
        dplyr::summarise(amount_ADJB = sum(amount_ADJB, na.rm = TRUE)) %>%
        dplyr::rename(type = label,
                value = amount_ADJB) %>%
        dplyr::mutate(type = ifelse(type %in% top_items, type, "Other"), 
                        period = as.numeric(period)) %>%
        dplyr::mutate(type = case_when(type %in% c("Municipal Infrastructure Grant", "Urban Settlement Development Grant") ~ "Infrastructure Grant", 
                        TRUE ~ type)) %>%
        dplyr::group_by(demarcation.code, type) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(id_cols = demarcation.code, 
                        names_from = type, 
                        values_from = value)  %>%
        dplyr::left_join(map_df %>% dplyr::select(demarcation.code, demarcation.label, `Local Government Equitable Share_2025`, `ADJB_conditional_2025`, act_spend, pct_act_2025, municipality.province_code)) %>%
        dplyr::mutate(total_grants = ADJB_conditional_2025 + `Local Government Equitable Share_2025`) %>%
        dplyr::rename(Municipality = demarcation.label, 
                        Province = municipality.province_code, 
                        `Total Grants (R)` = total_grants,
                        `Equitable Share Grant (R)` = `Local Government Equitable Share`, 
                        `Infrastructure Grants (R)` = `Infrastructure Grant`, 
                        `Conditional Grants (R)` = ADJB_conditional_2025, 
                        `CG Expenditure (R)` = act_spend, 
                        `CG Expenditure (%)` = pct_act_2025) %>%
        dplyr::select(Municipality, Province, `Total Grants (R)`, `Equitable Share Grant (R)`,  `Infrastructure Grants (R)`, `Conditional Grants (R)`, `CG Expenditure (R)`, `CG Expenditure (%)`)

utils::write.csv(table_df, here::here("grants", "grants_table.csv"))
