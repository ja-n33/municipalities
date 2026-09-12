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

for (fy in c("2021", "2122", "2223", "2324", "2425")){

    path <- here::here("data", paste0("mfma", fy, ".xlsx"))

    heads <- readxl::read_excel(path, sheet = 1, skip = 2, n_max = 2, col_names = FALSE)

    category <- zoo::na.locf(unlist(heads[1, ], use.names = FALSE), na.rm = FALSE)
    subcat <- unlist(heads[2, ], use.names = FALSE)

    col_names <- ifelse(is.na(subcat), tolower(category), tolower(paste0(category, "_", subcat)))

    col_names <- janitor::make_clean_names(col_names)

    if (fy == "2021"){
    temp_data <- readxl::read_excel(path, sheet = 1, skip = 5, col_names = col_names) %>%
                dplyr::mutate(period = paste0("20", stringr::str_sub(fy, start = -2, end = -1))) %>%
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
                dplyr::mutate(period = paste0("20", stringr::str_sub(fy, start = -2, end = -1))) %>%
                dplyr::rename_with(~ stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_"), .cols = 7:9) %>%
                dplyr::rename_with(~ paste0("prior", stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_")), .cols = 10:12)
    }

    else {
        temp_data <- readxl::read_excel(path, sheet = 1, skip = 5, col_names = col_names) %>%
                dplyr::mutate(period = paste0("20", stringr::str_sub(fy, start = -2, end = -1))) %>%
                dplyr::rename_with(~ stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_"),
                        .cols = 7:9) %>%
                dplyr::rename_with(~ paste0("prior", stringr::str_remove(.x, "^x20\\d{2}_\\d{2}_audit_")),
                        .cols = 10:12)
                        }

    fy_annexures <- dplyr::bind_rows(fy_annexures, temp_data) 
}

fy_annexures <- fy_annexures %>%
    dplyr::mutate(auditee = case_when(
        auditee == "Emalahleni" & stringr::str_detect(municipal_mdb_codes, "MP") ~ "Emalahleni (MP)",
        auditee == "Emalahleni" & stringr::str_detect(municipal_mdb_codes, "EC") ~ "Emalahleni (EC)",
        auditee %in% c("Dr A B Xuma Local Municipality", "Dr A B Xuma") ~ "Dr A B Xuma Local Municipality",
        auditee == "Elias Motsoaledi" ~ "Elias Motsoaledi (Greater Groblersdal)",
        auditee == "Lekwa-Teemane" ~ "Lekwa Teemane",
        TRUE ~ auditee))

label_code_lookup <- fy_annexures %>%
    dplyr::filter(!is.na(municipal_mdb_codes)) %>%
    dplyr::select(municipal_mdb_codes, auditee) %>%
    dplyr::filter(!stringr::str_detect(stringr::str_sub(municipal_mdb_codes, start = 1, end = 2), "DC")) %>%
    dplyr::distinct(municipal_mdb_codes, auditee) %>%
    dplyr::bind_rows(tibble::tibble(municipal_mdb_codes = "NW397", auditee = "Dr A B Xuma Local Municipality"))

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
    dplyr::rename(demarcation.code = municipal_mdb_codes, 
                demarcation.label = auditee) %>%
    dplyr::filter(auditee_type %in% c("LM", "IC", "ICM", "MET"), 
                demarcation.code %in% munic_list$demarcation.code, 
                outcomes_audit_opinion == "Audit not finalised at legislated date" |
                !is.na(unauthorised_irregular_fruitless_qual) |
                !is.na(unauth_amt) | 
                !is.na(irreg_amt)) %>%
        dplyr::distinct(demarcation.code, period)
yearcheck <- annexures_df %>% dplyr::count(period)


munic_check <- annexures_df %>%
                dplyr::group_by(period) %>%
                summarise(n_dmc = n_distinct(demarcation.code))

dupes <- annexures_df %>%
  dplyr::group_by(period) %>%
  dplyr::add_count(demarcation.code, name = "n") %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(period, demarcation.code)

setdiff(munic_list$demarcation.code, annexures_df$demarcation.code)


