#' Create website upload CSV
#' 
#' To create the CSV needed to upload reports to the OBI website. The CSV files should be in the following [format](https://docs.google.com/spreadsheets/d/19Ua83FMIsT9RjrugrvPD4NJEZcfI8e_cBFfrudDSDeM/edit#gid=141729511) 
#' (see the [SOP](https://docs.google.com/document/d/1g0BcXkA-rcm0AAtkTjK9uE7Bh2o5NzB_fBr_UDr_niw/edit) for more details)
#' 
#' @param report_name A character string that is the tile of the report excluding the site names. The report filename must be formatted as "[filename] - [sitename]". 
#' @param members_only A logical value that indicates whether the report is for members only. Default is 1.
#' @param site_year An integer that indicates the year used to determine the sites that have reports. Default is 2023.
#' @param exclude_site A character vector that contains any sites to exclude. Default is an empty vector.
#' @param output_path Path to output the CSV file to.
#' 
#' 
#' @export

create_website_upload_csv <- function(report_name,
                                      members_only = 1,
                                      site_year = 2023,
                                      exclude_site = c(""),
                                      output_path) {
  # load data ----------------------------
  site_names <- read_csv("data_raw/OBI _ performance reports example - Categories.csv",
                         show_col_types = FALSE) |>
    janitor::clean_names()
  site_names_mdhhs <- read_csv("data_raw/Engaged Hospital List-site_name_mdhhs.csv",
                               show_col_types = FALSE) |>
    janitor::clean_names()
  
  if (.Platform$OS.type == "windows") {
    # windows file path
    current_dt_path <-
      "P:/OBI_abstracted_data/Current_Data/data/output/obi_data_R.Rds"
  } else if (.Platform$OS.type == "unix") {
    # MAC file path
    current_dt_path <-
      "/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/output/obi_data_R.Rds"
  }
  
  # test path
  if (!file.exists(current_dt_path)) {
    stop("Check your VPN connection. Path doesn't exsit at ",
         current_dt_path)
  }
  
  obi_dt <- readRDS(current_dt_path)
  
  # data processing -----------------------------
  site_names_mdhhs <- site_names_mdhhs |>
    # 5 digits id
    mutate(mdhhs_id = str_pad(mdhhs_id, 5, pad = "0"))
  
  # workstation site names
  workstation_site_names <- obi_dt |>
    distinct(site_name, site_id, external_mdhhs_site_id) |>
    rename(workstation_site_name = site_name) |>
    mutate(
      workstation_site_name = case_when(
        workstation_site_name == "Corewell Health Lakeland/Niles Hospital" ~ "Corewell Health Lakeland Niles Hospital",
        workstation_site_name == "Trinity Health Saint Mary s (Grand Rapids)" ~ "Trinity Health Saint Mary's (Grand Rapids)",
        workstation_site_name == "DMC Hutzel Women s Hospital" ~ "DMC Hutzel Women's Hospital",
        TRUE ~ workstation_site_name
      )
    )
  
  # add IDs to website category data
  site_names_id <- site_names |>
    left_join(site_names_mdhhs, by = c("site_name" = "site_name"))
  
  # manual ID assignment
  site_names_id_manual <- site_names_id |>
    mutate(
      site_id_a_mx = case_match(
        site_name,
        "Munson Healthcare Charlevoix Hospital" ~ 26,
        "Sparrow Hospital Lansing" ~ 117,
        "Trinity Health St Mary Mercy Livonia Hospital" ~ 129,
        "UP Health System Portage Hancock (Level III)" ~ 97,
        "Trinity Health St Joseph Mercy Oakland Hospital Pontiac" ~ 126,
        "DMC Hutzel Women's Hospital Detroit" ~ 44,
        "DMC Sinai Grace Hospital Detroit" ~ 112,
        .default = site_id_a_mx
      )
    )
  
  site_names_id_manual_valid_sites <- site_names_id_manual |>
    filter(!is.na(site_id_a_mx))
  
  # merge with workstation data
  workstation_website_site_name_joined <- site_names_id_manual_valid_sites |>
    left_join(workstation_site_names, by = c("site_id_a_mx" = "site_id"))
  
  # identify missed sites
  workstation_website_site_name_missed <- site_names_id_manual_valid_sites |>
    anti_join(workstation_site_names, by = c("site_id_a_mx" = "site_id"))
  
  if (dim(workstation_website_site_name_missed)[1] != 0) {
    message("Missed sites: ",
            workstation_website_site_name_missed$site_name)
  }
  
  # constants -----------------------------
  yr_month_lbl <- format(Sys.Date(), "%Y %B")
  file_url_yr_month_lbl <- tolower(format(Sys.Date(), "%B-%Y"))
  title_lbl <- report_name
  file_url_path <- paste0("/wp-content/uploads/private/dlm_uploads/",
                          file_url_yr_month_lbl,
                          "/")
  
  # only keep sites that have push reports
  site_with_push_reports <- obi_dt |>
    filter(infant_year == site_year, !site_name %in% exclude_site) |>
    distinct(site_id) |>
    pull()
  
  # create required columns -----------------------------
  csv_upload_one_row_per_site <- workstation_website_site_name_joined |>
    filter(site_id_a_mx %in% site_with_push_reports) |>
    select(categories = site_name, workstation_site_name) |>
    transmute(
      title = paste0(yr_month_lbl, " - ", title_lbl, " - ", workstation_site_name),
      categories,
      members_only = members_only,
      redirect = 0,
      version = "",
      file_date = "",
      file_urls = paste0(file_url_path, title_lbl, " - ", workstation_site_name, ".pdf")
    )
  
  # expand rows with type column; type column has two values: "NTSV" and "NTSV - 2"
  csv_upload_matched_format <- expand_grid(type = c("download", "version"), csv_upload_one_row_per_site) |>
    arrange(categories) |>
    mutate(
      # version row
      title = ifelse(type == "version", "", title),
      categories = ifelse(type == "version", "", categories),
      members_only = ifelse(type == "version", "", members_only),
      redirect = ifelse(type == "version", "", redirect),
      # download row
      version = ifelse(type == "download", "", version),
      file_date = ifelse(type == "download", "", file_date),
      file_urls = ifelse(type == "download", "", file_urls)
    )
  
  # save data -----------------------------
  write_csv(
    csv_upload_matched_format,
    paste0(
      output_path,
      "csv_upload_matched_format ",
      Sys.Date(),
      ".csv"
    )
  )
}
