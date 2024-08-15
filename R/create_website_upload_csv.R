#' Create website upload CSV
#' 
#' To create the CSV needed to upload reports to the OBI website. The CSV files should be in the following [format](https://docs.google.com/spreadsheets/d/19Ua83FMIsT9RjrugrvPD4NJEZcfI8e_cBFfrudDSDeM/edit#gid=141729511) 
#' (see the [SOP](https://docs.google.com/document/d/1g0BcXkA-rcm0AAtkTjK9uE7Bh2o5NzB_fBr_UDr_niw/edit) for more details)
#' 
#' @param report_name A character string that is the tile of the report excluding the site names. The report filename must be formatted as "[filename] - [sitename]". 
#' @param tags A character vector that contains the tags for the report. The available tags on the website are "Data Request", "NTSV Performance Report", "NTSV Summary Report", "P4P Progress Report", "Weekly Dystocia Report", or "Other". Default is an empty vector.
#' @param members_only A logical value that indicates whether the report is for members only. Default is 1.
#' @param site_year An integer that indicates the year used to determine the sites that have reports. Default is 2023.
#' @param site_list A data frame that contains the site names. Default is the `site_names` data frame.
#' @param site_mdhhs A data frame that contains the site names, AMx IDs, and MDHHS IDs. Default is the `site_names_mdhhs` data frame.
#' @param exclude_site A character vector that contains any sites to exclude. Default is an empty vector.
#' @param output_path Path to output the CSV file to (must end in /).
#' 
#' @return A saved CSV file that can be uploaded to the OBI website.
#' @examples 
#' /dontrun{
#' library(tidyverse)
#' create_website_upload_csv(report_name = "test report - Michigan Medicine",
#'                           output_path = "test_report.csv")
#' }
#' 
#' @import tidyr
#' @export

create_website_upload_csv <- function(report_name,
                                      tags,
                                      members_only = 1,
                                      site_year = 2024,
                                      site_list = site_names,
                                      site_mdhhs = site_names_mdhhs,
                                      exclude_site = c(""),
                                      output_path) {
  
  # constants -----------------------------
  yr_month_lbl <- format(Sys.Date(), "%Y %B")
  file_url_yr_month_lbl <- tolower(format(Sys.Date(), "%B-%Y"))
  title_lbl <- report_name
  file_url_path <- paste0("/wp-content/uploads/private/dlm_uploads/",
                          file_url_yr_month_lbl,
                          "/")
  
  # load data ----------------------------
  site_names <- site_list
  site_names_mdhhs <- site_mdhhs
  
  obi_dt <- obstinit::read_current_data(sas_processed_dt = FALSE)
  
  # workstation site names
  workstation_site_names <- obi_dt |>
    dplyr::distinct(site_name, site_id, external_mdhhs_site_id) |>
    dplyr::rename(workstation_site_name = site_name) 
  
  # add IDs to website category data
  site_names_id <- site_names |>
    dplyr::left_join(site_names_mdhhs, by = c("site_name" = "site_name"))
  
  # manual ID assignment
  site_names_id_manual <- site_names_id |>
    dplyr::mutate(
      site_id_a_mx = dplyr::case_match(
        site_name,
        "Munson Healthcare Charlevoix Hospital" ~ 26,
        "Sparrow Hospital Lansing" ~ 117,
        "Trinity Health St Mary Mercy Livonia Hospital" ~ 129,
        "UP Health System Portage Hancock (Level III)" ~ 97,
        "Trinity Health St Joseph Mercy Oakland Hospital Pontiac" ~ 126,
        "DMC Hutzel Women's Hospital Detroit" ~ 44,
        "DMC Sinai Grace Hospital Detroit" ~ 112,
        "Dickinson County Hospital Iron Mountain" ~ 36,
        "Hillsdale Hospital Hillsdale" ~ 52,
        .default = site_id_a_mx
      )
    )
  
  site_names_id_manual_valid_sites <- site_names_id_manual |>
    dplyr::filter(!is.na(site_id_a_mx))
  
  # merge with workstation data
  workstation_website_site_name_joined <- site_names_id_manual_valid_sites |>
    dplyr::left_join(workstation_site_names, by = c("site_id_a_mx" = "site_id"))
  
  # identify missed sites
  workstation_website_site_name_missed <- site_names_id_manual_valid_sites |>
    dplyr::anti_join(workstation_site_names, by = c("site_id_a_mx" = "site_id"))
  
  if (dim(workstation_website_site_name_missed)[1] != 0) {
    message("Missed sites: ",
            workstation_website_site_name_missed$site_name)
  }
  
  # only keep sites that have push reports
  site_with_push_reports <- obi_dt |>
    dplyr::filter(infant_year == site_year, !site_name %in% exclude_site) |>
    dplyr::distinct(site_id) |>
    dplyr::pull()
  
  # create required columns -----------------------------
  csv_upload_one_row_per_site <- workstation_website_site_name_joined |>
    dplyr::filter(site_id_a_mx %in% site_with_push_reports) |>
    dplyr::select(categories = site_name, workstation_site_name) |>
    dplyr::transmute(
      title = paste0(yr_month_lbl, " - ", title_lbl, " - ", workstation_site_name),
      categories,
      tags = tags,
      members_only = members_only,
      redirect = 0,
      version = "",
      file_date = "",
      file_urls = paste0(file_url_path, title_lbl, " - ", workstation_site_name, ".pdf")
    )
  
  # expand rows with type column; type column has two values: "NTSV" and "NTSV - 2"
  csv_upload_matched_format <- expand_grid(type = c("download", "version"), csv_upload_one_row_per_site) |>
    dplyr::arrange(categories) |>
    dplyr::mutate(
      # download row
      title = ifelse(type == "version", "", title),
      categories = ifelse(type == "version", "", categories),
      tags = ifelse(type == "version", "", tags),
      members_only = ifelse(type == "version", "", members_only),
      redirect = ifelse(type == "version", "", redirect),
      # version row
      version = ifelse(type == "download", "", version),
      file_date = ifelse(type == "download", "", file_date),
      file_urls = ifelse(type == "download", "", file_urls)
    )
  
  # save data -----------------------------
  readr::write_csv(
    csv_upload_matched_format,
    paste0(
      output_path,
      "csv_upload_matched_format ",
      Sys.Date(),
      ".csv"
    )
  )
  
  if (!(tags %in% c("Data Request", "NTSV Performance Report", "NTSV Summary Report", "P4P Progress Report", "Weekly Dystocia Report", "Other"))) {
    stop("Tags must be one of the following: Data Request, NTSV Performance Report, NTSV Summary Report, P4P Progress Report, Weekly Dystocia Report, or Other.")
  }
  
  message("Site list based on active sites in 2024.")
  message("CSV file saved to: ", output_path)
}
