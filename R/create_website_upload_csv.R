#' Create website upload CSV
#' 
#' To create the CSV needed to upload reports to the OBI website. The CSV files should be in the following [format](https://docs.google.com/spreadsheets/d/19Ua83FMIsT9RjrugrvPD4NJEZcfI8e_cBFfrudDSDeM/edit#gid=141729511) 
#' (see the [SOP](https://docs.google.com/document/d/1g0BcXkA-rcm0AAtkTjK9uE7Bh2o5NzB_fBr_UDr_niw/edit) for more details). If you are uploading a report multiple times in a month and would like to overwrite past versions, 
#' you do not need to re-run this function; you can reuse the previous CSV as long as the report name remains unchanged. If you do NOT want to overwrite previous versions uploaded in the same month, you will need to ensure
#' the report does not have the same name as the previous version.
#' 
#' @param report_name A character string that is the tile of the report excluding the site names. The report filename must be formatted as "filename - sitename". 
#' @param tags A character vector that contains the tags for the report. The available tags must be one of the following: Data Request, NTSV Performance Report, NTSV Summary Report, Annual P4P Scorecards, PRIME Reports, Other, or Site-Specific Patient Voices Resources.
#' @param members_only A logical value that indicates whether the report is for members only. Default is 1.
#' @param site_list A character vector that contains site names for hospitals with reports.
#' @param output_path Path to output the CSV file to (must end in /).
#' 
#' @return A saved CSV file that can be uploaded to the OBI website.
#' @examples 
#' /dontrun{
#' library(tidyverse)
#' 
#' site_list <- c("Michigan Medicine Ann Arbor", "Hurley Medical Center Flint")
#' 
#' create_website_upload_csv(report_name = "test report - Michigan Medicine",
#'                          tags = "NTSV Performance Report",
#'                           site_list = site_list,
#'                           output_path = "test_report.csv")
#' }
#' 
#' @import tidyr
#' @export

create_website_upload_csv <- function(report_name,
                                      tags,
                                      members_only = 1,
                                      site_list,
                                      output_path) {
  

  # tags validation --------------------------------------------------------------
  
    if (!(tags %in% c("Data Request", "NTSV Performance Report",  "NTSV Summary Report", "Annual P4P Scorecards", "Other", "Site-Specific Patient Voices Resources"))) {
      stop("Tags must be one of the following: Data Request, NTSV Performance Report, Weekly Dystocia Report, Annual P4P Scorecards, PRIME Reports, Other, or Site-Specific Patient Voices Resources.")
    }
    
    if (tags %in% c("P4P Progress Report", "Weekly Dystocia Report")) {
      cli::cli_abort("{{tags}} is a retired tag. The corresponding section on OBI website has been removed.")
    }  
    
  
  # constants -----------------------------
  yr_month_lbl <- format(Sys.Date(), "%Y %B")
  file_url_yr_month_lbl <- tolower(format(Sys.Date(), "%B-%Y"))
  title_lbl <- report_name
  file_url_path <- paste0("/wp-content/uploads/private/dlm_uploads/",
                          file_url_yr_month_lbl,
                          "/")
  
  # create required columns -----------------------------
  csv_upload_one_row_per_site <- data.frame(site_name = site_list) |>
    dplyr::transmute(
      title = paste0(yr_month_lbl, " - ", title_lbl, " - ", site_name),
      categories = site_name,
      tags = tags,
      members_only = members_only,
      redirect = 0,
      version = "",
      file_date = "",
      file_urls = paste0(file_url_path, title_lbl, " - ", site_name, ".pdf")
    )
  
  # expand rows with type column; type column has two values: "NTSV" and "NTSV - 2"
  csv_upload_matched_format <- tidyr::expand_grid(type = c("download", "version"), csv_upload_one_row_per_site) |>
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

  message("CSV file saved to: ", output_path)
}
