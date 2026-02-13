#' Read current OBI data
#' @param sas_processed_dt `r lifecycle::badge("deprecated")`
#'   SAS processed data is not supported (SAS processing stopped in Aug 2024).
#' @description
#' Reads OBI analytic data from Turbo.
#' @return A data frame containing the current OBI data.
#' @export

read_current_data <- function(sas_processed_dt = lifecycle::deprecated()) {
  # Deprecation warning
  if (lifecycle::is_present(sas_processed_dt)) {
    lifecycle::deprecate_warn(
      when = "Aug 2024",
      what = "read_current_data(sas_processed_dt = )",
      details = "SAS processing stopped in Aug 2024. This parameter is removed in a future version."
    )
  }

  # Set file paths by platform
  if (.Platform$OS.type == "windows") {
    current_dt_path <-
      "P:/OBI_abstracted_data/Current_Data/output/obi_data_R.Rds"
    downloaded_dt_path <-
      "P:/OBI_abstracted_data/Current_Data/output/obi_data_R_downloaded.Rds"
  } else if (.Platform$OS.type == "unix") {
    current_dt_path <-
      "/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/output/obi_data_R.Rds"
    downloaded_dt_path <-
      "/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/CSV"
  }

  # Check if path exists
  if (!file.exists(current_dt_path)) {
    stop("Check your VPN connection. Path doesn't exist at ", current_dt_path)
  }

  # Read data
  dt <- readRDS(current_dt_path)

  # Check if data were updated today
  merged_last_update <- lubridate::as_date(file.info(current_dt_path)$mtime)
  downloaded_last_update <- lubridate::as_date(file.info(downloaded_dt_path)$mtime)
  is_today <- merged_last_update == lubridate::today() &&
    downloaded_last_update == lubridate::today()

  # Display status message
  if (is_today) {
    cli::cli_alert_success(glue::glue("Today's OBI data are loaded. {Sys.Date()}"))
  } else {
    cli::cli_bullets(c(
      "!" = "Data are loaded.",
      "x" = "Warning: Data did not update today.",
      "i" = "Last merged update: {merged_last_update}",
      "i" = "Last SFTP update: {downloaded_last_update}"
    ))
  }

  return(dt)
}
