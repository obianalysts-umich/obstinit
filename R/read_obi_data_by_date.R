#' Read OBI Data by Date
#'
#' Reads OBI data from a specified date folder based on the operating system.
#' The function automatically adjusts the file path structure based on the date:
#' dates before 2026-01-27 use the legacy path structure (/data/output/),
#' while dates on or after 2026-01-27 use the new AR transition path structure (/output/).
#'
#' @param date_folder_name Character string specifying the date folder name
#'   (default: "2026-02-27"). Should be in YYYY-MM-DD format.
#'
#' @return A data frame containing the OBI data from the specified date folder
#'
#' @examples
#' \dontrun{
#' # Load OBI data for a specific date
#' obi_data <- read_obi_data_by_date("2026-02-27")
#'
#' # Load older data (uses legacy path structure)
#' obi_data_old <- read_obi_data_by_date("2026-01-20")
#' }
#'
#' @import cli
#' @export
#'
#'
read_obi_data_by_date <- function(date_folder_name = "2026-02-27") {
  # data path -----------------------------
  if (.Platform$OS.type == "windows") {
    # windows file path
    drive_path <- "P:/OBI_abstracted_data/"
  } else if (.Platform$OS.type == "unix") {
    drive_path <- "/Volumes/nur-kanelow/OBI_abstracted_data/"
  }


  # Adjust path based on date
  # after January 22, 2026, the path structure changed because of AR transition
  date_obj <- as.Date(date_folder_name)
  cutoff_date <- as.Date("2026-01-27")

  if (date_obj > cutoff_date) {
    dt_path <- paste0(drive_path, date_folder_name, "/output/obi_data_R.Rds") # AR data path
  } else {
    dt_path <- paste0(drive_path, date_folder_name, "/data/output/obi_data_R.Rds")
  }

  # testing -----------------------------
  # test path
  if (!file.exists(dt_path)) {
    cli::cli_abort(c(
      "!" = "Path doesn't exist at {.file {dt_path}}",
      "i" = "Check your VPN connection.",
      "i" = "Make sure the date folder '{date_folder_name}' exists at '{drive_path}'"
    ))
  }

  # read data --------------------------------------------------------
  dt <- readRDS(dt_path)
  data_saved_dt <- as.Date(file.info(dt_path)$mtime)


  # message after done reading
  cli::cli_inform(c(
    "i" = "OBI date from {date_folder_name} folder loaded successfully.",
    "i" = "Data was saved on: {data_saved_dt}"
  ))

  return(dt)
}
