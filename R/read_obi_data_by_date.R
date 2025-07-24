#' Read OBI Data by Date
#'
#' Reads OBI data from a specified date folder based on the operating system.
#'
#' @param date_folder_name Character string specifying the date folder name
#'   (default: "2025-07-21"). Should be in YYYY-MM-DD format.
#'
#' @return A data frame containing the OBI data from the specified date folder
#'
#' @examples
#' \dontrun{
#' # Load OBI data for a specific date
#' obi_data <- read_obi_date_by_date("2025-07-21")
#' }
#'
#' @import cli
#' @export
read_obi_data_by_date <- function(date_folder_name = "2025-07-21") {
  # data path -----------------------------
  if (.Platform$OS.type == "windows") {
    # windows file path
    drive_path <- "P:/OBI_abstracted_data/"
  } else if (.Platform$OS.type == "unix") {
    drive_path <- "/Volumes/nur-kanelow/OBI_abstracted_data/"
  }

  dt_path <- paste0(drive_path, date_folder_name, "/data/output/obi_data_R.Rds")

  # testing -----------------------------
  # check if date is a Monday
  date_obj <- as.Date(date_folder_name)
  if (weekdays(date_obj) != "Monday") {
    cli::cli_inform(c(
      "i" = "Date '{date_folder_name}' is not a Monday. OBI data is typically backed up and saved on Mondays."
    ))
  }

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

  # message after done reading
  cli::cli_inform(c(
    "i" = "OBI date on {date_folder_name} loaded successfully."
  ))

  return(dt)
}
