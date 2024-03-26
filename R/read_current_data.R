#' Read current OBI data
#' @param sas_processed_dt if read SAS processed data, default to TRUE
#' @description
#' read OBI analytic data from Turbo
#' @export

read_current_data <- function(sas_processed_dt = TRUE) {
  # if reading SAS processed data
  if (sas_processed_dt) {
    if (.Platform$OS.type == "windows") {
      # windows file path
      current_dt_path <-
        "P:/OBI_abstracted_data/Current_Data/data/output/sourcetables_obi_export_recodes.csv"
    } else if (.Platform$OS.type == "unix") {
      # MAC file path
      current_dt_path <-
        "/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/output/sourcetables_OBI_export_recodes.csv"
    }
  } else {
    # read R processed R data file
    # to keep formatting
    if (.Platform$OS.type == "windows") {
      # windows file path
      current_dt_path <-
        "P:/OBI_abstracted_data/Current_Data/data/output/obi_data_R.Rds"
    } else if (.Platform$OS.type == "unix") {
      # MAC file path
      current_dt_path <-
        "/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/output/obi_data_R.Rds"
    }
  }

  # test path
  if (!file.exists(current_dt_path)) {
    stop("Check your VPN connection. Path doesn't exsit at ", current_dt_path)
  }

  # read data
  if (sas_processed_dt) {
    # read SAS processed data
    dt <- data.table::fread(current_dt_path)
  } else {
    # read R processed data
    dt <- readRDS(current_dt_path)
  }

  # message after done reading
  message(
    "Current OBI data are loaded. The data are updated on ",
    lubridate::as_date(file.info(current_dt_path)$mtime), "."
  )

  return(dt)
}
