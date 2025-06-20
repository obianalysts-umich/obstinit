

#' Read current eCQM data
#' @param risk_adjust if reading only risk adjustment data; default to FALSE
#' @description
#' read eCQM data from Turbo
#' @export

read_ecqm_data <- function(risk_adjust = FALSE) {
  
  if (risk_adjust == FALSE) {
    # read R processed R data file
    # to keep formatting
    if (.Platform$OS.type == "windows") {
      # windows file path
      current_dt_path <-
        "P:/QRDA1/Data/ecqm_full_dt_current.rds"
    } else if (.Platform$OS.type == "unix") {
      # MAC file path
      current_dt_path <-
        "/Volumes/nur-kanelow/QRDA1/Data/ecqm_full_dt_current.rds"
    }
  } else {
    # read R processed R data file
    # to keep formatting
    if (.Platform$OS.type == "windows") {
      # windows file path
      current_dt_path <-
        "P:/QRDA1/Data/ecqm_risk_adjust_dt_current.rds"
    } else if (.Platform$OS.type == "unix") {
      # MAC file path
      current_dt_path <-
        "/Volumes/nur-kanelow/QRDA1/Data/ecqm_risk_adjust_dt_current.rds"
    }
  }
    
  # test path
  if (!file.exists(current_dt_path)) {
    stop("Check your VPN connection. Path doesn't exsit at ", current_dt_path)
  }
  
  # read R processed data
  dt <- readRDS(current_dt_path)
  
  if (risk_adjust == FALSE) {
    # message after done reading
    message("Current full eCQM data are loaded. Note the data are not limited to the eCQM denominator criteria.")
  } else {
    # message after done reading
    message("Current risk adjustment eCQM data are loaded. Note the data are limited to the eCQM denominator criteria.")
  }
  
  return(dt)
}



