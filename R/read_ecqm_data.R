

#' Read current eCQM data
#' @param limit_to_denominator if TRUE then dataset is limited to only eCQM denominator cases. Default is TRUE. 
#' @description
#' read eCQM data from Turbo
#' @export

read_ecqm_data <- function(limit_to_denominator = TRUE) {
  
  # read R processed R data file
  # to keep formatting
  if (.Platform$OS.type == "windows") {
    # windows file path
    current_dt_path <-
      "P:/QRDA1/Data/ecqm_dt_current.rds"
  } else if (.Platform$OS.type == "unix") {
    # MAC file path
    current_dt_path <-
      "/Volumes/nur-kanelow/QRDA1/Data/ecqm_dt_current.rds"
  }
    
  # test path
  if (!file.exists(current_dt_path)) {
    stop("Check your VPN connection. Path doesn't exsit at ", current_dt_path)
  }
  
  # read R processed data
  dt <- readRDS(current_dt_path)
  
  if (limit_to_denominator == FALSE) {
    # message after done reading
    message("Current full eCQM data are loaded. Note the data are not limited to the eCQM denominator criteria.")
  } else {
    # message after done reading
    message("Current eCQM data are loaded. Note the data are limited to the eCQM denominator criteria.")
  }
  
  return(dt)
}



