
#' Read current OBI data
#' @description
#' read OBI analytic data from Turbo
#' 
#'
#' @export
#' @rdname read_current_data

read_current_data = function() {
  if (.Platform$OS.type == "windows") {
    # windows file path
    current_dt_path <-
      "P:/OBI_abstracted_data/Current_Data/data/output/sourcetables_obi_export_recodes.csv"
  } else if (.Platform$OS.type == "unix") {
    # MAC file path
    current_dt_path <-
      "/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/output/sourcetables_OBI_export_recodes.csv"
  }
  
  
  # test path
  if(!file.exists(current_dt_path)){
    error("Check your VPN connection. Path doesn't exsit at ", current_dt_path)
  } 
  
  dt = data.table::fread(current_dt_path)
  
  # message after done reading
  message("Current OBI data are loaded")
  
  return(dt)
  
  
}
