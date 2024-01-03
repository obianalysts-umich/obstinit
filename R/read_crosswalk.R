
#' Read in OBI crosswalk
#' @description
#' this function reads the current OBI crosswalk file. This file contains information on hospital variables like teaching status, as well as group 1 status and color.
#' 
#' @rdname read_crosswalk
#' @import tidyverse
#' @export

read_crosswalk = function() {
  turbo_path = turbo_root_path()
  crosswalk_file_path = paste0(turbo_path, "OBI hospital and provider/OBI Hospital Information/Crosswalk/crosswalk_current.xlsx")
  
  openxlsx::read.xlsx(
    crosswalk_file_path,
    sheet = 1
  ) %>%
    janitor::clean_names() %>% 
    filter(!duplicated(mdhhs_id))
}
