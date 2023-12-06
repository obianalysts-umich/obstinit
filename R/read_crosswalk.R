
#' Read in OBI crosswalk
#' @description
#' this function reads the current OBI crosswalk file. This file contains information on hospital variables like teaching status, as well as group 1 status and color.
#' 
#' @rdname read_crosswalk
#' @import tidyverse
#' @export

read_crosswalk = function() {
  openxlsx::read.xlsx(
    "P:/OBI hospital and provider/OBI Hospital Information/Crosswalk/crosswalk_current.xlsx",
    sheet = 1
  ) %>%
    janitor::clean_names()
}