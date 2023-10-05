#' Create OBI case lock dates
#' 
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @export


create_90_day_lock_dt = function() {
  format(lubridate::today() - lubridate::days(91), "%m/%d/%Y")
}