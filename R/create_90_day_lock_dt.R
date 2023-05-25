#' Create OBI case lock dates
#' 
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @export
#' @rdname create_obi_cohort

create_90_day_lock_dt = function() {
  format(Sys.Date() - lubridate::days(91), "%m/%d/%Y")
}