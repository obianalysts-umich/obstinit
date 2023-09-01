
#' Limit dataset to hospitals that have data for all years 2020 - current
#' 
#' @param df Data frame -- usually this is OBI's current, processed data read into R using data.table::fread
#' @import tidyverse
#' @export
#' @rdname sites_w_all_years_dt

sites_w_all_years_dt = function(df, max_year) {
  df %>% filter(birth_year > 2019, birth_year <= max_year) %>% distinct(site_name, birth_year)
}