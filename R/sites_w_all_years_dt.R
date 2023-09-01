
#' Limit dataset to hospitals that have data for all years 2020 - max year of your choice
#' 
#' @param df Data frame -- usually this is OBI's current, processed data read into R using data.table::fread. Preferably this has already been run through obstinit::create_obi_cohort()
#' @import tidyverse
#' @export
#' @rdname sites_w_all_years_dt

sites_w_all_years_dt = function(df, max_year) {
  n_years = (max_year - 2020) + 1
  
  sites = df %>% filter(birth_year > 2019, birth_year <= max_year) %>% distinct(site_name, birth_year) %>% add_count(site_name) %>% arrange(site_name) %>% filter(n == n_years) %>% distinct(site_name) %>% pull()
  
  df %>% filter(site_name %in% sites) 
}
