#' Create incident case site df
#' @param df Dataframe to be used for calculation
#' @param current_year What year of data is the "current" year?
#' @import tidyverse
#' @export
#' @return A dataframe with one row per site, containing each sites' incident cases comparing the current year to last year. Grouped by MDHHS ID.
#' @family incident_cases

create_incident_case_site_df <- function(df, current_year) {
  # calculate last year
  last_year <- current_year - 1
  
  # variable names for pivot
  ces_var_thisyear <- paste0("ces_rate_", current_year)
  ces_var_lastyear <- paste0("ces_rate_", last_year)
  vol_var_thisyear <- paste0("n_", current_year)
  
  df |>
    # filter down to this year and last year
    filter(infant_year >= last_year) |>
    summarize(
      n = n(),
      ces_rate = mean(cesarean_flg, na.rm = T),
      .by = c(infant_year, external_mdhhs_site_id)
    ) |>
    pivot_wider(names_from = infant_year, values_from = c(n, ces_rate)) |>
    mutate(
      incident_cases = (eval(parse(text = ces_var_thisyear)) - eval(parse(text = ces_var_lastyear))) * eval(parse(text = vol_var_thisyear)),
      incident_cases = if_else(
        incident_cases < 0,
        floor(incident_cases),
        ceiling(incident_cases)
      )
    )
}



#' Calculate collaborative-wide incident cases
#' @param df Dataframe to be used for calculation
#' @param current_year What year of data is the "current" year?
#' @import tidyverse
#' @export
#' @return A single number representing the total incident cases across the collaborative. This number is NEGATIVE if there were fewer cesareans this year than last and POSITIVE if there were more cesareans this year than last.
#' @family incident_cases

calculate_collab_incident_cases <- function(df, current_year) {
  # calculate last year
  last_year <- current_year - 1
  
  # variable names for pivot
  ces_var_thisyear <- paste0("ces_rate_", current_year)
  ces_var_lastyear <- paste0("ces_rate_", last_year)
  vol_var_thisyear <- paste0("n_", current_year)
  
  df |>
    # filter down to this year and last year
    filter(infant_year >= last_year) |>
    summarize(
      n = n(),
      ces_rate = mean(cesarean_flg, na.rm = T),
      .by = c(infant_year, external_mdhhs_site_id)
    ) |>
    pivot_wider(names_from = infant_year, values_from = c(n, ces_rate)) |>
    mutate(
      incident_cases = (eval(parse(text = ces_var_thisyear)) - eval(parse(text = ces_var_lastyear))) * eval(parse(text = vol_var_thisyear)),
      incident_cases = if_else(
        incident_cases < 0,
        floor(incident_cases),
        ceiling(incident_cases)
      )
    ) |>
    summarize(sum(incident_cases, na.rm = T)) |>
    pull()
}
