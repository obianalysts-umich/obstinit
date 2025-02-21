#' NNA Progress
#' 
#' To calculate progress towards the NNA goal.
#' 
#' @param  obi_dt OBI nightly export data. 
#' @param start_date The start date of the time period for which you are interested in seeing NNA progress. Must be formatted as "YYYY-MM-DD".
#' @param end_date The end date of the time period for which you are interested in seeing NNA progress. Must be formatted as "YYYY-MM-DD".
#' @param OE_output A dataframe that contains the data read in from the O:E ratio .csv file.
#' @param ... Grouping arguments. By default includes external_mdhhs_site_id and site_name.
#' 
#' @return A dataframe with the progress towards the NNA goals during the selected timeframe.
#' @examples 
#' /dontrun{
#' NNA_progress(
#' obi_dt, 
#' start_date = "2024-10-01", 
#' end_date = "2024-11-10", 
#' OE_output, 
#' g1_status_25, delivery_volume_2023_lbl
#' )
#' }
#' 
#' @import tidyverse
#' @export
#' 


NNA_progress <- function(obi_dt, start_date, end_date, OE_output, ...){
  
  OE_output_nonames <- OE_output |> 
    select(-site_name) 
  
  obi_dt |> 
    filter(infant_dob_dt >= start_date, infant_dob_dt < end_date) |> 
    group_by(site_name, external_mdhhs_site_id, ...) |> 
    summarize(
      birth_vol_current_timeframe = sum(birth),
      cesarean_rate_current_timeframe = mean(cesarean_flg, na.rm = TRUE),
    ) |>
    ungroup() |> 
    left_join(OE_output_nonames, by = c("external_mdhhs_site_id" = "external_mdhhs_site_id")) |>
    mutate(
      incident_cases = (cesarean_rate_current_timeframe - ces_estimate) * birth_vol_current_timeframe,
      incident_cases = ifelse(
        incident_cases < 0,
        floor(incident_cases),
        ceiling(incident_cases)
      ),
      num_still_to_avert_expected = NNA_expected_12mo + incident_cases, 
      num_still_to_avert_HP2030 = NNA_HP2030_12mo + incident_cases
    ) |> 
    select(-c(birth_vol:ces_rate_24_imputed),-c(NNA_expected_9mo, NNA_HP2030_9mo))
}
