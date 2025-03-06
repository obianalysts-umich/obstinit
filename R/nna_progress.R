#' NNA Progress
#' 
#' To calculate progress towards the NNA goal.
#' 
#' @param  obi_dt OBI nightly export data. 
#' @param start_date The start date of the time period for which you are interested in seeing NNA progress. Must be formatted as "YYYY-MM-DD". Default is "2024-10-01".
#' @param end_date The end date of the time period for which you are interested in seeing NNA progress. Must be formatted as "YYYY-MM-DD". Default is "2025-10-01".
#' @param OE_output The .csv file path to the O:E output. Default is the path for the 2025 O:E output: "OE_ratios/2025/current_OE_ratio_dt.xlsx". The turbo root path is automatically included.
#' @param ... Grouping arguments. By default includes external_mdhhs_site_id and site_name.
#' @param imputed_24 Logical. If TRUE, the baseline cesarean rate will use imputed rates where available. Default is FALSE.
#' 
#' @return A dataframe with the progress towards the NNA goals during the selected timeframe.
#' @examples 
#' /dontrun{
#' NNA_output <- NNA_progress(
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


NNA_progress <- function(
    obi_dt, start_date = "2024-10-01", 
    end_date = "2025-10-01", 
    OE_output = "OE_ratios/2025/current_OE_ratio_dt.xlsx", 
    ...,
    imputed_24 = FALSE){
  
  OE_output_nonames <- readxl::read_excel(paste0(turbo_root_path(), OE_output)) |> 
    select(-c(site_name))
  
  if(imputed_24 == TRUE){
    OE_output_nonames <- OE_output_nonames |> 
      mutate(
        cesarean_rate = case_when(
          !is.na(ces_rate_24_imputed) ~ ces_rate_24_imputed,
          TRUE ~ cesarean_rate
        )
      )
  } else {
    OE_output_nonames <- OE_output_nonames 
  }
  
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
      incident_cases = (cesarean_rate_current_timeframe - cesarean_rate) * birth_vol_current_timeframe,
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
