#' Impute Cesarean rates for Ascension Hospitals (data breach)
#' 
#' @description
#' This function uses the Ascension Hospitals' NTSV birth and cesarean volumes from January 2024 to April 2024 to calculate their average cesarean rate and imputes those values for the months of May and June 2024, when there was a data breach impacing case abstraction. Once this dataset has been created, it can be joined to any hospital-level dataset using MDHHS ID.
#' 
#' @param df An UNGROUPED (patient-level) dataframe that includes MDHHS ID
#' 
#' @import tidyverse
#' 
#' @examples
#' /dontrun{
#' obi <- read_current_dt()
#' obi_cohort <- obi |> create_obi_cohort()
#' ascension_ces_rates <- obi_cohort |> impute_ascension_ces_rates()
#' }
#' 
#' @export

impute_ascension_ces_rates <- function(df) {
  ## list of ascension sites
  ascensions <- c("39001", "63029", "25006", "50014", "63019", "63020", "82053")
  
  ##last month of 2024 data we have
  last_month_dt <- df  |>
    # limit to ascension sites in 2024
    filter(infant_year == 2024, external_mdhhs_site_id %in% ascensions) |>
    summarize(max_month = max(infant_month)) |>
    pull()
  
  last_ces_mo <- paste0("ces_vol_month_", last_month_dt)
  
  last_birth_mo <- paste0("birth_vol_month_", last_month_dt)
  
  ## impute birth and cesarean volume for ascension sites
  monthly_vol_ascension <- df  |>
    # limit to ascension sites in 2024
    filter(infant_year == 2024, external_mdhhs_site_id %in% ascensions) |>
    # calculate monthly birth volume
    group_by(site_name, external_mdhhs_site_id, infant_month) |>
    summarize(birth_vol = n(),
              ces_vol = sum(cesarean_flg, na.rm = T)) |>
    pivot_wider(
      names_from = infant_month,
      values_from = c(birth_vol, ces_vol),
      names_prefix = "month_"
    ) |>
    mutate(
      # average birth volume January - April
      avg_birth_vol = ceiling(rowMeans(across(
        c(birth_vol_month_1:birth_vol_month_4)
      ))),
      # average cesarean volume January - April
      avg_ces_vol = ceiling(rowMeans(across(
        c(ces_vol_month_1:ces_vol_month_4)
      ))),
      # estimate 2024 birth volume - use observed number Jan - April and July - September, use Jan - April avg for May and June
      birth_vol_24_est = rowSums(across(
        c(birth_vol_month_1:birth_vol_month_4)
      )) + rowSums(across(c(
        birth_vol_month_7:{{last_birth_mo}}
      ))) + (avg_birth_vol * 2),
      # estimate 2024 ccesarean volume  - use observed number Jan - April and July - September, use Jan - April avg for May and June
      ces_vol_24_est = rowSums(across(c(
        ces_vol_month_1:ces_vol_month_4
      ))) + rowSums(across(c(
        ces_vol_month_7:{{last_ces_mo}}
      ))) + (avg_ces_vol * 2),
      ces_rate_24_est = ces_vol_24_est / birth_vol_24_est
    ) |>
    ungroup() |>
    # select only necessary var
    select(external_mdhhs_site_id,
           birth_vol_24_est,
           ces_vol_24_est,
           ces_rate_24_est)
  
}
