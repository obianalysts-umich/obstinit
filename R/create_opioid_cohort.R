
#' Create opioid cohort
#' @description
#' This function creates the cohort to be used for opioid analytics. It take the obi_cohort dataframe created by obstinit::create_obi_cohort() and creates a variable for number of days from delivery to discharge, then filters the dataframe to births in 2023 where the birthing person did not use opioids during pregnancy, the LOS was <= 4 days, and births with an opioid prescribed at discharge are not missing data on opioid type, unit, quantity, or dose. It creates groups for mode of delivery (vaginal, vaginal with advanced laceration, and cesarean) and selects only the variables relevant to opioid analyses. Last, it reads in OME conversion factors from the Turbo drive and merges them into the OBI dataframe.
#' 
#' @param df A dataframe; preferably the dataframe created by create_obi_cohort()
#' 
#' @import tidyverse
#'                
#' @export

create_opioid_cohort = function(df) {
  # read in OME multiplication factors
  
  if (Sys.info()["sysname"] == "Windows") {
    turbo_path = "P:/opioids/"
  } else if (Sys.info()["sysname"] == "Darwin") {
    turbo_path <- "/Volumes/nur-kanelow/opioids/"
  }
  
  OME <- readxl::read_xlsx(paste0(turbo_path, "OME_data.xlsx")) |>
    select(-c(mult_factor_source, notes)) |>
    rename(opioid_name = opioid) |>
    select(-c(dose_common_unit))
  
  # create appropriately filtered OBI dataset
  
  df1 = df |>
    mutate(
      discharge_dt = lubridate::dmy_hms(discharge_dt),
      dob_to_dischg_days = interval(as.Date(infant_dob_dt), as.Date(discharge_dt)) %/% days(1)
    ) |>
    filter(
      # No opioid use during pregnancy
      opioid_use_b == 0,
      # Limit to 2023 cohort
      birth_year >= 2023,
      # LOS birth to discharge is <= 4 days
      dob_to_dischg_days <= 4,
      # births where a discharge opioid was prescribed are not missing data on opioid type, quantity, unit or dose
      discharge_opioid_e == 0 |
        (
          discharge_opioid_e == 1 & !is.na(opioid_dose_no1) &
            opioid_unit_e1 != 99 &
            !is.na(opioid_unit_e1) &
            !is.na(opioid_quantity_no1)
        )
    ) |>
    # create mode of delivery groups - vaginal with no advanced laceration, vaginal with advanced laceration, cesarean
    mutate(opioid_group = ifelse(
      mode_of_delivery_cd %in% c(4:6),
      "Cesarean",
      ifelse(
        mode_of_delivery_cd %in% c(1:3) &
          (laceration_fourth_deg_b == 1 |
             laceration_third_deg_b),
        "Vaginal with laceration",
        "Vaginal no laceration"
      )
    )) |>
    # select only relevant variables for opioid calculation
    select(
      site_name,
      patientid,
      birth,
      opioid_group,
      discharge_opioid_e,
      opioid_e1,
      opioid_e2,
      opioid_e3,
      opioid_unit_e1,
      opioid_unit_e2,
      opioid_unit_e3,
      opioid_dose_no1,
      opioid_dose_no2,
      opioid_dose_no3,
      opioid_quantity_no1,
      opioid_quantity_no2,
      opioid_quantity_no3,
      opioid_24hr_prior_dc_b,
      opioid_type_24hr_prior_dc_e,
      starts_with("codeine_"),
      starts_with("hydrocodone_"),
      starts_with("hydromorphone_"),
      starts_with("morphine_"),
      starts_with("oxycodone_"),
      starts_with("tramadol_")
    )
  
  # Merge OME conversion factors into OBI dataset
  
  df_list <- list(df1, OME, OME, OME)
  
  by_list <- list(
    c("opioid_e1" = "opioid_num"),
    c("opioid_e2" = "opioid_num"),
    c("opioid_e3" = "opioid_num")
  )
  
  opioid_cohort_final <- reduce2(df_list, by_list, left_join) |>
    rename(
      opioid_name1 = opioid_name.x,
      mult_factor_OME1 = mult_factor_OME.x,
      opioid_name2 = opioid_name.y,
      mult_factor_OME2 = mult_factor_OME.y,
      opioid_name3 = opioid_name,
      mult_factor_OME3 = mult_factor_OME
    ) |>
    select(
      site_name:opioid_quantity_no3,
      opioid_name1,
      opioid_name2,
      opioid_name3,
      mult_factor_OME1,
      mult_factor_OME2,
      mult_factor_OME3,
      opioid_24hr_prior_dc_b,
      opioid_type_24hr_prior_dc_e,
      starts_with("codeine_"),
      starts_with("hydrocodone_"),
      starts_with("hydromorphone_"),
      starts_with("morphine_"),
      starts_with("oxycodone_"),
      starts_with("tramadol_")
    ) |>
    mutate(
      opioid_OME1 = opioid_dose_no1 * opioid_quantity_no1 * mult_factor_OME1,
      opioid_OME2 = opioid_dose_no2 * opioid_quantity_no2 * mult_factor_OME2,
      opioid_OME3 = opioid_dose_no3 * opioid_quantity_no3 * mult_factor_OME3,
      opioid_OME_total = rowSums(across(opioid_OME1:opioid_OME3), na.rm = T)
    ) |>
    # filter out births with negative OME or OME > 99999 - this is in line with advice given to CDAs by Q&O team on how to input data for missing values
    filter(opioid_OME_total >= 0,
           is.na(opioid_quantity_no1) | opioid_quantity_no1 < 99999)
  
}
