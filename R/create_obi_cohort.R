
#' Create OBI cohort of locked, complete cases for most analyses
#' @description
#' this function does 
#'    1. include case ≥2020-01-01 based on infant date of birth
#'    2. include case if case is complete using flg_complete
#'    3. create case_locked varaible using 90 days lock
#'    4. filter to complete and locked cases(limit_to_locked = TRUE)
#'    5. create mdhhs_id character variable with leading 0's
#' 
#' 
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @param limit_to_locked Logical -- if TRUE, only return cases that are locked
#' @import tidyverse
#' @export

create_obi_cohort = function(df,
                             limit_to_locked = T) {
  df1 = df |>
    mutate(
      infant_dob_dt = lubridate::dmy_hms(infant_dob_dt),
      #case locks at MIDNIGHT AFTER THIS DATE
      case_lock_dt = lubridate::date(infant_dob_dt) + days(90),
      case_locked = ifelse(case_lock_dt < lubridate::today(), 1, 0),
      across(
        c(
          starts_with("opioid_e"),
          starts_with("opioid_dose"),
          starts_with("opioid_quantity_no"),
          starts_with("opioid_unit")
        ),
        ~ ifelse(discharge_opioid_e == 1, .x, NA)
      ),
      mdhhs_id = as.character(external_mdhhs_site_id),
      mdhhs_id = ifelse(nchar(mdhhs_id) < 5, paste0("0", mdhhs_id), mdhhs_id),
      race_eth = factor(
        race_ethnicity3,
        levels = c(
          "AMERICAN INDIAN/ALASKAN NATIVE, NON-HISPANIC",
          "ASIAN/PACIFIC ISLANDER, NON-HISPANIC",
          "BLACK, NON-HISPANIC",
          "HISPANIC",
          "MORE THAN ONE RACE, NOT HISPANIC/LATINO",
          "RACE AND/OR ETHNICITY MISSING",
          "RACE AND/OR ETHNICITY UNKNOWN",
          "WHITE, NON-HISPANIC"
        ),
        labels = c(
          "American Indian or Alaskan Native",
          "Asian or Pacific Islander",
          "Black",
          "Hispanic",
          "More than one race",
          "Missing",
          "Unknown",
          "White"
        )
      ),
      g1_site_2023 = ifelse(
        site_name %in% c(
          "Ascension Genesys Hospital (Grand Blanc)",
          "Ascension St. John Hospital (Detroit)",
          "Bronson Methodist Hospital (Kalamazoo)",
          "Corewell Health Dearborn Hospital",
          "Corewell Health William Beaumont University Hospital",
          "Covenant Healthcare (Saginaw)",
          "DMC Hutzel Women s Hospital",
          "Henry Ford Hospital (Detroit)",
          "Henry Ford West Bloomfield Hospital (West Bloomfield Township)",
          "Holland Hospital (Holland)",
          "Hurley Medical Center (Flint)",
          "MyMichigan Medical Center (Midland)",
          "Sparrow Hospital (Lansing)",
          "St. Joseph Mercy (Oakland)",
          "Trinity Health Ann Arbor",
          "Trinity Health Saint Mary s (Grand Rapids)",
          "University of Michigan Health, West"
        ),
        1,
        0
      ),
      g1_color_2023 = case_when(
        site_name %in% c(
          "Bronson Methodist Hospital (Kalamazoo)",
          "St. Joseph Mercy (Oakland)",
          "Trinity Health Saint Mary s (Grand Rapids)"
        ) ~ "green",
        site_name %in% c(
          "Corewell Health Dearborn Hospital",
          "Henry Ford West Bloomfield Hospital (West Bloomfield Township)",
          "Trinity Health Ann Arbor"
        ) ~ "yellow",
        site_name %in% c(
          "Ascension Genesys Hospital (Grand Blanc)",
          "Ascension St. John Hospital (Detroit)",
          "Corewell Health William Beaumont University Hospital",
          "Covenant Healthcare (Saginaw)",
          "DMC Hutzel Women s Hospital",
          "Henry Ford Hospital (Detroit)",
          "Holland Hospital (Holland)",
          "Hurley Medical Center (Flint)",
          "MyMichigan Medical Center (Midland)",
          "Sparrow Hospital (Lansing)",
          "University of Michigan Health, West"
        ) ~ "red",
        TRUE ~ as.character(NA)
      ),
      select_g1_site_2023 = ifelse(
        site_name %in% c(
          "Ascension Genesys Hospital (Grand Blanc)",
          "Ascension St. John Hospital (Detroit)",
          "Corewell Health William Beaumont University Hospital",
          "Covenant Healthcare (Saginaw)",
          "DMC Hutzel Women s Hospital",
          "Henry Ford Hospital (Detroit)",
          "Holland Hospital (Holland)",
          "Hurley Medical Center (Flint)",
          "MyMichigan Medical Center (Midland)",
          "Sparrow Hospital (Lansing)",
          "University of Michigan Health, West"
        ),
        1,
        0
      ),
    )

  if (limit_to_locked == T) {
    df1 |> filter(
      flg_complete == 1,
      case_locked == 1,
      infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00")
    )
  }
  
  else if (limit_to_locked == F) {
    df1 |> filter(flg_complete == 1,
                  infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00"))
  }
  
}
