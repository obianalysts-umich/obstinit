
#' Create OBI cohort of locked, complete cases for most analyses
#' @description
#' this function does 
#'    1. include case ≥2020-01-01 based on infant date of birth
#'    2. include case if case is complete using flg_complete
#'    3. filter to complete and locked cases(limit_to_locked = TRUE)
#' 
#' 
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @param limit_to_locked Logical -- if TRUE, only return cases that are locked
#' @param sas_processed_dt Logical -- set to TRUE  if using data processed from SAS; set to FALSE (default) to use data processed from R
#' @export

create_obi_cohort = function(df,
                             sas_processed_dt = FALSE,
                             limit_to_locked = TRUE) {
  # data processing using sas data
  if (sas_processed_dt) {
    cli::cli_abort(
      c(
        "SAS processed data is not supported. SAS processing stopped in Aug 2024.",
        "x" = "change `sas_processed_dt = FALSE`"
      )
    )
  } else {
    if (limit_to_locked) {
      df |>
        # additionally limit to locked
        filter(
          infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00"),
          flg_complete == 1,
          locked_90days_flg == 1
        )
    } else{
      df |>
        # limit to cases >= 2024 and complete
        filter(infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00"),
               flg_complete == 1)
    }
  }
}
