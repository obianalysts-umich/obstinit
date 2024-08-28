#' Calculate dystocia compliance
#' 
#' To calculate dystocia compliance, overall and by type
#' @param df A data frame, preferably one that has already been run through obstint::create_obi_cohort()
#' @param include_types If TRUE, calculates compliance for individual types of dystocia
#' @import tidyverse
#' @export
#' @rdname calculate_dys_comp

calculate_dys_comp <- function(df, include_types = F) {
  df1 = df |>
    summarize(
      dys_comp_num = sum(dystocia_noncompliant_flg == 0, na.rm = T),
      dys_comp_denom = sum(!is.na(dystocia_noncompliant_flg)),
      dys_comp_rate = dys_comp_num / dys_comp_denom
    )
  
  if (include_types) {
    df |>
      summarize(across(
        c(
          dystocia_noncompliant_flg,
          dystocia_failed_induction_noncompliant_flg,
          dystocia_latent_phase_arrest_noncompliant_flg,
          dystocia_active_phase_arrest_noncompliant_flg,
          dystocia_arrest_descent_noncompliant_flg
        ),
        list(
          denom = ~ sum(!is.na(.x)),
          num = ~ sum(.x == 0, na.rm = T),
          rate = ~ mean(.x == 0, na.rm = T)
        )
      ))
  } else
    (return(df1))
}
