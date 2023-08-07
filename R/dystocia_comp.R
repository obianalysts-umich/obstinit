
#' Dystocia compliance functions
#' To calculate dystocia compliance, overall and by type
#' @param df A data frame, preferably one that has already been run through obstint::create_obi_cohort()
#' @param include_types If TRUE, calculates compliance for individual types of dystocia

calculate_dys_comp_rate = function(df, include_types = F) {
  df1 = df %>% summarize(
    dys_comp_num = sum(overall_dystocia_compliance_num),
    dys_comp_denom = sum(overall_dystocia_den_all),
    dys_comp_rate = dys_comp_num / dys_comp_denom
  )
  
  if (include_types) {
    df %>% summarize(
      dys_comp_num = sum(overall_dystocia_compliance_num),
      dys_comp_denom = sum(overall_dystocia_den_all),
      dys_comp_rate = dys_comp_num / dys_comp_denom,
      failed_iol_num = sum(failed_induction_num_compliance, na.rm = T),
      failed_iol_denom = sum(failed_induction_den_all, na.rm = T),
      failed_iol_comp_rate = failed_iol_num / failed_iol_denom,
      latent_arrest_num = sum(latent_num_compliance, na.rm = T),
      latent_arrest_denom = sum(latent_den_all, na.rm = T),
      latent_arrest_comp_rate = latent_arrest_num / latent_arrest_denom,
      active_arrest_num = sum(active_num_compliance, na.rm = T),
      active_arrest_denom = sum(active_den_all, na.rm = T),
      active_arrest_comp_rate = active_arrest_num / active_arrest_denom,
      arrest_descent_num = sum(arrest_num_compliance, na.rm = T),
      arrest_descent_denom = sum(arrest_den_all, na.rm = T),
      arrest_descent_comp_rate = arrest_descent_num / arrest_descent_denom
    )
  } else
    (return(df1))
}
