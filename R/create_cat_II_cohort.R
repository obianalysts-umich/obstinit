
#' Create cat II cohort
#'
#' @export
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @param limit_to_catII_sites Whether or not to limit the cohort to 2023 Cat II sites - set to false by default
#' @import tidyverse
#' @rdname create_cat_II_cohort

create_cat_II_cohort = function(df, limit_to_catII_sites = F) {
  df1 = df %>%
    filter(
      planned_mode_of_delivery_cd == 1,
      transfer_from_home_birth_b != 1,
      mode_of_delivery_cd == 4,
      ces_primary_indication_cd == 6,
      fht_category_e %in% c(1, 3)
    )
  
  if (limit_to_catII_sites) {
    df1 %>%
      filter(QII_Choice_2023 == "Cat 2 Fetal Heart Rate Tracings")
  }
}