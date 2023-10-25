
#' Create dystocia cohort
#' 
#' To create dystocia compliance cohort
#' @param df A data frame, preferably one that has already been run through obstint::create_obi_cohort()
#' @import tidyverse
#' @export
#' @rdname create_dystocia_cohort

create_dystocia_cohort = function(df) {
  df %>%
    filter(planned_mode_of_delivery_cd == 1,
           transfer_from_home_birth_b != 1)
}