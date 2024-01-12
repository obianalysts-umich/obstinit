
#' Create TeamBirth cohort
#'
#' @export
#' @param df Data frame -- usually this is obi_cohort created using obstinit::create_obi_cohort
#' @param limit_to_teambirth_sites Whether cohort should be limited to 2023 teambirth sites; default is TRUE
#' @import tidyverse
#' @rdname create_teambirth_cohort

create_teambirth_cohort <-
  function(df, limit_to_teambirth_sites = T) {
    df1 <- df |>
      filter(planned_mode_of_delivery_cd == 1) |>
      mutate(
        huddle_per_12h = ifelse(
          huddle_doc_b == 1,
          (huddle_doc_count_no / Admission_minus_delivery) * 12,
          0
        ),
        tb_full_compliance = ifelse(admit_huddle_mtg_compliance == 1 &
                                      huddle_per_12h >= 1, 1, 0),
        teambirth_year = case_when(
          QII_Choice_2021 == "Team Birth" ~ 3,
          QII_Choice_2022 == "Team Birth" ~ 2,
          TRUE ~ 1
        )
      )
    
    if (limit_to_teambirth_sites) {
      df1 |>
        filter(QII_Choice_2023 == "Team Birth")
    }
    else{
      return(df1)
    }
  }