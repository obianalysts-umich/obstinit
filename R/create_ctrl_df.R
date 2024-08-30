#' Structure OBI data for plotting control charts
#' @description This function takes in a data frame row per observation like OBI data.
#'            It then summarize the numerator and denominator of a rate of interest.
#'            This function also use control chart principles to calculate CIs and flag alerts.
#'
#' @param df A data frame, for example OBI export data
#' @param date_var The date variable to be used for grouping; usually infant_dob_dt;
#'                 Make use this variable is in a date format, for example: lubridate::dmy_hms(infant_dob_dt)
#' @param date_gran "month" or "quarter"; The granularity of dates we want to use for our control chart; default is "month".
#' @param num_var The variable to be summarized as the numerator - should be binary 0 1
#' @param den_var The variable to be summarized as the denominator - should be binary 0 1
#' @param nsigmas a numeric value specifying the number of sigmas to use for computing control limits. default to 3.
#' @param long Whether to pivot the data to long format - default is TRUE as this is the data structure needed for ggplot2
#' @param increase_is_bad If TRUE, this is a trend that would ideally be decreasing over time; if FALSE, ideally increasing. default is TRUE.
#' @param for_highchart If TRUE, multiplies all rates by 100 for highchart; default is FALSE.
#' @rdname create_ctrl_df
#'
#'  
#' @returns
#' An tibble with the aggregated numerator and denominator by input date variable
#'
#'
#' @export


create_ctrl_df <- function(df,
                           date_var,
                           num_var,
                           den_var,
                           date_gran = "month",
                           nsigmas = 3,
                           long = F,
                           increase_is_bad = TRUE,
                           for_highchart = FALSE) {
  # outcome rates by time table--------------------------------------------

  ctrl_cohort <- df |>
    mutate(
      year_mon = zoo::as.yearmon({{ date_var }}),
      year_qtr = zoo::as.yearqtr({{ date_var }})
    )

  ## date granularity + for_highchart date formatting

  if (date_gran == "month") {
    ctrl_cohort <- ctrl_cohort |> mutate(date_var = year_mon)

    if (for_highchart) {
      ctrl_cohort <- ctrl_cohort |> mutate(date_var = lubridate::my(date_var))
    }
  } else if (date_gran == "quarter") {
    ctrl_cohort <- ctrl_cohort |> mutate(date_var = year_qtr)

    if (for_highchart) {
      ctrl_cohort <- ctrl_cohort |> mutate(date_var = as.character(date_var))
    }
  }

  ## group by date_var and calculate numerator and denominator rates

  ctrl_cohort <- ctrl_cohort |>
    group_by(date_var) |>
    summarize(
      num = sum({{ num_var }}, na.rm = T),
      denom = sum({{ den_var }}, na.rm = T),
      rate = num / denom,
      .groups = "drop"
    )

  # qicharts2 package to get CL frozen @ 12 mo ------------------------------

  limits_pre <- qicharts2::qic(
    num,
    n = denom,
    x = date_var,
    freeze = 12,
    data = ctrl_cohort,
    chart = "p"
  )

  ## get CL values

  CL <- summary(limits_pre)$CL

  ## bind to original

  ctrl_w_CL <- ctrl_cohort |> mutate(CL = as.numeric(CL))

  # use qcc package to get limits (for the gray area) ---------------------

  qc_limits <- qcc::qcc(
    type = "p",
    data = ctrl_w_CL$num,
    sizes = ctrl_w_CL$denom,
    center = ctrl_w_CL$CL,
    plot = F,
    nsigmas = nsigmas
  )

  # get limits

  limits <- qc_limits$limits
  rownames(limits) <- c(1:nrow(limits))

  # bind limits to main dataset

  ctrl_cohort_fin <- cbind(ctrl_w_CL, limits)

  # apply shift violations to prior rows ----------------------------------

  ctrl_cohort_fin <- ctrl_cohort_fin |>
    mutate(
      x3_sig_viol = ifelse(rate > UCL | rate < LCL, 1, 0),
      n_pts_above_CL = ifelse(rate > CL, 1, 0),
      n_pts_below_CL = ifelse(rate < CL, 1, 0)
    )

  ## make data.table and assign values for shift violations

  ctrl_cohort_fin <- data.table::setDT(ctrl_cohort_fin)
  ctrl_cohort_fin[, rleid_pts_above := sum(n_pts_above_CL), by = rleid(n_pts_above_CL)]
  ctrl_cohort_fin[, rleid_pts_below := sum(n_pts_below_CL), by = rleid(n_pts_below_CL)]

  # final data manipulation -----------------------------------------------
  ## apply violations to N prior data points, note if point is above UCL or
  ## below LCL, apply colors for ggplot

  ctrl_cohort_alerts <- ctrl_cohort_fin |>
    mutate(
      violations = ifelse(x3_sig_viol == 1, 1, ifelse(rleid_pts_above >= 8 | rleid_pts_below >= 8, 4, 0)),
      above_or_below = ifelse(
        violations == 1,
        ifelse(rate > UCL, "Above", "Below"),
        as.character(NA)
      ),
      p_chart_alert = case_when(
        violations == 1 & above_or_below == "Above" ~ "Above UCL",
        violations == 1 &
          above_or_below == "Below" ~ "Below LCL",
        violations == 4 ~ "Shift",
        TRUE ~ "No alert"
      ),
      p_chart_alert = factor(p_chart_alert, levels = unique(p_chart_alert)),
      point_color =
        case_when(
          increase_is_bad & p_chart_alert == "Above UCL" ~ "#b64083",
          increase_is_bad == F &
            p_chart_alert == "Above UCL" ~ OBI.color::prim_teal(),
          increase_is_bad &
            p_chart_alert == "Below LCL" ~ OBI.color::prim_teal(),
          increase_is_bad == F &
            p_chart_alert == "Below LCL" ~ "#b64083",
          p_chart_alert == "Shift" ~ "#f8b434",
          TRUE ~ OBI.color::prim_dark_blue()
        )
    ) |>
    select(-c(x3_sig_viol:above_or_below)) |>
    group_by(p_chart_alert) |>
    mutate(col_ID = cur_group_id())

  ## multiply all rates by 100 for highchart

  if (for_highchart) {
    ctrl_cohort_alerts <- ctrl_cohort_alerts |> mutate(
      rate = round(rate * 100, digits = 1),
      CL = round(CL *
        100, digits = 1),
      LCL = round(LCL *
        100, digits = 1),
      UCL = round(UCL *
        100, digits = 1)
    )
  }

  # pivot longer if long = true -------------------------------------------

  if (long) {
    ctrl_dt_long <- ctrl_cohort_alerts |>
      # select(-c(num, denom)) |>
      pivot_longer(
        cols = c(rate, CL, LCL, UCL),
        names_to = "ctrl_chart_part",
        values_to = "ctrl_chart_value"
      ) |>
      mutate(ctrl_chart_part = factor(
        ctrl_chart_part,
        levels = c("UCL", "CL", "LCL", "rate"),
        ordered = T
      ))

    ctrl_dt_long
  } else {
    (return(ctrl_cohort_alerts))
  }
}
