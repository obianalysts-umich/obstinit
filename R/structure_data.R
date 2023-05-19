
#' Structure OBI data for plotting control charts
#'
#' @param df A data frame
#' @param date_var The date variable to be used for grouping; usually infant_dob_dt;
#'                 Make use this variable is in a date format, for example: lubridate::dmy_hms(infant_dob_dt)
#' @param date_gran The granularity of dates we want to use for our control chart; year_mon or year_qtr
#' @param num_var The variable to be summarized as the numerator of the rate we're interested in calculating - should be binary 0 1
#' @param den_var The variable to be summarized as the denominator of the rate we're interested in calculating - should be binary 0 1
#' @param nsigmas a numeric value specifying the number of sigmas to use for computing control limits.
#' @param long Whether to pivot the data to long format - default is T as this is the data structure needed for ggplot2
#' @param increase_is_bad If TRUE, this is a trend that would ideally be decreasing over time; if FALSE, ideally increasing
#' @param for_highchart If TRUE, multiplies all rates by 100 for highchart
#' @export
#' @rdname structure_data

structure_data = function(df,
                          date_var,
                          num_var,
                          den_var,
                          date_gran = year_mon,
                          nsigmas = 3,
                          long = F,
                          increase_is_bad = T,
                          for_highchart = F) {

  # outcome rates by time table--------------------------------------------------

  ctrl_cohort = df %>%
    mutate(
      year_mon = zoo::as.yearmon({{date_var}}),
      year_qtr = zoo::as.yearqtr({{date_var}}),
      date_var = {{date_gran}}
    ) %>%
    group_by(date_var) %>%
    summarize(
      num = sum({{num_var}}),
      denom = sum({{den_var}}),
      rate = num / denom,
      .groups = "drop"
    )

  # qicharts2 package to get CL frozen @ 12 mo ------------------------------

  limits_pre = qicharts2::qic(
    num,
    n = denom,
    x = date_var,
    freeze = 12,
    data = ctrl_cohort,
    chart    = 'p'
  )

  ## get CL values

  CL = summary(limits_pre)[, 12]

  ## bind to original

  ctrl_w_CL = ctrl_cohort %>% mutate(CL = as.numeric(CL))

  # use qcc package to get limits (for the gray area) -------------------------------------------

  qc_limits = qcc::qcc(
    type = "p",
    data = ctrl_w_CL$num,
    sizes = ctrl_w_CL$denom,
    center = ctrl_w_CL$CL,
    plot = F,
    nsigmas = nsigmas
  )

  # get limits

  limits = qc_limits$limits
  rownames(limits) = c(1:nrow(limits))

  # bind limits to main dataset

  ctrl_cohort_fin = cbind(ctrl_w_CL, limits)

  # apply shift violations to prior rows ------------------------------------

  ctrl_cohort_fin = ctrl_cohort_fin %>%
    mutate(x3_sig_viol = ifelse(rate > UCL | rate < LCL, 1, 0),
           n_pts_oneside_CL = ifelse(rate > CL, 1, 0))

  ## make data.table and assign values for shift violations

  ctrl_cohort_fin = data.table::setDT(ctrl_cohort_fin)
  ctrl_cohort_fin[, rleid_pts := sum(n_pts_oneside_CL), by = data.table::rleid(n_pts_oneside_CL)]
  
  if (for_highchart) {
    ctrl_cohort_fin = ctrl_cohort_fin %>% mutate(
      rate = round(rate * 100, digits = 1),
      CL = round(CL *
                   100, digits = 1),
      LCL = round(LCL *
                    100, digits = 1),
      UCL = round(UCL *
                    100, digits = 1)
    )
  }

  # final data manipulation -------------------------------------------------
  ## apply violations to N prior data points, note if point is above UCL or
  ## below LCL, apply colors for ggplot

  ctrl_cohort_alerts <- ctrl_cohort_fin %>%
    mutate(
      violations = ifelse(x3_sig_viol == 1, 1, ifelse(rleid_pts >= 8, 4, 0)),
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
    ) %>% select(-c(x3_sig_viol:above_or_below)) %>% group_by(p_chart_alert) %>% mutate(col_ID = cur_group_id())

  # pivot longer if long = true ---------------------------------------------

  if (long) {
    ctrl_dt_long = ctrl_cohort_alerts %>%
      #select(-c(num, denom)) %>%
      pivot_longer(
        cols = c(rate, CL, LCL, UCL),
        names_to = "ctrl_chart_part",
        values_to = "ctrl_chart_value"
      ) %>%
      mutate(ctrl_chart_part = factor(
        ctrl_chart_part,
        levels = c("UCL", "CL", "LCL", "rate"),
        ordered = T
      ))

    ctrl_dt_long
  }

  else
    (return(ctrl_cohort_alerts))

}
