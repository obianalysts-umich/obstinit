
#' Structure OBI data for plotting control charts
#'
#' @param df A data frame
#' @param date_var The date variable to be used for grouping; usually infant_dob_dt
#' @param date_gran The granularity of dates we want to use for our control chart; monthly or quarterly
#' @param num_var The variable to be summarized as the numerator of the rate we're interested in calculating - should be binary 0 1
#' @param den_var The variable to be summarized as the denominator of the rate we're interested in calculating - should be binary 0 1
#' @param long Whether to pivot the data to long format - default is T as this is the data structure needed for ggplot2
#' @import tidyverse
#' @import data.table
#' @export
#' @rdname structure_data

structure_data = function(df,
                          date_var,
                          date_gran = c(year_mon, year_qtr),
                          num_var,
                          den_var,
                          long = T) {
  ctrl_cohort = df %>% filter(flg_complete == 1, birth_year != 2019) %>% mutate(
    date_lub = lubridate::dmy_hms({
      {
        date_var
      }
    }),
    year_mon = zoo::as.yearmon(date_lub),
    year_qtr = zoo::as.yearqtr(date_lub),
    date_var = {
      {
        date_gran
      }
    }
  ) %>% group_by(date_var) %>% summarize(
    num = sum({
      {
        num_var
      }
    }),
    denom = sum({
      {
        den_var
      }
    }),
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

  ## get CL

  CL_pre = summary(limits_pre)[, 12]

  ## rep values

  CL = rep(CL_pre, nrow(ctrl_cohort))

  ## bind to original

  ctrl_w_CL = cbind(ctrl_cohort, CL)

  # use qcc package to get limits -------------------------------------------

  qc_limits = qcc::qcc(
    type = "p",
    data = ctrl_w_CL$num,
    sizes = ctrl_w_CL$denom,
    center = ctrl_w_CL$CL,
    plot = F
  )

  # get limits

  limits = qc_limits$limits
  rownames(limits) = c(1:nrow(limits))

  # bind limits to main dataset

  ctrl_cohort_fin = cbind(ctrl_w_CL, limits)

  # apply shift violations to prior rows ------------------------------------

  ctrl_cohort_fin = ctrl_cohort_fin %>% mutate(
    x3_sig_viol = ifelse(rate > UCL, 1, 0),
    n_pts_oneside_CL = ifelse(rate > CL, 1, 0)
  )

  ## make data.table and assign values for shift violations

  ctrl_cohort_fin = data.table::setDT(ctrl_cohort_fin)
  ctrl_cohort_fin[, rleid_pts := sum(n_pts_oneside_CL), by = data.table::rleid(n_pts_oneside_CL)]

  # final data manipulation -------------------------------------------------
  ## apply violations to N prior data points, note if point is above UCL or
  ## below LCL, apply colors for ggplot

  ctrl_cohort_alerts = ctrl_cohort_fin %>% mutate(
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
    p_chart_color = case_when(
      p_chart_alert == "Above UCL" ~ OBI.color::prim_pink(),
      p_chart_alert == "Below LCL" ~ OBI.color::prim_teal(),
      p_chart_alert == "Shift" ~ "#f8b434",
      p_chart_alert == "No alert" ~ OBI.color::prim_dark_blue()
    )
  ) %>% select(-c(x3_sig_viol:above_or_below))

# pivot longer if long = true ---------------------------------------------

  if (long){
    ctrl_dt_long = ctrl_cohort_alerts %>% select(
      -c(
        num,
        denom
      )
    ) %>% pivot_longer(
      cols = c(rate, CL, LCL, UCL),
      names_to = "ctrl_chart_part",
      values_to = "ctrl_chart_value"
    ) %>% mutate(
      ctrl_chart_part = factor(
        ctrl_chart_part,
        levels = c("UCL", "CL", "LCL", "rate"),
        ordered = T
      )
    )
  }

  else (return(ctrl_cohort_alerts))

}
