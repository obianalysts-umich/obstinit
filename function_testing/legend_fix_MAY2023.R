
library(tidyverse)
library(ggforce)

# data --------------------------------------------------------------------

obi = data.table::fread(
  "P:/OBI_abstracted_data/2023-05-09/data/output/sourcetables_OBI_export_recodes.csv"
)

obi_cohort = obi %>% mutate(
  infant_dob_dt = lubridate::dmy_hms(infant_dob_dt),
  case_lock_dt = infant_dob_dt + lubridate::days(90),
  case_locked = ifelse(case_lock_dt < Sys.Date(), 1, 0)
) %>% filter(birth_year > 2019, flg_complete == 1, case_locked == 1)


# functions ----------------------------------------------------------------

## structure data

structure_data = function(df,
                          date_var,
                          num_var,
                          den_var,
                          date_gran = year_mon,
                          nsigmas = 3,
                          long = F,
                          increase_is_bad = T) {
  
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
    mutate(x3_sig_viol = ifelse(rate > UCL, 1, 0),
           n_pts_oneside_CL = ifelse(rate > CL, 1, 0))
  
  ## make data.table and assign values for shift violations
  
  ctrl_cohort_fin = data.table::setDT(ctrl_cohort_fin)
  ctrl_cohort_fin[, rleid_pts := sum(n_pts_oneside_CL), by = data.table::rleid(n_pts_oneside_CL)]
  
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
        ),
      line_value = case_when(
        p_chart_alert == "Above UCL" ~ 2,
        p_chart_alert == "Below UCL" ~ -2,
        p_chart_alert == "Shift" ~ 1,
        TRUE ~ 0
      )
    ) %>% select(-c(x3_sig_viol:above_or_below))
  
  # pivot longer if long = true ---------------------------------------------
  
  if (long) {
    ctrl_dt_long = ctrl_cohort_alerts %>%
      select(-c(num, denom)) %>%
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

## plot ctrl chart

plot_ctrl_chart = function(df, plot_center_line = T, increase_is_bad = T) {
  
  # assign line color based on value
  
  line_color_pal = c(if (any(df$p_chart_alert == "No alert")) {
    OBI.color::prim_dark_blue()
  },
  if (increase_is_bad &
      any(df$p_chart_alert == "Below UCL")) {
    OBI.color::prim_teal()
  },
  if (increase_is_bad == F &
      any(df$p_chart_alert == "Below UCL")) {
    "#b64083"
  },
  if (any(df$p_chart_alert == "Shift")) {
    "#f8b434"
  },
  if (increase_is_bad &
      any(df$p_chart_alert == "Above UCL")) {
    "#b64083"
  },
  if (increase_is_bad == F &
      any(df$p_chart_alert == "Above UCL")) {
    OBI.color::prim_teal()
  })
  
  line_values = c(unique(df$line_value))
  
  # sort value for line color assignment
  # make sure if violation happened before shift, color assignments are correct
  line_values_sort = line_values[order(line_values)]
  
  # assign labels for legend
  
  legend_lab = df %>% select(point_color, p_chart_alert) %>% group_by(point_color, p_chart_alert) %>% slice_head() %>% select(p_chart_alert) %>% pull()
  
  # plot --------------------------------------------------------------------
  
  plot_1 = ggplot(aes(x = date_var),
                  data = df) +
    geom_ribbon(aes(ymin = LCL, ymax = UCL),
                fill = "#CAC4CE",
                alpha = 0.4) +
    geom_point(
      aes(y = rate, fill = point_color),
      size = 3,
      shape = 21,
      stroke = 0
    ) +
    scale_fill_identity(guide = guide_legend("Control chart alert"),
                        labels = legend_lab) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent)
  
  plot_2 = if (length(line_values) == 1) {
    plot_1 +
      geom_line(aes(y = rate, color = point_color), linewidth = 0.8) +
      scale_color_identity(guide = "none")
  }
  else{
    plot_1 +
      geom_link2(aes(y = rate, color = line_value), linewidth = 0.8) +
      scale_color_gradientn(
        colors = line_color_pal,
        # values = scales::rescale(line_values),
        values = scales::rescale(line_values_sort),
        guide = "none"
      )
  }
  
  if (plot_center_line){plot_2 + geom_line(
    aes(y = CL),
    color = OBI.color::prim_dark_blue(),
    linetype = "dashed",
    linewidth = 0.75,
    alpha = 0.5
  )}
  else{plot_2}
  
  
}


# test --------------------------------------------------------------------

ces_ctrl_df = obi_cohort %>% structure_data(infant_dob_dt, cesarean, birth)

ces_ctrl_df %>% plot_ctrl_chart()

legend_val = ces_ctrl_df %>% select(point_color, p_chart_alert) %>% group_by(point_color, p_chart_alert) %>% slice_head() %>% select(point_color) %>% pull()

legend_lab = ces_ctrl_df %>% select(point_color, p_chart_alert) %>% group_by(point_color, p_chart_alert) %>% slice_head() %>% select(p_chart_alert) %>% pull()

dys_comp_df = obi_cohort %>% structure_data(infant_dob_dt, overall_dystocia_compliance_num, overall_dystocia_den_all, increase_is_bad = F)

dys_comp_df %>% plot_ctrl_chart(increase_is_bad = F)

legend_lab_dys = dys_comp_df %>% select(point_color, p_chart_alert) %>% group_by(point_color, p_chart_alert) %>% slice_head() %>% select(p_chart_alert) %>% pull()
