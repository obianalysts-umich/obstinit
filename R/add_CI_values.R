#' confidence interval for a rate
#' @description
#' This function create upper and lower CI for observed mean or proportion.
#' Two new variables will be added: LC and UC
#' the formula is based on 95% CI for a PROPORTION: rate+/- 1.96 x sqrt((!!var x (1-!!var))/!!n_cases) OR 95% CI for a MEAN: mean +/- (1.96 * (sd(!!var, na.rm = T) / sqrt(!!n_cases)))
#' 
#' 
#' @param var variable you want to sort
#' @param n_cases number of cases used to calculate CI
#' @param mean_or_proportion Confidence interval for a mean or proportion? Set to proportion by default
#' 
#' @importFrom dplyr %>% across mutate mutate_at select filter rename rename_at
#'
#' @examples
#' library(dplyr)
#'tb = tibble::tibble(cs_rate = c(0.1, 0.3),
#'                     n_pt_cases = c(140, 234))
#'tb %>% 
#'  add_CI_values(var = cs_rate,
#'                n_cases = n_pt_cases)
#'                
#'OME_dt |> 
#'   add_CI_values(var = opioid_OME_total,
#'                 n_cases = n_pt,
#'                 mean_or_proportion = "mean")
#'                
#' @export


add_CI_values <- function(data, var, n_cases, mean_or_proportion = "proportion") {
  var <- enexpr(var)
  n_cases <- enexpr(n_cases)
  
  if (mean_or_proportion == "proportion") {
    data %>%
      mutate(
        LC = !!var - (1.96 * sqrt((
          !!var * (1-!!var)
        ) / !!n_cases)),
        UC = !!var + (1.96 * sqrt((
          !!var * (1-!!var)
        ) / !!n_cases)),
        LC = ifelse(LC < 0, 0.0000, round(LC, 4)),
        UC = ifelse(UC > 1, 1.0000, round(UC, 4))
      )
    
  }
  else if (mean_or_proportion == "mean") {
    data %>%
      mutate(
        LC = !!var - (1.96 * (sd(!!var, na.rm = T) / sqrt(!!n_cases))),
        UC = !!var + (1.96 * (sd(!!var, na.rm = T) / sqrt(!!n_cases))),
        LC = round(LC, 4),
        UC = round(UC, 4)
      )
  }
}
