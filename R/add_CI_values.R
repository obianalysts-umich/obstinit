#' confidence interval for a rate
#' @description
#' This function create upper and lower CI for observed rate.
#' Two new variables will be added LC and UC
#' the formula is based on 95% CI: rate+/- 1.96 x sqrt((!!var x (1-!!var))/!!n_cases)
#' 
#' 
#'
#' @param var variable you want to sort
#' @param n_cases number of cases used to calculate CI
#'
#' @examples
#'tb = tibble::tibble(cs_rate = c(0.1, 0.3),
#'                     n_pt_cases = c(140, 234))
#'tb %>% 
#'  add_CI_values(var = cs_rate,
#'                n_cases = n_pt_cases)


add_CI_values <- function(data,
                          var,
                          n_cases) {
  
  var <- enexpr(var)
  n_cases <- enexpr(n_cases)
  
  data %>% 
    mutate(
      LC = !!var - (1.96 * sqrt((!!var * (1-!!var))/!!n_cases)),
      UC = !!var + (1.96 * sqrt((!!var * (1-!!var))/!!n_cases)),
      LC = round(LC,4)*100,
      UC= round(UC,4)*100)
  
}
