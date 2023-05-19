#' sort multiple selected varible as list
#' @description
#' multiple selected varibales shouldn't have different values based on order
#' for example, {1|2} euqal to {2|1}. This function short the numbers
#' 
#'
#' @param var variable you want to sort
#'
#' @examples
#' val = "8,1,0,3"
#' sort_muti_selection(val)

sort_muti_selection <- function(var) {
  set = as.numeric(unlist(str_extract_all(var, "\\d+")))
  paste(sort(set), collapse = ",")
}


#' sort multiple selected variable in dataset
#' @description
#' multiple selected varibales shouldn't have different values based on order
#' for example, {1|2} euqal to {2|1}. This function short the numbers
#' 
#'
#' @param var variable you want to sort
#' @export
#'
#' @examples
#' df <- tibble(col = c("{1|2}", "{2|1}", "{3|2|1}", "{2|3|1}", "{2|1|3}"))
#' df %>% mutate(new = sort_muti_selection_var(col))


sort_muti_selection_var <- function(var) {
  lapply({{var}}, sort_muti_selection) %>% unlist(.)
}