#' sort multiple selected varible as list
#' @description
#' multiple selected varibales shouldn't have different values based on order
#' for example, {1|2} euqal to {2|1}. This function short the numbers
#'
#'
#' @param var variable you want to sort
#'
#' @examples
#' val <- "8,1,0,3"
#' sort_muti_selection(val)
sort_muti_selection <- function(var) {
  set <- as.numeric(unlist(str_extract_all(var, "\\d+")))
  paste(sort(set), collapse = ",")
}


#' sort multiple selected variable in dataset
#' @description
#' multiple selected varibales shouldn't have different values based on order
#' for example, {1|2} euqal to {2|1}. This function short the numbers
#'
#' @param var variable you want to sort
#' @export
#'
#' @examples
#' df <- tibble(col = c("{1|2}", "{2|1}", "{3|2|1}", "{2|3|1}", "{2|1|3}"))
#' df %>% mutate(new = sort_muti_selection_var(col))
sort_muti_selection_var <- function(var) {
  lapply({{ var }}, sort_muti_selection) %>% unlist(.)
}



#' @title Add individual procedure indicator flag vars
#'
#' @param data OBI data
#' @param map indication map; e.g. see data(smm_cmp_indications_map)
#' @param multiselect_var variable name; e.g. severe_procedure_e, variables ends with _e
#' @param var_suffix indication variable name suffix, e.g. "_flg"
#'
#' @details This function takes a muti-select variable for example severe_procedure_e and creates flags for each indication
#'          The value to label mapping is defined in *_indications_map.csv
#' @examples /dontrun{
#' obi_dt = readRDS("/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/output/merged_add_data_sources.Rds")
#' data(smm_cmp_indications_map)
#' add_indicator_flgs_based_on_multiselect_var(obi_dt,
#'                                             map = smm_cmp_indications_map,
#'                                             severe_procedure_e,
#'                                             "_smm_cdc_flg")
#' }
#'
#' @export
add_indicator_flgs_based_on_multiselect_var <- function(data,
                                                        map = NULL,
                                                        multiselect_var = NULL,
                                                        var_suffix = "_smm_cdc_flg") {
  # process map label -------------------------------------------------------
  if (is.null(map)) {
    stop("map is required")
  } else {
    map <- map |>
      mutate(
        indication_lbl = janitor::make_clean_names(indication),
        indication_lbl = paste0(indication_lbl, var_suffix)
      )
  }


  # get indicators list combinations -----------------------------
  list <- data |>
    count({{ multiselect_var }}) |>
    mutate(
      ind_list = obstinit::sort_muti_selection_var({{ multiselect_var }}),
      ind_list = strsplit(ind_list, ",")
    ) |>
    select(-n)

  # long format of indicators list -----------------------------
  list_long <- list |>
    unnest(cols = c(ind_list))

  # add labels to indicators list
  list_long_lbl <- list_long |>
    mutate(ind_list = as.numeric(ind_list)) |>
    left_join(map, by = c("ind_list" = "value"))

  # wide format of indicators label for each combinations in OBI data
  label_wide <- list_long_lbl |>
    select({{ multiselect_var }}, indication_lbl) |>
    mutate(indication_lbl = factor(indication_lbl, levels = unique(map$indication_lbl))) |>
    mutate(ind_value = 1) |>
    pivot_wider(
      names_from = indication_lbl,
      values_from = ind_value,
      # This results in all columns in indication factor level
      names_expand = TRUE,
      values_fill = 0
    )

  # add separate indicator vars to data -----------------------------
  # bare name to quote
  quo_name <- quo_name(enquo(multiselect_var))

  data |>
    left_join(label_wide, by = quo_name)
}
