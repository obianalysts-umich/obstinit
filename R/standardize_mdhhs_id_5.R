#' Normalize MDHHS IDs to 5-digit format
#'
#' This function normalizes Michigan Department of Health and Human Services (MDHHS) IDs to a standard 5-digit format.
#' - If the ID has 4 digits, a leading zero is added.
#' - If the ID has 6 digits, the first digit is removed. (SS format)
#' - IDs with 5 digits are left unchanged.
#'
#' @param id A character vector or numeric vector of MDHHS IDs.
#'
#' @return A character vector of normalized 5-digit MDHHS IDs.
#'
#' @examples
#' tibble(id = c("1234", "01234", "101234", "56789", "123456")) |>
#'   mutate(mdhhs_id = standardize_mdhhs_id_5(id))
#'
#' @export
standardize_mdhhs_id_5 <- function(id) {
  id <- as.character(id)
  # If 4 digits, add leading 0
  changed_4 <- nchar(id) == 4
  if (any(changed_4)) {
    message(
      "IDs with 4 digits changed: ", paste(id[changed_4], collapse = ", "),
      " -> ", paste(paste0("0", id[changed_4]), collapse = ", ")
    )
  }
  id <- ifelse(changed_4, paste0("0", id), id)

  # If 6 digits, remove the first digit
  changed_6 <- nchar(id) == 6
  if (any(changed_6)) {
    message(
      "IDs with 6 digits changed: ", paste(id[changed_6], collapse = ", "),
      " -> ", paste(substr(id[changed_6], 2, 6), collapse = ", ")
    )
  }
  id <- ifelse(changed_6, substr(id, 2, 6), id)
  id
}
