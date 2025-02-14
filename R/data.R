#' SMM complication indications map
#'
#' Used in nightly export data processing to create separate indicator variables for each SMM complication indication.
#'
#' @format ## `smm_cmp_indications_map`
#' A data frame with 2 columns:
#' \describe{
#'   \item{value}{numeric value}
#'   \item{indication}{value label text}
#' }

"smm_cmp_indications_map"


#' Site name list for website upload
#'
#' Used in create_website_upload_csv() as a list of site names. Update 11/07/24 to change Ascensions to Henry Fords. Update 2/14/25 to correct Marshfield Dickinson.
#'
#' @format ## `site_names`
#' A data frame with 1 column:
#' \describe{
#'   \item{site_name}{character string of site name}
#' }

"site_names"

#' Site MDHHS ID list
#'
#' Used in create_website_upload_csv() to map site names to MDHHS IDs. Update 11/07/24 to change Ascensions to Henry Fords. Update 2/14/25 to correct Marshfield Dickinson.
#'
#' @format ## `site_names`
#' A data frame with 3 columns:
#' \describe{
#'   \item{site_name}{character string of site name}
#'   \item{site_id_a_mx}{double AMx ID}
#'   \item{mdhhs_id}{character string MDHHS ID}
#' }

"site_names_mdhhs"