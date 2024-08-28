#' Print HTML report to PDF
#' 
#' To print HTML report to a PDF, this function uses the `pagedown::chrome_print` function. The function adds a footer to the PDF that includes the BCBSM logo and language. The function also allows for the inclusion of page numbers in the PDF.
#' 
#' @param input_path A character string that is the file path of the html report.  
#' @param output_pdf_path A character string that is the file path of the final PDF report. 
#' @param top_margin A numeric that indicates the top margin of the PDF. Default is 0.4.
#' @param bottom_margin A numeric that indicates the bottom margin of the PDF. Default is 0.4.
#' @param page_number A logical that indicates if the page number should be included in the PDF. Default is FALSE.
#' @param page_range A character string that indicates the page range of the PDF. Default is "", which prings all pages. 
#'
#' @return Creates a PDF containing the BCBSM logo and language in a footnote. The file is a PDF copy of the HTML verson of the report.. 
#' @examples 
#' /dontrun{
#' print_obi_report("report.html", 
#' "PRO Patient Voices Report.pdf", 
#' top_margin = 0.4, 
#' bottom_margin = 0.4,
#' page_number = TRUE)
#' }
#' 
#' @export


print_obi_report <- function(input_path, 
                     output_pdf_path, 
                     top_margin = 0.4, 
                     bottom_margin = 0.4,
                     page_number = FALSE,
                     page_range = "") {
  
  footer <- htmltools::tags$footer(
    style = "width: 100%",
    htmltools::img(
      src = knitr::image_uri("reports/BCBSM BCN_ProcessBlue_B.png"), 
      style = 'display: inline-block; vertical-align: middle; line-height: 1; margin:10px 20px; height: 29px; width: 83.43px'
    ), htmltools::div(
      style = "display: inline-block; vertical-align: middle; line-height: 1; font-size: 6pt; width: 670px; font-style: italic; color: #ededed;  font-family: Arial", 
      "Support for the Obstetrics Initiative is provided by Blue Cross Blue Shield of Michigan and Blue Care Network as part of the BCBSM Value Partnerships program. Although Blue Cross Blue Shield of Michigan and the Obstetrics Initiative work collaboratively, the opinions, beliefs and viewpoints expressed by the author do not necessarily reflect the opinions, beliefs and viewpoints of BCBSM or any of its employees."
    )
  )
  
  
  if (page_number == TRUE) {
    header <- htmltools::div(
      style = "font-size: 6pt; text-align: right; width: 98%; padding-right: 12pt; color: #ededed; font-family: Arial", 
      htmltools::span(class = "pageNumber"), "/", htmltools::span(class = "totalPages")
    )
  } else {
    header <- htmltools::div(
      style = "font-size: 6pt; text-align: right; width: 98%; padding-right: 12pt; color: #ededed; font-family: Arial", 
      " "
    )
  }
  
  
  pagedown::chrome_print(
    input = here::here(input_path),
    output = here::here(output_pdf_path),
    options = list(
      displayHeaderFooter = TRUE, 
      headerTemplate = format(header, indent = FALSE),
      footerTemplate = format(footer, indent = FALSE),
      marginTop = top_margin,
      marginBottom = bottom_margin,
      pageRanges = page_range
    )
  )
}