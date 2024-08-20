


print_obi_report <- function(input_path, 
                     output_pdf_path, 
                     top_margin = 0.4, 
                     bottom_margin = 0.4,
                     page_number = FALSE,
                     page_range = NULL) {
  
  footer <- tags$footer(
    style = "width: 100%",
    img(
      src = knitr::image_uri("reports/BCBSM BCN_ProcessBlue_B.png"), 
      style = 'display: inline-block; vertical-align: middle; line-height: 1; margin:10px 20px; height: 29px; width: 83.43px'
    ), div(
      style = "display: inline-block; vertical-align: middle; line-height: 1; font-size: 6pt; width: 670px; font-style: italic; color: #ededed;  font-family: Arial", 
      "Support for the Obstetrics Initiative is provided by Blue Cross Blue Shield of Michigan and Blue Care Network as part of the BCBSM Value Partnerships program. Although Blue Cross Blue Shield of Michigan and the Obstetrics Initiative work collaboratively, the opinions, beliefs and viewpoints expressed by the author do not necessarily reflect the opinions, beliefs and viewpoints of BCBSM or any of its employees."
    )
  )
  
  
  if (page_number == TRUE) {
    header <- div(
      style = "font-size: 6pt; text-align: right; width: 98%; padding-right: 12pt; color: #ededed; font-family: Arial", 
      span(class = "pageNumber"), "/", span(class = "totalPages")
    )
  } else {
    header <- div(
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