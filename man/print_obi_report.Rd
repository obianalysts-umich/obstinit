% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_obi_report.R
\name{print_obi_report}
\alias{print_obi_report}
\title{Print HTML report to PDF}
\usage{
print_obi_report(
  input_path,
  output_pdf_path,
  top_margin = 0.4,
  bottom_margin = 0.5,
  page_number = FALSE,
  page_range = "",
  phe_footer = FALSE
)
}
\arguments{
\item{input_path}{A character string that is the file path of the html report.}

\item{output_pdf_path}{A character string that is the file path of the final PDF report.}

\item{top_margin}{A numeric that indicates the top margin of the PDF. Default is 0.4.}

\item{bottom_margin}{A numeric that indicates the bottom margin of the PDF. Default is 0.5.}

\item{page_number}{A logical that indicates if the page number should be included in the PDF. Default is FALSE.}

\item{page_range}{A character string that indicates the page range of the PDF. Default is "", which prings all pages.}

\item{phe_footer}{A logical that indicates if the PHE footer should be included in the PDF. Default is FALSE.}
}
\value{
Creates a PDF containing the BCBSM logo and language in a footnote. The file is a PDF copy of the HTML verson of the report..
}
\description{
To print HTML report to a PDF, this function uses the \code{pagedown::chrome_print} function. The function adds a footer to the PDF that includes the BCBSM logo and language. The function also allows for the inclusion of page numbers in the PDF.
}
\examples{
/dontrun{
print_obi_report("report.html", 
"PRO Patient Voices Report.pdf", 
top_margin = 0.4, 
bottom_margin = 0.5,
page_number = TRUE)
}

}
