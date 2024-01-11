#' Automatically create folders for a project
#'
#' @details Automatically generate a project
#'     folder structure and .ignore in the working directory of your R session.
#'     
#' @export
create_project_folder <- function() {
  base::dir.create("code")
  base::dir.create("data_public")
  base::dir.create("images")
  base::dir.create("other_docs")
  message(paste("The following directories have been created in: ", getwd(), ".", sep=""))
  message("- code")
  message("- data_public")
  message("- images")
  message("- other_docs")
  

  # create .gitignore -----------------------------------------------------------
  ignore_path <- file.path(getwd(), ".gitignore")
  ignore_template <- gitignore::gi_fetch_templates("R")
  
  gitignore::gi_write_gitignore(fetched_template = ignore_template,  gitignore_file = ignore_path)
  
}