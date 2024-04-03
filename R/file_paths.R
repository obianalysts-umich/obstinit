
# summary -----------------------------------------------------------------
# this script contains all scripts related to creating paths


#' @title Create Dropbox root path
#' @description
#' Based in PC or Mac. If on PC, specify the unique name of the PC user.
#' 
#'
#' @param pc_unique_name your unique name. This will be used to find your account on PC. 
#'    No need to input this if using Mac
#'
#' @export
#'

dropbox_root_path <- function(pc_unique_name = "ourdea") {
  if (Sys.info()["sysname"] == "Windows") {
    user_dir <- fs::dir_ls("C:/Users")
    user <- user_dir[grep(pc_unique_name, user_dir)]
  } else if (Sys.info()["sysname"] == "Darwin") {
    user <- "~"
  }
  

  # testing -----------------------------------------------------------------
  if (file.exists(user) == FALSE) {
    stop(paste0("Path doesn't exist at ", user))
  } else {
    # retrun valid path
    cli::cli_inform(paste0("Your dropbox path is set as ", user))
    user
  }

}


#' @title OBI Turbo root path
#' @description Turbo path to nur-kanelow Turbo drive
#'
#' @export
#'
turbo_root_path <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    turbo_root <- "P:/"
  } else if (Sys.info()["sysname"] == "Darwin") {
    turbo_root <- "/Volumes/nur-kanelow/"
  }
  
  # testing -----------------------------------------------------------------
  if (file.exists(turbo_root) == FALSE) {
    cli::cli_abort(c("Path doesn't exist at ", turbo_root,
                     "i" = "Are you connect to VPN?"))
  } else {
    # retrun valid path
    cli::cli_inform(paste0("Your turbo path is set as ", turbo_root))
    turbo_root
  }
}

