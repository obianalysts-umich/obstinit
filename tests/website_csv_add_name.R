
library(obstinit)
library(tidyverse)

# add site name to match website name ----------------------------------

# must match the site name on the website
new_site_name <- "McLaren Northern Michigan (Petoskey)"

site_names <- site_names |> 
  rbind(
    tibble(
      site_name = new_site_name,
    )
  )


# get mdhhs id from obi data ----------------------------------------------

obi_dt <- create_obi_cohort(read_current_data())

new_mdhhs <- obi_dt |> 
  filter(grepl("Petoskey", site_name)) |>
  distinct(site_name, external_mdhhs_site_id, site_id) |> 
  rename(
    site_name = site_name,
    mdhhs_id = external_mdhhs_site_id,
    site_id_a_mx = site_id
  )
  
site_names_mdhhs <- site_names_mdhhs |> 
  rbind(new_mdhhs)


# documentation -----------------------------------------------------------

# update R/ddata.R to list updates
usethis::use_data(site_names, overwrite = T)
usethis::use_data(site_names_mdhhs, overwrite = T)
devtools::document()


# test process ------------------------------------------------------------
create_website_upload_csv(
  report_name = "test", 
  tags = "Other", 
  output_path = "/Users/rebeccawardrop/Desktop/"
)
