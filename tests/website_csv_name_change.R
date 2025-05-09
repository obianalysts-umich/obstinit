library(obstinit)
library(tidyverse)

# change site name to match website name ----------------------------------
site_names |> filter(grepl("Jackson", site_name)) |> distinct(site_name) |> select(site_name)

site_names <- site_names |> 
  dplyr::mutate(
    site_name = dplyr::case_when(
      site_name == "Henry Ford Allegiance Health Jackson" ~ "Henry Ford Jackson Hospital",
      TRUE ~ site_name
    )
  )

site_names_mdhhs |> filter(grepl("Jackson", site_name)) |> distinct(site_name) |> select(site_name)

site_names_mdhhs <- site_names_mdhhs |> 
  dplyr::mutate(
    site_name = dplyr::case_when(
      site_name == "Henry Ford Allegiance Health Jackson" ~ "Henry Ford Jackson Hospital",
      TRUE ~ site_name
    )
  )

usethis::use_data(site_names, overwrite = T)
usethis::use_data(site_names_mdhhs, overwrite = T)
devtools::document()


# test process ------------------------------------------------------------
create_website_upload_csv(
  report_name = "test", 
  tags = "Other", 
  output_path = "/Users/rebeccawardrop/Desktop/"
)
