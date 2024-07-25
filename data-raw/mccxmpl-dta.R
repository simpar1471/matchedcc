mccxmpl <- haven::read_dta("data-raw/mccxmpl.dta") |>
  dplyr::mutate(dplyr::across(tidyselect::everything(), .fns = as.integer))

usethis::use_data(mccxmpl, overwrite = TRUE)
