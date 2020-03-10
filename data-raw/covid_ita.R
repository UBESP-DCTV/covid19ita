## code to prepare `covid_ita` dataset goes here
requireNamespace("purrr", quietly = TRUE)
requireNamespace("readr", quietly = TRUE)
library(covid19ita)

data_levels <- list("italia", "regioni", "province")
dest_dir <- tempdir()

are_ok <- purrr::map_lgl(data_levels, download_dpc, dir = dest_dir)

if (all(are_ok)) {

  usethis::ui_done("All data correctly downloaded")

  covid_ita <- file.path(dest_dir, paste0(data_levels, ".csv")) %>%
      purrr::set_names(data_levels) %>%
      purrr::map(readr::read_csv)

  covid_stato    <- covid_ita[["italia"]]
  covid_regioni  <- covid_ita[["regioni"]]
  covid_province <- covid_ita[["province"]]

  usethis::use_data(covid_stato, covid_regioni, covid_province,
    overwrite = TRUE
  )

} else {
  usethis::ui_oops("Some download had error. No data are imported in the package.")
}


