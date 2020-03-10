## code to prepare `covid_ita` dataset goes here
requireNamespace("purrr", quietly = TRUE)
requireNamespace("readr", quietly = TRUE)
requireNamespace("here", quietly = TRUE)
library(covid19ita)

data_levels <- list("italia", "regioni", "province")

purrr::walk(data_levels, download_dpc, dir = here::here("data-raw"))

data_paths <- here::here("data-raw", paste0(data_levels, ".csv"))
covid_ita  <- purrr::map(data_paths, readr::read_csv)

usethis::use_data(covid_ita, overwrite = TRUE)
