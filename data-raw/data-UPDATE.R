library(tidyverse)
source(here::here("data-raw", "data-dpc_covid19_ita.R"), local = TRUE)
data_dpc()


source(here::here("data-raw", "data-INTERNAL.R"), local = TRUE)
source(here::here("data-raw", "data-EXPORTED.R"), local = TRUE)

rm(list = ls(all.names = TRUE))

devtools::check_man()
res_check <- devtools::check()


if (length(res_check$errors) == 0L & length(res_check$warnings) == 0L) {

  nws <- here::here("NEWS.md")
  readr::write_lines(
    c(list(
      "* data update",
      ""
    ), readr::read_lines(file = nws)),
    file = nws
  )

  git2r::commit(message = "Data update", all = TRUE)

  usethis::use_version("minor")
}
