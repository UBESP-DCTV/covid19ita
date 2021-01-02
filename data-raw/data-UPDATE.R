source(here::here("data-raw", "data-dpc_covid19_ita.R"), local = TRUE)
data_dpc()


source(here::here("data-raw", "data-INTERNAL.R"), local = TRUE)
source(here::here("data-raw", "data-EXPORTED.R"), local = TRUE)

rm(list = ls(all.names = TRUE))

devtools::check_man()

here::here("R") %>%
  list.files(pattern = ".R$", full.names = TRUE) %>%
  purrr::walk(tools::showNonASCIIfile)


{
  options(golem.app.prod = FALSE)
  golem::detach_all_attached()
  golem::document_and_reload()
  run_app(language = "ita")
}


# devtools::check()
