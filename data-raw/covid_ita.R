## code to prepare `covid_ita` dataset goes here
requireNamespace("purrr", quietly = TRUE)
requireNamespace("readr", quietly = TRUE)
library(covid19ita)

data_levels <- list("italia", "regioni", "province")
dest_dir <- tempdir()

are_ok <- purrr::map2_lgl(data_levels, data_levels,
    ~ download_dpc(.x,  dir = dest_dir, file_name = paste0(.y, ".csv"))
)

if (!all(are_ok)) {
  usethis::ui_oops("Some download had error. No data are imported in the package.")
} else {

  plottly_help_txt <- HTML(
    "Tramite i pulsanti in semi-trasparenza è possibile:</br>
      - zoommare (anche a selezione)</br>
      - deselezionare livelli (singolo click)</br>
      - isolare un livello (doppio click)</br>
      - dettagli multipli (doppia linguetta)</br>
      - esportazione (macchina foto)</br>
  ")


  usethis::ui_done("All data correctly downloaded")

  covid_ita <- file.path(dest_dir, paste0(data_levels, ".csv")) %>%
      purrr::set_names(data_levels) %>%
      purrr::map(readr::read_csv)

  dpc_covid19_ita_andamento_nazionale <- covid_ita[["italia"]]
  dpc_covid19_ita_regioni             <- covid_ita[["regioni"]]
  dpc_covid19_ita_province            <- covid_ita[["province"]]

  last_data_update <- lubridate::now()

  usethis::use_data(
    dpc_covid19_ita_andamento_nazionale,
    dpc_covid19_ita_regioni,
    dpc_covid19_ita_province,
    overwrite = TRUE
  )
  usethis::use_data(
    plottly_help_txt,
    dpc_covid19_ita_andamento_nazionale,
    dpc_covid19_ita_regioni,
    dpc_covid19_ita_province,
    last_data_update,
    internal = TRUE,
    overwrite = TRUE
  )

}


