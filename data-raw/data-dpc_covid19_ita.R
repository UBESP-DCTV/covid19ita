data_dpc <- function() {

  # initial clean- and set-up ---------------------------------------
  cli::cat_rule("Dataset: DPC CoViD-19 Ita")

  # Code to prepare the data ----------------------------------------

  data_levels <- list("italia", "regioni", "province")
  dest_dir <- tempdir()

  are_ok <- purrr::map_lgl(
    .x = data_levels,
    ~ covid19ita::download_dpc(.x,
                               dir = dest_dir,
                               file_name = paste0(.x, ".csv")
    )
  )

  if (all(are_ok)) {
    usethis::ui_done("All data correctly downloaded")
  } else {
    usethis::ui_oops(
      "Download error. No data are imported in the package."
    )
  }


  covid_ita <- file.path(dest_dir, paste0(data_levels, ".csv")) %>%
    purrr::set_names(data_levels) %>%
    purrr::map(readr::read_csv, guess_max = 3000)

  dpc_covid19_ita_andamento_nazionale <- covid_ita[["italia"]]
  dpc_covid19_ita_regioni <- covid_ita[["regioni"]]
  dpc_covid19_ita_province <- covid_ita[["province"]]

  last_data_update <- lubridate::now()



  # Code to save the data -------------------------------------------

  covid19ita:::write_raw_rds(c(
    "dpc_covid19_ita_andamento_nazionale",
    "dpc_covid19_ita_regioni",
    "dpc_covid19_ita_province",
    "last_data_update"
  ))
}
## After executed the function (ie creating/updating the data) remember
## to update `data-INTERNAL.R`, and `data-EXPORTED.R` accordingly to
## store them in the correct place into the package's data.
