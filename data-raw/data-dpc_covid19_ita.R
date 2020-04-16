data_dpc <- function() {

  # initial clean- and set-up ---------------------------------------
  cli::cat_rule("Dataset: DPC CoViD-19 Ita")

  # Detach all loaded packages and clean your environment
  invisible(golem::detach_all_attached())
  rm(list = ls(all.names = TRUE), inherits = TRUE)

  # Document and reload your package
  golem::document_and_reload()

  ## NOTE: all the needed packages must be included in the Imports (
  ##       if used actively in teh package) or in the Suggests list of
  ##       packages into the `DESCRIPTION` file. Here, call them using
  ##       the explicit notation `package::function()` without
  ##       `library()` any of them!



  # Code to prepare the data ----------------------------------------

  data_levels <- list("italia", "regioni", "province")
  dest_dir <- tempdir()

  are_ok <- purrr::map2_lgl(
    .x = data_levels,
    .y = data_levels,
    ~ download_dpc(.x, dir = dest_dir, file_name = paste0(.y, ".csv"))
  )

  if (!all(are_ok)) {
    ui_oops(
      "Download error. No data are imported in the package."
    )
  } else {
    ui_done("All data correctly downloaded")
  }


  covid_ita <- file.path(dest_dir, paste0(data_levels, ".csv")) %>%
    purrr::set_names(data_levels) %>%
    purrr::map(readr::read_csv)

  dpc_covid19_ita_andamento_nazionale <- covid_ita[["italia"]]
  dpc_covid19_ita_regioni <- covid_ita[["regioni"]]
  dpc_covid19_ita_province <- covid_ita[["province"]]

  last_data_update <- lubridate::now()



  # Code to save the data -------------------------------------------

  write_raw_rds(c(
    "dpc_covid19_ita_andamento_nazionale",
    "dpc_covid19_ita_regioni",
    "dpc_covid19_ita_province",
    "last_data_update"
  ))
}


data_dpc()

## After executed the function (ie creating/updating the data) remember
## to update `data-INTERNAL.R`, and `data-EXPORTED.R` accordingly to
## store them in the correct place into the package's data.
