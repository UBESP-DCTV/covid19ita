data_dictionary <- function() {

  # initial clean- and set-up ---------------------------------------
  cli::cat_rule("Dataset: TEMPLATE")

  # Detach all loaded packages and clean your environment
  invisible(golem::detach_all_attached())

  # Document and reload your package
  golem::document_and_reload()

  ## NOTE: all the needed packages must be included in the Imports (
  ##       if used actively in teh package) or in the Suggests list of
  ##       packages into the `DESCRIPTION` file. Here, call them using
  ##       the explicit notation `package::function()` without
  ##       `library()` any of them!




  # Code to prepare the data ----------------------------------------

  dictionary <- c(
    ricoverati_con_sintomi = "hospitalized_with_symptoms",
    terapia_intensiva = "intensive_care",
    totale_ospedalizzati = "total_hospitalized",
    isolamento_domiciliare = "isolation",
    totale_positivi = "active_cases",
    variazione_totale_positivi = "active_cases_variation",
    nuovi_positivi = "new_active_cases",
    dimessi_guariti = "recovered",
    totale_casi = "total_cases",
    deceduti = "deaths",
    casi_da_sospetto_diagnostico = "positive_cases_from_clinical_activity",
    casi_da_screening = "positive_cases_from_planned_screening",
    tamponi = "tests_performed",
    casi_testati = "people_tested",
    ingressi_terapia_intensiva = "admissions_in_intensive_care"
  )


  ui_done("dictionary ready")




  # Code to save the data -------------------------------------------

  ## Must be the names of the datasets!
  write_raw_rds(c(
    "dictionary"
  ))
}


data_dictionary()


## After executed the function (ie creating/updating the data) remember
## to update `data-INTERNAL.R`, and `data-EXPORTED.R` accordingly to
## store them in the correct place into the package's data.
##
## dictionary <- read_data_raw("dictionary")
