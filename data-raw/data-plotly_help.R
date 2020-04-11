data_plotly_help <- function() {

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

  eng_plottly_help_txt <- shiny::HTML(
    "Click on the semi-trasparent icons to:</br>
      - zoom </br>
      - remove levels (one click)</br>
      - keep only one level (double click)</br>
      - multiple details (tab)</br>
      - export images (camera)</br>
  ")
  ui_done("eng_plottly_help_txt ready")

  plottly_help_txt <- shiny::HTML(
    "Tramite i pulsanti in semi-trasparenza è possibile:</br>
      - zoommare (anche a selezione)</br>
      - deselezionare livelli (singolo click)</br>
      - isolare un livello (doppio click)</br>
      - dettagli multipli (doppia linguetta)</br>
      - esportazione (macchina foto)</br>
  ")
  ui_done("plottly_help_txt ready")





  # Code to save the data -------------------------------------------

  ## Must be the names of the datasets!
  write_raw_rds(c(
    "eng_plottly_help_txt",
    "plottly_help_txt"
  ))

}


data_plotly_help()


## After executed the function (ie creating/updating the data) remember
## to update `data-INTERNAL.R`, and `data-EXPORTED.R` accordingly to
## store them in the correct place into the package's data.
##
## eng_plottly_help_txt <- read_data_raw("eng_plottly_help_txt")
## plottly_help_txt <- read_data_raw("plottly_help_txt")
