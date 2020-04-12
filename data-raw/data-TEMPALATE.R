data_TEMPLATE <- function() {

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

  template_dataset_1 <- "7"
  ui_done("template_dataset_1 ready")


  template_dataset_2 <- lubridate::now()
  ui_done("template_dataset_2 ready")




  # Code to save the data -------------------------------------------

  ## Must be the names of the datasets!
  write_raw_rds(c(
    "template_dataset_1",
    "template_dataset_2"
  ))

}


data_TEMPLATE()


## After executed the function (ie creating/updating the data) remember
## to update `data-INTERNAL.R`, and `data-EXPORTED.R` accordingly to
## store them in the correct place into the package's data.
##
## template_dataset_1 <- read_data_raw("template_dataset_1")
## template_dataset_2 <- read_data_raw("template_dataset_2")
