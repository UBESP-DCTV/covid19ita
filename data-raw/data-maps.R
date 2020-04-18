data_maps <- function() {

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

  palette_list_t <- list(
    Person = c(
      "#cccccc", "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4",
      "#1d91c0", "#225ea8", "#6e016b", "#990000", "#d7301f", "#FF0000"
    ),
    Spectral = c("#cccccc", rev(grDevices::rainbow(20)[1:12])),
    WhiteOrangeRed = c('#FFFFFF', RColorBrewer::brewer.pal(9, "Reds") ),
    RedWhiteBlue = RColorBrewer::brewer.pal(11, "RdBu"),
    BlueWhiteRed = rev(RColorBrewer::brewer.pal(11, "RdBu")),
    RedWhiteGrey = rev(RColorBrewer::brewer.pal(11, "RdGy"))
  )
  ui_done("palette_list_t ready")




  # Code to save the data -------------------------------------------

  ## Must be the names of the datasets!
  write_raw_rds(c(
    "palette_list_t"
  ))
}


data_maps()


## After executed the function (ie creating/updating the data) remember
## to update `data-INTERNAL.R`, and `data-EXPORTED.R` accordingly to
## store them in the correct place into the package's data.
##
#> palette_list_t <- read_data_raw("palette_list_t")
