# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("stringr")
usethis::use_package("purrr", type = "Suggests")
usethis::use_package("readr", type = "Suggests")
usethis::use_tibble()
usethis::use_package("plotly")
usethis::use_package("htmlwidgets")
usethis::use_package("leaflet")
usethis::use_package("RColorBrewer")
usethis::use_package("RJSONIO")
usethis::use_package("RCurl")
usethis::use_package("httr")
usethis::use_package("slider")
usethis::use_package("shinyjs")
usethis::use_package("shinyWidgets")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "ts_ita")
golem::add_module(name = "ts_reg")
golem::add_module(name = "ts_prv")
golem::add_module(name = "info_header")
golem::add_module(name = "img_header")
golem::add_module(name = "help_plot")
golem::add_module(name = "focus_20200314")
golem::add_module(name = "ind_ita")
golem::add_module(name = "focus_20200318_friuli")
golem::add_module(name = "focus_20200318_piemonte")
golem::add_module(name = "focus_20200318_veneto_intensive")
golem::add_module(name = "focus_20200320_novara")
golem::add_module(name = "focus_20200323_picco")
golem::add_module(name = "focus_20200325")
golem::add_module(name = "focus_20200327")
golem::add_module(name = "focus_20200331")
golem::add_module(name = "focus_20200404_magnani")
golem::add_module(name = "focus_20200406_mort_veneto")
golem::add_module(name = "maps")
golem::add_module(name = "focus_20200415_tamponi")
golem::add_module(name = "icuve_ts")


## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("download_dpc")
golem::add_utils("helpers")
golem::add_utils("render_box_using")
golem::add_utils("extract_ci_from_gg_txt")
golem::add_fct("cumulate_for_days")
golem::add_utils("pred_to_tbl")
golem::add_fct("regional_poiss")
golem::add_fct("mort_data_reg")
golem::add_fct("mort_data_comuni")
golem::add_fct("mort_data_veneto")
golem::add_utils("plot_mort_data")
golem::add_fct("clean_ggplotly")
golem::add_utils("write_raw_rds")
golem::add_fct("pull_region_w_pop")
golem::add_fct("update_supersecrets")


## Update package documentation and namespace
devtools::document(roclets = c('rd', 'collate', 'namespace'))


## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file( "script" )
# golem::add_js_handler( "handlers" )
# golem::add_css_file( "custom" )

## Tests ----
## Add one line by test you want to create
usethis::use_test("download_dpc")
usethis::use_test("helper_dashboard")
usethis::use_test("plot_and_table_alignment")
# Documentation

## Spellcheck
usethis::use_spell_check()

## Vignette ----
# usethis::use_vignette("covid19ita")
# devtools::build_vignettes()

## Quality ----

## styler
#
## run this only once!!
{# Create configuration file for lintr
  # Source this file in package root directory
  # List here files to exclude from lint checking, as a character vector
excluded_files <- c(
  list.files("data",      recursive = TRUE, full.names = TRUE),
  list.files("docs",      recursive = TRUE, full.names = TRUE),
  list.files("inst/doc",  recursive = TRUE, full.names = TRUE),
  list.files("man",       recursive = TRUE, full.names = TRUE),
  list.files("pkgdown",   recursive = TRUE, full.names = TRUE),
  list.files("vignettes", recursive = TRUE, full.names = TRUE)
)

my_linters  <-  lintr::with_defaults(
  line_length_linter = lintr::line_length_linter(72),
  T_and_F_symbol_linter = lintr::T_and_F_symbol_linter,
  closed_curly_linter = lintr::closed_curly_linter(
    allow_single_line = TRUE
  ),
  extraction_operator_linter = lintr::extraction_operator_linter,
  absolute_path_linter = lintr::absolute_path_linter,
  nonportable_path_linter = lintr::nonportable_path_linter,
  semicolon_terminator_linter = lintr::semicolon_terminator_linter,
  undesirable_function_linter = lintr::undesirable_function_linter,
  undesirable_operator_linter = lintr::undesirable_operator_linter,
  unneeded_concatenation_linter = lintr::unneeded_concatenation_linter,
  object_usage_linter = NULL
)

  ### Do not edit after this line ###

  library(magrittr)
  library(dplyr)

  # Make sure we start fresh
  if (file.exists(".lintr")) { file.remove(".lintr") }

  # List current lints
  lintr::lint_package(
      linters = my_linters,
      exclusions = excluded_files
    ) %>%
    as.data.frame %>%
    group_by(linter) %>%
    tally(sort = TRUE) %$%
    sprintf(
      "linters: with_defaults(\n    %s\n    dummy_linter = NULL\n  )\n",
      paste0(linter, " = NULL, # ", n, collapse = "\n    ")
    ) %>%
    cat(file = ".lintr")

  sprintf(
    "exclusions: list(\n    %s\n  )\n",
    paste0('"', excluded_files, '"', collapse = ",\n    ")
  ) %>%
    cat(file = ".lintr", append = TRUE)

  # Clean up workspace
  remove(excluded_files)
}

## after setup of .lint file
lintr::lint_package()

# styler::style_pkg(exclude_files = excluded_files)


# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

