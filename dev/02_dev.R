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


## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("download_dpc")
golem::add_utils("helpers")
golem::add_utils("render_box_using")
golem::add_utils("extract_ci_from_gg_txt")
golem::add_fct("cumulate_for_days")
golem::add_utils("pred_to_tbl")
golem::add_fct("regional_poiss")



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

# Documentation

## Spellcheck
usethis::use_spell_check()

## Vignette ----
# usethis::use_vignette("covid19ita")
# devtools::build_vignettes()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

