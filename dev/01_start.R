# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
golem::fill_desc(
  pkg_name = "covid19ita", # The Name of the package containing the App
  pkg_title = "Covid-19 - Italy", # The Title of the package containing the App
  pkg_description = "Monitoring platform for Covid-19 infection diffusion in Italy.", # The Description of the package containing the App
  author_first_name = "Corrado", # Your First Name
  author_last_name = "Lanera", # Your Last Name
  author_email = "corrado.lanera@unipd.it", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional)
)

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_ccby_license( name = "UBEP" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_cran_badge()
usethis::use_news_md( open = FALSE )
usethis::use_package_doc()

## Use git ----
usethis::use_git()
usethis::git_vaccinate()
## commit before to continue...
usethis::use_github( "UBESP-DCTV" )
usethis::use_tidy_github()



## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()
usethis::use_roxygen_md()
usethis::use_pipe()

## Favicon ---- (changed the inst/app/www/favicon.ico to the ubep's one)
# If you want to change the favicon (default is golem's one)
# golem::remove_favicon()
# golem::use_favicon() # path = "path/to/ico". Can be an online file.
usethis::use_logo( "favicon-96x96.png" )

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()


## CI ----
usethis::use_appveyor()
usethis::use_tidy_ci()


## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "covid_ita" )

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

