# covid19ita 0.10.2

* Update wordlist
* Set up for English
* Modified `run_app()` to accept a language argument to run the app in the 
  proper language. Possible option are "ita" for Italian language and "eng" for
  English language (default = "ita" to maintain compatibility).
* removed metathis from the dependencies
* duplicated files for English version (#4)

# covid19ita 0.10.1

* Current positive to the sidebar

# covid19ita 0.10.0

* minor to fix CMD notes
* Added contribution
* note landscape for non-PC devices
* minor label update
* data update (and fix (#3))
* incorporate `selectize = TRUE` in the option for choices
* removed old modules impact Veneto

# covid19ita 0.9.0

* Fixed typo in UI (issue #1)
* Added national indices

# covid19ita 0.8.2

* minor fix to metadata

# covid19ita 0.8.1

* minor fix to pass CMD
* Added metadata for social link sharing

# covid19ita 0.8.0

* Added content for "In evidenza"
* Added modules for insights 202003214

# covid19ita 0.7.3

* Data update

# covid19ita 0.7.2

* Added meta title

# covid19ita 0.7.1

* dashboard title page
* default for Italy
* fix google analytics tag

# covid19ita 0.7.0

* Added home page
* Reshaping menus and credits

# covid19ita 0.6.2

* fix internal data saving
* Add help for plottly

# covid19ita 0.6.1

* fix encoding issues
* added credits to R packages

# covid19ita 0.6.0

* added impact plots for Veneto region
* removed first page regional intensive care

# covid19ita 0.5.4

* Add plot for regional intensive care
* Added `last_data_update` internal object
* Added info summary module for sidebar
* Fix delta computation events provincial level
* Added @berkeley3, and G. Lorenzoni to contributors
* Added module for impact predictive models (Veneto region)

# covid19ita 0.5.3

* Data update
* Restored original `appveyor.yml` (from `{usethis}`)
* Added ORCID to `DESCRIPTION`

# covid19ita 0.5.1

* Add info on datasets in UI and `README`
* Update "Last update"s in README

# covid19ita 0.5.0


* Update the project to maturing lifecycle (and badge in the `README`)
* Added lifecycle support from `{lifecycle}` package
* Fix screenshot view

# covid19ita 0.4.3

* Add app screenshot to the `README`

# covid19ita 0.4.2

* Fix region -> province in UI

# covid19ita 0.4.1

* Added provincial UI

# covid19ita 0.4.0

* Added provincial module
* National module converted in italian
* Menu and title in italian

# covid19ita 0.3.2

* Uniform language UI (italian)

# covid19ita 0.3.1

* Increase grid plot width

# covid19ita 0.3.0

* App update with regional
* Added regional module
* Data Update
* Added link to the source code project on GitHub
* Added body tabs related to the options in the sidebar

# covid19ita 0.2.0

* Data update
* Activated automatic spellcheck (`{spellcheck}` package + italian words
  used added to the custom dictionary)
* Fixed License and dependencies in `DESCRIPTION`
* Update import for internal data usage too
* Include shinydashboard interface
* Added National time series module

# covid19ita 0.1.2

* Update data to dpc naming

# covid19ita 0.1.1

* Added "last data update" in the README

# covid19ita 0.1.0

* provided and documented `covid_stato`, `covid_province`, and
  `covid_regioni` dataset
* defined `download_dpc()` function to download dcp data from its
  official GitHub repo.

# covid19ita (development version)

* Setup basic dev tools and settings (see `dev/01_start.R`).

# covid19ita 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
