# covid19ita 0.114.1

* update `Dockerfile`
* added delay for the shock in the exogen model
* data update

# covid19ita 0.114.0

* added ICU exogen model with shock on non critical hospitalizations

# covid19ita 0.113.0

* Added `mod_reg_tsicuve_noncritical` to show timeseries and the 
  corresponding ETS forecasting for non-critical hospitalizations.

# covid19ita 0.112.0

* Added ets prediction to siterep (including estimation up to 15 days
  and CI95% up to 4 days).
* added provincial and centre level at siterep.

# covid19ita 0.111.0

* Added `mod_reg_tsicuve` to section *Terapie intensive*
* Moved module `mod_ts_icuve`to section "In evidenza"
* Fix non-ASCII characters
* Update WORDLIST
* Move `{covid19.icuve}` into **Enhances** to avoid its installation 
  even when installed with `dependencies = TRUE` (solves #89).

# covid19ita 0.110.0

* Remove `ylim` in `ggbeds` in `mod_icuve_ts`
* Solved the problem with `mod_icuve_ts` at line 186 (#86)
* Solved the problem with default account (#86)
* Solve automatic update of Figure 3 in `mod_icuve_ts` (#86)
* Fixed 1/2 of #85
* Fixed problem with `ggswab` (#84)
* data update
* Integrated login credential in the main app
* New module `mod_tsicuve` that shows forecasts of Time-Series models
  applied to ICU data from the Veneto region. The following Time-Series
  models were considered: Holter-Winters Filtering (HTF), Exponential 
  smoothing state space model (ETS), and ARIMA.
* Added `xts`, `tscount`, and `forecast` to Imports

# covid23icuve 0.109.1

* Restore home page after login
* data update
* Added login module.
* Added modules `mod_icuve_sitrep`, `mod_icuve_ts` and `mod_icuve_static` to showing ICU's
  situation report, indicators on time series and static data, and predictions for ICUs in Veneto.

# covid19ita 0.109.0

* data update

# covid19ita 0.108.0

* data update

# covid19ita 0.107.1

* Update default visualization for regions: total current cases, and
  total current patients in ICUs.
* reformat x scales for ts-plots using 2 weeks instead of 1 day.
* update `data-UPDATE.R` script to automate committing of updates
* data update

# covid19ita 0.95.0 -- 0.106.0

* data update

# covid19ita 0.94.0

* required dev version of tibble due to an error
* update data
* package `{growthcurve}` is nomore into CRAN, switched to the
  development version from GitHub (briandconnelly/growthcurve)

# covid19ita 0.93.0

* fix update function
* update data

# covid19ita 0.92.0

* update updating functions
* adjust naming convention to the new dpc ones
  (https://github.com/pcm-dpc/COVID-19/blob/master/CHANGELOG_EN.md)
* data update

# covid19ita 0.90.0 -- 0.91.0

* data update

# covid19ita 0.89.0

* data update
* reformat default set o measure displayed

# covid19ita 0.62.0 -- 0.88.0

* data update

# covid19ita 0.61.0

* update gh-action with updated dependencies
* data update

# covid19ita 0.60.0

* data update

# covid19ita 0.59.0

* update remote gh-action (`{lintr}` was out-dated)
* data update

# covid19ita 0.56.0 -- 0.58.0

* data update

# covid19ita 0.55.0

* changed `readr::write_rds()` with `saveRDS()` due to the error
  "simpleWarning: cannot initialize lzma encoder, error 5"
* data update

# covid19ita 0.53.0 -- 0.54.0

* data update

# covid19ita 0.52.0

* data update
* slider 7 day window incomplete tails' data fixed for maps module

# covid19ita 0.47.0 -- 0.51.0

* data update

# covid19ita 0.46.0

* data update (including case tested)

# covid19ita 0.45.0

* improve maps section
* data update

# covid19ita 0.44.0

* data update
* Add name (and email) for GitHub Action User, to be used in its
  git commits (#64).
* separate CI checks into separate workflow for master and develop
  branches. Push/pull-request to master branch will perform full checks
  including oldrel and dev R version. Push/pull-request on develop
  branch will check only against release R version (#65)

# covid19ita 0.43.0


* Update data
* Included covr into gh-action to avoid badges conflicts
* Activate gh-action for (R3.5, R3.6)*(Linux, OSX, Windows) + Rdevel on
  OSX (the only one available at the moment for gh-actions) (#59)
* Remove Travis and Appveyor CI (#59)

# covid19ita 0.42.0

* added cii best practice badge
* update data
* activate lintr on Travis-CI and GitHub Actions
* fix lintr check (all but `extraction_operator_linter` (1408 problems),
  `line_length_linter` (913 problems), and `cyclocomp_linter`
  (2 problems in *_mod_maps.R)).
* activate GitHub Actions for CI.
* activate pkgdown, including a GitHub action to automatically deploy
  the documentation's WEB site.
* update README including information on how to contribute
  (resolve #56).
* translated 20200415_tamponi (close #54).
* refactored 20200415_tamponi (#54).

# covid19ita 0.41.0

* included focus on "Tests and hospitalization"
* data update
* fix text in 20200404_magnani and 20200406_mort_veneto for ita and eng

# covid19ita 0.40.0

* data update

# covid19ita 0.39.0

* data update
* fix map section in eng (#49)

# covid19ita 0.38.0

* update data dpc
* `README` update (#4)

# covid19ita 0.37.0

* all script creating data extracted by topic, recreate data
  independently link `.rds` in `data-raw`. Defined `data-TEMPLATE.R`
  as a template to create script for internal and exported data,
  and `data-INTERNAL.R`, and `data-EXPORTED.R` to collect the proper
  ones and save them properly. Maximum (ie xz lev 9) compression
  is set for internal data (fix #36)
* update spellcheck
* substitute all UTF8 chars with ASCII encodings, this way CMD check
  throw no errors, nor wornings, nor notes (fix #2)
* remove doubles from app_ui (fix #46)
* update affiliations eng
* extracted home from `eng-app_ui.R` to `eng_dashboard_home.R`
* fix translation of 20200406 mort_veneto and 20200404 Magnani

# covid19ita 0.36.0

* update data
* added animation to maps
* fix text in 20200404_Magnani and 20200406_mort_veneto (#44)

# covid19ita 0.35.1

* fix internal data

# covid19ita 0.35.0

* improve internal (fix #38)
* update map section
* data update
* extract home page from `app_ui.R` to `dashboard_home.R`
* added CM and DF to home

# covid19ita 0.34.0

* maps update
* data update

# covid19ita 0.33.1

* fix translation to eng 

# covid19ita 0.33.0

* added geo-spatial module
* translate 20200406 mortality veneto

# covid19ita 0.32.3

* translate 20200404 Magnani

# covid19ita 0.32.2

* data update
* minor fix to module mort veneto

# covid19ita 0.32.1

* fix plot veneto by age
* removed plot veneto by sex

# covid19ita 0.32.0

* data update
* added module focus 20200406 mortality veneto

# covid19ita 0.31.0

* added helper `ggmort()` as helper for standardd mortality plot
* added `clean_ggplotly()` to render plotly with reduced number of
  buttons
* added internal weekly mortality 2015-2020 data from 
  https://www.istat.it/it/files/2020/03/dati-comunali-settimanali-ANPR-1.zip
* added internal version of mortality 2019-2020 data from
  https://www.istat.it/it/files/2020/03/Tavola-sintetica-decessi.xlsx
* added internal data `residenti_anpr_1084`
* added module focus 20200404 Magnani

# covid19ita 0.30.0

* data update

# covid19ita 0.29.0

* data update

# covid19ita 0.28.0

* data update

# covid19ita 0.27.0

* data update

# covid19ita 0.26.2

* Fix covid 29 -> 19 on focus title

# covid19ita 0.26.1

* translation comparative piemonte veneto (#35)
* consistent colour and names for levels eng (#33)
* death and tests labels switched (#34)
* fix English modules changes for data naming
* fix today focus date

# covid19ita 0.26.0

* data update (including renaming)
* added focus module 20200331 epidemiologia comparativa Veneto-Piemonte
* removed useless button from Plotly
* added instructions to manage plots
* colour and label name for levels (#33)

# covid19ita 0.25.2

* death and tests labels switched (#34)

# covid19ita 0.25.1

* Update dictionary
* fix namespace
* Dependencies to {scales}

# covid19ita 0.25.0

* update data
* Logarithmic scale (resolve #5)
* fix date in dropdown menu eng
* logarithmic scale - eng (#5)
* Solve bug (#31) 
* Added Venezia to default provinces (fix #30)

# covid19ita 0.24.0

* data update

# covid19ita 0.23.1

* update plot tamponi/intensive (eng) (#29)

# covid19ita 0.23.0

* data update
* translation focus Veneto hosp 20200328
*update plot tamponi/intensive eng (#29)
* focus Veneto total hosp 20200327
* update plot tamponi/intensive (#29)

# covid19ita 0.22.0

* data update

# covid19ita 0.21.4

* update data

# covid19ita 0.21.3

# covid19ita 0.21.2

* update selection procedure for national plots (fix #28)

# covid19ita 0.21.1

* fix for changing names in national dataset

# covid19ita 0.21.0

* update data
* included FG in contributors
* included CC in contributors
* change in contributors eng (#15)
* translated 20200325 hosp and created the relative eng-test (#27)

# covid19ita 0.20.0

* update data
* fixed plot 2020 03 18 intensive Veneto
* 20200325 module Veneto hosp
* fix colour focus_picco (#24)
* eng-test for veneto-intensive, Novara and picco
* translated 20200323 picco (#23)

# covid19ita 0.19.0

* spellcheck
* `cumulate_for_days()` -> `accumulate_for_days()`
* parametric national and regional logistic estimation
* fix namespace for CMD check's notes
* fix attributions

# covid19ita 0.18.0

* data update

# covid19ita 0.17.0

* update data
* Update team group
* fix link to LinkedIn account

# covid19ita 0.16.0

* Update data
* Added focus section for Vercelli and Alessandria
* Update istruzioni (ita)
* update team group eng
* added Alessandria and Vercelli
* translated x-y labels of the graphs in the epidemic section (#4)
* translated Novara 2020-03-20 (#20)

# covid19ita 0.15.2

* fix date axes in Novara

# covid19ita 0.15.1

# covid19ita 0.15.0

* data update
* fix date position (sidebar) (#18)
* translated 2020-03-19 Veneto and added tab in eng-app_ui and image in eng-app_server (#12)
* index t.intensiva/tamponi non sintomatici dinamico
* Added insight for Novara (ita, #20)
* Changed plot intensive/tamponi y in tamponi non sintomatici (#17)
* Fix legend tamponi (#10)

# covid19ita 0.14.1

* in evidenza: cumulo terapie intensive Veneto

# covid19ita 0.14.0

* small change to eng_mod_0318 (#7)
* rectified a mistake
* translated eng_mod_0318_friuli and updated text in eng_app_ui(#6)
* added message header
* added button to switch language
* data update
* fix label etichette/tamponi (#9)

# covid19ita 0.13.2

* index intensive su tamponi
* fix title highlight Piemonte

# covid19ita 0.13.1

* fix data province e nazionale from dpc

# covid19ita 0.13.0

* data update
* small change to eng_mod_0318_piemonte (#7)
* small modifications to the translation (#4)
* translated eng_mod_0318_piemonte and updated text in eng_app_ui(#7)
* small modifications to the translation (#4)
* added (ita) module for focus on Piemonte (20200318)

# covid19ita 0.12.1

* added (ita) module for focus on friuli (20200318)
* data update (after dpc fix)

# covid19ita 0.12.0

* data update
* small modification to file eng-mod_0314 (#4)
* completed translation of eng-mod_0114 and eng-mode_helped. Added eng_plottly_help_txt to covid_ita     (#4)
* completed translation of eng-mod_info_sidebar (#4)
* completed translation of eng-app_ui and eng-run_dev (#4)

# covid19ita 0.11.0

* data update

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
