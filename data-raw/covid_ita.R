## code to prepare `covid_ita` dataset goes here
requireNamespace("purrr", quietly = TRUE)
requireNamespace("readr", quietly = TRUE)
library(covid19ita)

data_levels <- list("italia", "regioni", "province")
dest_dir <- tempdir()

are_ok <- purrr::map2_lgl(data_levels, data_levels,
    ~ download_dpc(.x,  dir = dest_dir, file_name = paste0(.y, ".csv"))
)

if (!all(are_ok)) {
  usethis::ui_oops("Download error. No data are imported in the package.")
} else {

  eng_plottly_help_txt <- shiny::HTML(
    "Click on the semi-trasparent icons to:</br>
      - zoom </br>
      - remove levels (one click)</br>
      - keep only one level (double click)</br>
      - multiple details (tab)</br>
      - export images (camera)</br>
  ")

  plottly_help_txt <- shiny::HTML(
    "Tramite i pulsanti in semi-trasparenza è possibile:</br>
      - zoommare (anche a selezione)</br>
      - deselezionare livelli (singolo click)</br>
      - isolare un livello (doppio click)</br>
      - dettagli multipli (doppia linguetta)</br>
      - esportazione (macchina foto)</br>
  ")


  region_population <- tibble::tribble(
    ~ denominazione_regione, 	         ~residenti,
    "Lombardia"	           ,             10018806,
    "Lazio"   	           ,              5898124,
    "Campania"	           ,              5839084,
    "Sicilia"              ,              5056641,
    "Veneto"               ,              4907529,
    "Emilia Romagna"       ,              4448841,
    "Piemonte"	           ,              4392526,
    "Puglia"	             ,              4063888,
    "Toscana"	             ,              3742437,
    "Calabria"	           ,              1965128,
    "Sardegna"	           ,              1653135,
    "Liguria"	             ,              1565307,
    "Marche"	             ,              1538055,
    "Abruzzo"	             ,              1322247,
    "Friuli Venezia Giulia",              1217872,
    "P.A. Trento"          ,               541098,
    "P.A. Bolzano"         ,               531178,
    "Umbria"	             ,               888908,
    "Basilicata"	         ,               570365,
    "Molise"	             ,               310449,
    "Valle d'Aosta"     	 ,               126883
  )

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
    tamponi = "tests_performed"
  )


  usethis::ui_done("All data correctly downloaded")

  covid_ita <- file.path(dest_dir, paste0(data_levels, ".csv")) %>%
      purrr::set_names(data_levels) %>%
      purrr::map(readr::read_csv)

  dpc_covid19_ita_andamento_nazionale <- covid_ita[["italia"]]
  dpc_covid19_ita_regioni             <- covid_ita[["regioni"]]
  dpc_covid19_ita_province            <- covid_ita[["province"]]

  last_data_update <- lubridate::now()

  usethis::use_data(
    dpc_covid19_ita_andamento_nazionale,
    dpc_covid19_ita_regioni,
    dpc_covid19_ita_province,
    overwrite = TRUE
  )
  usethis::use_data(
    plottly_help_txt,
    eng_plottly_help_txt,
    dpc_covid19_ita_andamento_nazionale,
    dpc_covid19_ita_regioni,
    dpc_covid19_ita_province,
    last_data_update,
    region_population,
    dictionary,
    internal = TRUE,
    overwrite = TRUE
  )

}


