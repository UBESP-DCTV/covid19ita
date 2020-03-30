nations <- function() {
  sort(unique(dpc_covid19_ita_andamento_nazionale$stato))
}

regions <- function() {
  sort(unique(dpc_covid19_ita_regioni$denominazione_regione))
}

provinces <- function() {
  sort(unique(dpc_covid19_ita_province$denominazione_provincia))
}

measures <- function(
  level = c("national", "regional", "provincial"),
  lang = c("ita", "eng")
) {
  level <- match.arg(level)
  lang <- match.arg(lang)

  dictionary <- c(
    "hospitalized_with_symptoms", "intensive_care", "total_hospitalized",
    "isolation", "active_cases", "recovered", "total_cases", "deaths",
    "tests_performed"
  ) %>%
    purrr::set_names(c(
      "ricoverati_con_sintomi", "terapia_intensiva",
      "totale_ospedalizzati", "isolamento_domiciliare",
      "totale_attualmente_positivi", "dimessi_guariti",
      "deceduti", "totale_casi", "tamponi"
    ))

  res <- sort(switch(level,
    national = ,
    regional = c(
      "ricoverati_con_sintomi", "terapia_intensiva",
      "totale_ospedalizzati", "isolamento_domiciliare",
      "totale_attualmente_positivi", "dimessi_guariti",
      "deceduti", "totale_casi", "tamponi"),
    provincial = "totale_casi"
  ))

  res <- if (lang == "eng") {
    res  %>%
      purrr::set_names(dictionary[res])
  } else {
    res %>% purrr::set_names()
  }

  names(res) <- names(res) %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_title()

  res
}

measure_to_labels <- function(x, lang = c("ita", "eng")) {
 lang <- match.arg(lang)
 stopifnot(is.character(x))

 dictionary <- c(
   "hospitalized_with_symptoms", "intensive_care", "total_hospitalized",
   "isolation", "active_cases", "recovered", "total_cases", "deaths",
   "tests_performed"
   ) %>%
   purrr::set_names(c(
   "ricoverati_con_sintomi", "terapia_intensiva",
   "totale_ospedalizzati", "isolamento_domiciliare",
   "totale_attualmente_positivi", "dimessi_guariti",
   "deceduti", "totale_casi", "tamponi"
   ))

 stopifnot(all(x %in% names(dictionary)))


 if (lang == "eng") {
   x <- dictionary[x]
 }

 stringr::str_replace_all(x, "_", " ") %>%
   stringr::str_to_title()
}
