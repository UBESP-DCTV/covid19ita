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
                     lang = c("ita", "eng")) {
  level <- match.arg(level)
  lang <- match.arg(lang)

  res <- sort(switch(level,
    national = c(
      "ricoverati_con_sintomi", "terapia_intensiva",
      "totale_ospedalizzati", "isolamento_domiciliare",
      "totale_positivi", "variazione_totale_positivi", "nuovi_positivi",
      "dimessi_guariti", "deceduti", "casi_da_sospetto_diagnostico",
      "casi_da_screening", "totale_casi", "tamponi", "casi_testati"
    ),
    regional = c(
      "ricoverati_con_sintomi", "terapia_intensiva",
      "totale_ospedalizzati", "isolamento_domiciliare",
      "totale_positivi", "variazione_totale_positivi", "nuovi_positivi",
      "dimessi_guariti", "deceduti", "casi_da_sospetto_diagnostico",
      "casi_da_screening", "totale_casi", "tamponi", "casi_testati"
    ),
    provincial = "totale_casi"
  ))

  res <- if (lang == "eng") {
    res %>%
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

  stopifnot(all(x %in% names(dictionary)))


  if (lang == "eng") {
    x <- dictionary[x]
  }

  stringr::str_replace_all(x, "_", " ") %>%
    stringr::str_to_title()
}



super_secret <- function() {
  path <- here::here("supersecret")
  if (!file.exists(path)) {
    stop("Can't find secret file at: ", path)
  }

  readr::read_csv(path, skip = 1L)
}

nomatch <- function() {
  shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
  shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
}
