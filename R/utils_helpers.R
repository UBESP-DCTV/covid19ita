nations <- function() {
  sort(unique(dpc_covid19_ita_andamento_nazionale$stato))
}

regions <- function() {
  sort(unique(dpc_covid19_ita_regioni$denominazione_regione))
}

provinces <- function() {
  sort(unique(dpc_covid19_ita_province$denominazione_provincia))
}

measures <- function(level = c("national", "regional", "provincial")) {
  level <- match.arg(level)

  sort(switch(level,
    national = ,
    regional = c(
      "ricoverati_con_sintomi", "terapia_intensiva",
      "totale_ospedalizzati", "isolamento_domiciliare",
      "totale_attualmente_positivi", "dimessi_guariti",
      "deceduti", "totale_casi", "tamponi"),
    provincial = "totale_casi"
  ))
}
