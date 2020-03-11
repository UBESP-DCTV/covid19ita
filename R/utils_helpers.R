regions <- function() {
  unique(dpc_covid19_ita_regioni$denominazione_regione)
}

measures <- function(level = c("national", "regional", "provincial")) {
  level <- match.arg(level)

  switch(level,
    national = ,
    regional = c(
      "ricoverati_con_sintomi", "terapia_intensiva",
      "totale_ospedalizzati", "isolamento_domiciliare",
      "totale_attualmente_positivi", "dimessi_guariti",
      "deceduti", "totale_casi", "tamponi"),
    provincial = "totale_casi"
  )
}
