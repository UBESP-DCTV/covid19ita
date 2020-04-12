data_population <- function() {

  # initial clean- and set-up ---------------------------------------
  cli::cat_rule("Dataset: Population")

  # Detach all loaded packages and clean your environment
  invisible(golem::detach_all_attached())

  # Document and reload your package
  golem::document_and_reload()

  ## NOTE: all the needed packages must be included in the Imports (
  ##       if used actively in teh package) or in the Suggests list of
  ##       packages into the `DESCRIPTION` file. Here, call them using
  ##       the explicit notation `package::function()` without
  ##       `library()` any of them!




  # Code to prepare the data ----------------------------------------

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
  ui_done("region_population ready")




  # pop_regioni <- "http://demo.istat.it/pop2019/dati/regioni.zip"
  #
  # tmp_regioni <- tempfile(fileext = ".zip")
  #
  # download.file(pop_regioni, tmp_regioni, mode = 'wb')
  #
  #
  # decessi_genere <- unzip(tmp_regioni, "regioni.csv") %>%
  #   readr::read_csv(skip = 1) %>%
  #   dplyr::select(.data$Regione, dplyr::starts_with("Totale")) %>%
  #   dplyr::group_by(Regione) %>%
  #   dplyr::summarise_all(sum, na.rm = TRUE)
  residenti_anpr_1084 <- tibble::tribble(
    ~NOME_REGIONE,      ~Totale_Maschi, ~Totale_Femmine,
    "Totale",	               6177016,         6496805,
    "Piemonte",	              391799,          417469,
    "Valle d'Aosta",            18268,           20588,
    "Lombardia",              2769384,         2912559,
    "Trentino A.A.",            50868,           52953,
    "Veneto",                  527531,          542071,
    "Friuli Venezia Giulia",    42849,           44577,
    "Liguria",                 147120,          161849,
    "Emilia-Romagna",	        899173,          946560,
    "Toscana",	                404649,          431081,
    "Umbria",                   93868,          100686,
    "Marche",                  175079,          186308,
    "Lazio",                    51261,           54331,
    "Abruzzo",                  57323,           59194,
    "Molise",                   12326,           12420,
    "Campania",                 79492,           82714,
    "Puglia",                  202119,          213439,
    "Basilicata",               16411,           16778,
    "Calabria",                 17885,           18251,
    "Sicilia",                 132204,          135107,
    "Sardegna",                 87407,           87870
  )
  ui_done("residenti_anpr_1084 ready")





  # Code to save the data -------------------------------------------

  ## Must be the names of the datasets!
  write_raw_rds(c(
    "region_population",
    "residenti_anpr_1084"
  ))

}


data_population()


## After executed the function (ie creating/updating the data) remember
## to update `data-INTERNAL.R`, and `data-EXPORTED.R` accordingly to
## store them in the correct place into the package's data.




