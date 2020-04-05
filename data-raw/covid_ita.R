## code to prepare `covid_ita` dataset goes here
requireNamespace("purrr", quietly = TRUE)
requireNamespace("readr", quietly = TRUE)
requireNamespace("readxl", quietly = TRUE)
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




# decessi (Magnani focus 20200404) --------------------------------


  decessi_url <- paste0(
    "https://www.istat.it/",
    "it/files/2020/03/",
    "Tavola-sintetica-decessi.xlsx"
  )

  tmp <- tempfile(fileext = ".xlsx")

  download.file(decessi_url, tmp, mode = 'wb')


  decessi_genere <- tmp %>%
    readxl::read_excel(
      "Totale per sesso",
      skip = 2,
      col_names = c(
        "id_reg", "id_prov",
        "nome_reg", "nome_prov", "nome_comune",
        "id_codprov",
        "tot_m_2019", "tot_f_2019", "tot_mf_2019",
        "tot_m_2020", "tot_f_2020", "tot_mf_2020",
        "var_m_2020", "var_f_2020", "var_mf_2020"
      ),
      na = "-"
   )

  decessi_eta <- tmp %>%
    readxl::read_excel(
      "Età",
      skip = 2,
      col_names = c(
        "id_reg", "id_prov",
        "nome_reg", "nome_prov", "nome_comune",
        "id_codprov",
        "tot_65-74_2019", "tot_75-84_2019", "tot_85+_2019",
        "tot_65-74_2020", "tot_75-84_2020", "tot_85+_2020",
        "var_65-74_2020", "var_75-84_2020", "var_85+_2020"
      ),
      na = "-"
    )

  decessi_eta_maschi <- tmp %>%
    readxl::read_excel(
      "Età Maschi",
      skip = 2,
      col_names = c(
        "id_reg", "id_prov",
        "nome_reg", "nome_prov", "nome_comune",
        "id_codprov",
        "tot_65-74_2019", "tot_75-84_2019", "tot_85+_2019",
        "tot_65-74_2020", "tot_75-84_2020", "tot_85+_2020",
        "var_65-74_2020", "var_75-84_2020", "var_85+_2020"
      ),
      na = "-"
    )

  decessi_eta_femmine <- tmp %>%
    readxl::read_excel(
      "Età Femmine",
      skip = 2,
      col_names = c(
        "id_reg", "id_prov",
        "nome_reg", "nome_prov", "nome_comune",
        "id_codprov",
        "tot_65-74_2019", "tot_75-84_2019", "tot_85+_2019",
        "tot_65-74_2020", "tot_75-84_2020", "tot_85+_2020",
        "var_65-74_2020", "var_75-84_2020", "var_85+_2020"
      ),
      na = "-"
    )



# Mortalit`a settimanale ------------------------------------------

  settimanale_url <- paste0(
    "https://www.istat.it/",
    "it/files//2020/03/",
    "dati-comunali-settimanali-ANPR-1.zip"
  )

  tmp_sett <- tempfile(fileext = ".zip")

  download.file(settimanale_url, tmp_sett, mode = 'wb')

  comuni_settimana <- unzip(tmp_sett, "comuni_settimana.xlsx") %>%
    readxl::read_xlsx() %>%
    janitor::clean_names() %>%
    dplyr::rename(
      regione = nome_regione,
      provincia = nome_provincia,
      comune = nome_comune
    ) %>%
    dplyr::mutate(
      regione = stringr::str_replace_all(regione, c(
        "Trentino-Alto Adige/Südtirol" = "Trentino A.A.",
        "Valle d'Aosta/Vallée d'Aoste" = "Valle d'Aosta",
        "Friuli-Venezia Giulia" = "Friuli Venezia Giulia"
      ))
    )










# Storing data ----------------------------------------------------

  usethis::use_data(
    dpc_covid19_ita_andamento_nazionale,
    dpc_covid19_ita_regioni,
    dpc_covid19_ita_province,

    comuni_settimana,
    residenti_anpr_1084,
    decessi_genere,
    decessi_eta,
    decessi_eta_maschi,
    decessi_eta_femmine,

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

    comuni_settimana,
    residenti_anpr_1084,
    decessi_genere,
    decessi_eta,
    decessi_eta_maschi,
    decessi_eta_femmine,

    internal = TRUE,
    overwrite = TRUE
  )

}


