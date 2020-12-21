data_mortalita <- function() {

  # initial clean- and set-up ---------------------------------------
  cli::cat_rule("Dataset: TEMPLATE")

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

  # decessi (Magnani focus 20200404) --------------------------------



  #NOME DELLA NUOVA CATELLA:
  #Tavola-riepilogativa-e-tracciato-record-dati-al-30-settembre
  #NOME DEL FILE:
  # Tavola riepilogativa_30SETTEMBRE_7.903COMUNI
  decessi_url <- paste0(
    "https://www.istat.it/",
    "it/files//2020/03/",
    "Tavola-riepilogativa-e-tracciato-record-dati-al-30-settembre.zip"
  )

  tmp <- tempfile(fileext = ".zip")

  download.file(decessi_url, tmp, mode = "wb")


  comuni_aggregato <- unzip(
    tmp, "Tavola-riepilogativa-e-tracciato-record-dati-al-30-settembre/Tavola riepilogativa_30SETTEMBRE_7.903COMUNI.xlsx",
    exdir = tempdir()
  )


  decessi_genere <- comuni_aggregato %>%
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
  ui_done("decessi_genere ready")



#il foglio si chiama età 65+ totale

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
  ui_done("decessi_eta ready")



  #età 65+ maschi


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
  ui_done("decessi_eta_maschi ready")


 #età 65+ femmine

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
  ui_done("decessi_eta_femmine ready")




  mort_data_reg_age <- mort_data_reg("age")
  ui_done("mort_data_reg_age ready")

  mort_data_reg_sex <- mort_data_reg("sex")
  ui_done("mort_data_reg_sex ready")




  mort_data_veneto_age <- mort_data_veneto("age")
  ui_done("mort_data_veneto_age ready")

  mort_data_veneto_sex <- mort_data_veneto("sex")
  ui_done("mort_data_veneto_sex ready")



  # Mortalit`a settimanale ------------------------------------------
  #


  settimanale_url <- paste0(
    "https://www.istat.it/",
    "it/files//2020/03/",
    "Dataset-decessi-comunali-giornalieri-e-tracciato-record_dati-al-30-settembre.zip"
  )

  tmp_sett <- tempfile(fileext = ".zip")

  download.file(settimanale_url, tmp_sett, mode = "wb")

  comuni_settimana <- unzip(
    tmp_sett, "comuni_giornaliero_30settembre.csv",
    exdir = tempdir()
  ) %>%
    read.csv(encoding = "Latin-1") %>%
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
  ui_done("comuni_settimana ready")




  # Mortalità comuni ------------------------------------------------

  nord <- c(
    "Friuli Venezia Giulia", "Emilia-Romagna", "Liguria", "Veneto",
    "Lombardia", "Piemonte", "Trentino A.A.", "Valle d'Aosta"
  )
  sud_centro_isole <- setdiff(
    unique(comuni_settimana[["regione"]]),
    nord
  )

  mort_data_comuni <- comuni_settimana %>%
    mutate(m_15 = ifelse(m_15 == "n.d.", NA, m_15),
           m_16 = ifelse(m_16 == "n.d.", NA, m_16),
           m_17 = ifelse(m_17 == "n.d.", NA, m_17),
           m_18 = ifelse(m_18 == "n.d.", NA, m_18),
           m_19 = ifelse(m_19 == "n.d.", NA, m_19),
           m_20 = ifelse(m_20 == "n.d.", NA, m_20),
           f_15 = ifelse(f_15 == "n.d.", NA, f_15),
           f_16 = ifelse(f_16 == "n.d.", NA, f_16),
           f_17 = ifelse(f_17 == "n.d.", NA, f_17),
           f_18 = ifelse(f_18 == "n.d.", NA, f_18),
           f_19 = ifelse(f_19 == "n.d.", NA, f_19),
           f_20 = ifelse(f_20 == "n.d.", NA, f_20),
           t_15 = ifelse(t_15 == "n.d.", NA, t_15),
           t_16 = ifelse(t_16 == "n.d.", NA, t_16),
           t_17 = ifelse(t_17 == "n.d.", NA, t_17),
           t_18 = ifelse(t_18 == "n.d.", NA, t_18),
           t_19 = ifelse(t_19 == "n.d.", NA, t_19),
           t_20 = ifelse(t_20 == "n.d.", NA, t_20)) %>%
    mutate_at(vars(m_15:t_20), as.numeric) %>%
    dplyr::select(-.data$reg, -.data$prov, -.data$cod_provcom) %>%
    dplyr::mutate(
      area = dplyr::if_else(.data$regione %in% nord,
                            true = "nord",
                            false = "sud, centro, isole"
      )
    ) %>%
    tidyr::pivot_longer(.data$m_15:.data$t_20,
                        names_to = c("sex", "year"),
                       # names_ptypes = list(sex = character(), year = integer()),
                        names_sep = "_",
                        values_to = "n_death"#,
                       # values_ptypes = list(n_death = integer())
    ) %>%
    mutate(cl_eta = ifelse(cl_eta >))#create age class
    dplyr::group_by(regione, eta) %>%
    dplyr::mutate()
  ui_done("mort_data_comuni ready")




  # Code to save the data -------------------------------------------

  ## Must be the names of the datasets!
  write_raw_rds(c(
    "decessi_genere",
    "decessi_eta",
    "decessi_eta_maschi",
    "decessi_eta_femmine",

    "mort_data_reg_age", "mort_data_reg_sex", "mort_data_veneto_age",
    "mort_data_veneto_sex",
    "comuni_settimana",
    "mort_data_comuni"
  ))
}


data_mortalita()


## After executed the function (ie creating/updating the data) remember
## to update `data-INTERNAL.R`, and `data-EXPORTED.R` accordingly to
## store them in the correct place into the package's data.
##
## decessi_genere <- read_data_raw("decessi_genere")
## decessi_eta <- read_data_raw("decessi_eta")
## decessi_eta_maschi <- read_data_raw("decessi_eta_maschi")
## decessi_eta_femmine <- read_data_raw("decessi_eta_femmine")
##
## mort_data_reg_age <- read_data_raw("mort_data_reg_age")
## mort_data_reg_sex <- read_data_raw("mort_data_reg_sex")
## mort_data_veneto_age <- read_data_raw("mort_data_veneto_age")
## mort_data_veneto_sex <- read_data_raw("mort_data_veneto_sex")
##
## comuni_settimana <- read_data_raw("comuni_settimana")
## mort_data_comuni <- read_data_raw("mort_data_comuni")
