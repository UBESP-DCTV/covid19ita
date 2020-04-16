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

  decessi_url <- paste0(
    "https://www.istat.it/",
    "it/files/2020/03/",
    "Tavola-sintetica-decessi.xlsx"
  )

  tmp <- tempfile(fileext = ".xlsx")

  download.file(decessi_url, tmp, mode = "wb")


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
  ui_done("decessi_genere ready")





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

  settimanale_url <- paste0(
    "https://www.istat.it/",
    "it/files//2020/03/",
    "dati-comunali-settimanali-ANPR-1.zip"
  )

  tmp_sett <- tempfile(fileext = ".zip")

  download.file(settimanale_url, tmp_sett, mode = "wb")

  comuni_settimana <- unzip(
    tmp_sett, "comuni_settimana.xlsx",
    exdir = tempdir()
  ) %>%
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
    dplyr::select(-.data$reg, -.data$prov, -.data$cod_provcom) %>%
    dplyr::mutate(
      area = dplyr::if_else(.data$regione %in% nord,
        true = "nord",
        false = "sud, centro, isole"
      )
    ) %>%
    tidyr::pivot_longer(.data$maschi_2015:.data$totale_2020,
      names_to = c("sex", "year"),
      names_ptypes = list(sex = character(), year = integer()),
      names_sep = "_",
      values_to = "n_death",
      values_ptypes = list(n_death = integer())
    ) %>%
    dplyr::mutate(
      classe_di_eta = .data$classe_di_eta %>%
        forcats::fct_collapse(
          "0-64 anni" = c("0-14 anni", "15-64 anni")
        )
    )
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
