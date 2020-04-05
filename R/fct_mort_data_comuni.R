#' Reschape comuni_settimana data tidyly
#'
#'
#' @return a tibble including regione, provincia,
#'   comune, settimana, classe di età, area (nord - or -
#'   centro, sud, isole), strata, year, n_death
#' @export
#'
#' @examples
#' mort_data_comuni()
mort_data_comuni <- function() {

  nord <- c(
    "Friuli Venezia Giulia", "Emilia-Romagna", "Liguria", "Veneto",
    "Lombardia", "Piemonte", "Trentino A.A.", "Valle d'Aosta"
  )
  sud_centro_isole <- setdiff(
    unique(comuni_settimana[["regione"]]),
    nord
  )

  comuni_settimana %>%
    dplyr::select(-.data$reg, -.data$prov, -.data$cod_provcom) %>%
    dplyr::mutate(
      area = dplyr::if_else(.data$regione %in% nord,
        "nord", "sud, centro, isole"
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
      classe_di_eta = classe_di_eta %>%
        stringr::str_replace_all(c(
          "0-14" = "0-64",
          "15-64" = "0-64"
        ))
    )

}
