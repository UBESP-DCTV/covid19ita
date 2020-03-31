#' Italian COVID-19 data
#'
#' Datasets containing the italian COVID-19 data.
#'
#' @format \code{\link[tibble]{tibble}}s with one row per day each
#'   levels of detail (regional adn provincial) and as many variables
#'   as provided by the original data (see the corresponding sections).
#'
#' @source \url{https://github.com/pcm-dpc/COVID-19}
#'
#' @details
#' Licenza: [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/deed.en) - [Visualizza licenza](https://github.com/pcm-dpc/COVID-19/blob/master/LICENSE)
#'
#' Editore/Autore del dataset: [Dipartimento della Protezione Civile](http://www.protezionecivile.it/)
#'
#' Dati forniti dal [Ministero della Salute](http://www.salute.gov.it/)
#'
#' Elaborazione e gestione dati a cura del [Dipartimento della Protezione Civile](http://www.protezionecivile.it/)
#'
#' @note Le Province autonome di Trento e Bolzano sono indicate in
#'   "denominazione regione" e con il codice 04 del Trentino Alto Adige.
#'
#'   Ogni Regione ha una Provincia denominata "In fase di
#'   definizione/aggiornamento" con il codice provincia da 979 a 999,
#'   utile ad indicare i dati ancora non assegnati alle Province.
#'
#' @name dpc_covid19_ita
#' @keywords datasets
NULL


#' @rdname dpc_covid19_ita
#'
#' @section Data at national level:
#' A \code{\link[tibble]{tibble}} with one row per day and
#'   15 variables:
#' \describe{
#'   \item{data}{Date of notification - Italian time zone)}
#'   \item{stato}{Country of reference - XYZ (ISO 3166-1 alpha-3)}
#'   \item{ricoverati_con_sintomi}{Hospitalised patients with symptoms}
#'   \item{terapia_intensiva}{Intensive Care}
#'   \item{totale_ospedalizzati}{Total hospitalised patients}
#'   \item{isolamento_domiciliare}{Home confinement}
#'   \item{totale_positivi}{Total amount of current positive cases (Hospitalised patients + Home confinement)}
#'   \item{variazione_totale_positivi}{News amount of current positive cases (Hospitalised patients + Home confinement)}
#'   \item{nuovi_positivi}{today active cases - yesterday active cases}
#'   \item{dimessi_guariti}{Recovered}
#'   \item{deceduti}{Death}
#'   \item{totale_casi}{Total amount of positive cases}
#'   \item{tamponi}{Tests performed}
#'   \item{note_it}{Notes in Italian language}
#'   \item{note_eng}{Notes in English Language}
#' }
#'
"dpc_covid19_ita_andamento_nazionale"



#' @rdname dpc_covid19_ita
#'
#' @section Data at regional level:
#' A \code{\link[tibble]{tibble}} with one row per day/region
#'  and 19 variables:
#' \describe{
#'   \item{data}{Date of notification - Italian time zone)}
#'   \item{stato}{Country of reference - XYZ (ISO 3166-1 alpha-3)}
#'   \item{codice_regione}{Code of the Region (ISTAT 2019)}
#'   \item{denominazione_regione}{Name of the Region}
#'   \item{lat}{Latitude - WGS84}
#'   \item{long}{Longitude - WGS84}
#'   \item{ricoverati_con_sintomi}{Hospitalised patients with symptoms}
#'   \item{terapia_intensiva}{Intensive Care}
#'   \item{totale_ospedalizzati}{Total hospitalised patients}
#'   \item{isolamento_domiciliare}{Home confinement}
#'   \item{totale_positivi}{Total amount of current positive cases (Hospitalised patients + Home confinement)}
#'   \item{variazione_totale_positivi}{News amount of current positive cases (Hospitalised patients + Home confinement)}
#'   \item{nuovi_positivi}{today active cases - yesterday active cases}
#'   \item{dimessi_guariti}{Recovered}
#'   \item{deceduti}{Death}
#'   \item{totale_casi}{Total amount of positive cases}
#'   \item{tamponi}{Tests performed}
#'   \item{note_it}{Notes in Italian language}
#'   \item{note_eng}{Notes in English Language}
#' }
#'
"dpc_covid19_ita_regioni"


#' @rdname dpc_covid19_ita
#'
#' @section Data at provincial level:
#' A \code{\link[tibble]{tibble}} with one row per day/province
#'  and 12 variables:
#' \describe{
#'   \item{data}{Date of notification - Italian time zone)}
#'   \item{stato}{Country of reference - XYZ (ISO 3166-1 alpha-3)}
#'   \item{codice_regione}{Code of the Region (ISTAT 2019)}
#'   \item{denominazione_regione}{Name of the Region}
#'   \item{codice_provincia}{Code of the Province}
#'   \item{denominazione_provincia}{Name of the Province}
#'   \item{sigla_provincia}{Province abbreviation}
#'   \item{lat}{Latitude - WGS84}
#'   \item{long}{Longitude - WGS84}
#'   \item{totale_casi}{Total amount of positive cases}
#'   \item{tamponi}{Tests performed}
#'   \item{note_it}{Notes in Italian language}
#'   \item{note_eng}{Notes in English Language}
#' }
#'
"dpc_covid19_ita_province"
