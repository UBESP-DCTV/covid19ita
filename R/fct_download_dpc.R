#' Download dpc COVID-19 Italian data
#'
#' For each possible level (i.e., national = italia, regional = regioni,
#' provincial = province) download data from the official Italian
#' **Dipartimento della Protezione Civile** GitHub repository.
#'
#' @param level (chr) one of "italia", "regioni", "province"
#' @param dir (chr, default = ".") path of the directory where the downloaded file is
#'   saved
#' @param file_name (chr, default = "<level>.csv") downloaded file filename
#'
#' @return An (invisible) logical, `TRUE` for success and `FALSE` for failure.
#' @export
#'
#' @examples
#' \dontrun{
#'   download_dpc("italia")
#' }
download_dpc <- function(
    level = c("italia", "regioni", "province"),
    dir = ".",
    file_name = NULL
) {
  level <- match.arg(level)

  if (level == "italia") {
    level <- "andamento-nazionale"
  }

  data_url <- glue::glue(
    "https://raw.githubusercontent.com/",
    "pcm-dpc/COVID-19/master/",
    "dati-{level}/dpc-covid19-ita-{level}.csv"
  )

  file_name <- file_name %||% basename(data_url)

  dest_url <- file.path(dir, file_name)
  code <- utils::download.file(data_url, dest_url)

  ok <- (code == 0) &&
    stringr::str_detect(readLines(dest_url, 1L), "^data,stato")

  if (ok) {
    usethis::ui_done("{usethis::ui_value(file_name)} downloaded.")
  } else {
    usethis::ui_oops(
      "{usethis::ui_value(file_name)} not downloaded correctly."
    )
  }

  invisible(ok)
}
