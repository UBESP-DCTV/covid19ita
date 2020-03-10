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
    file_name = paste0(level, ".csv")
) {
  level <- match.arg(level)
  force(file_name)

  if (level == "italia") {
    level <- "andamento-nazionale"
  }

  data_url <- glue::glue(
    "https://raw.githubusercontent.com/",
    "pcm-dpc/COVID-19/master/",
    "dati-{level}/dpc-covid19-ita-{level}.csv"
  )

  code <- download.file(data_url, file.path(dir, file_name))
  invisible(code == 0)
}
