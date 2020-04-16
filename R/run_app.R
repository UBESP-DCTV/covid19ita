#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param language (chr, default = "ita") the language of the application
#'   i.e., "ita" for Italian or "eng" for English)
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
                    ...,
                    language = c("ita", "eng")) {
  language <- match.arg(language)

  if (language == "ita") {
    with_golem_options(
      app = shinyApp(
        ui = app_ui,
        server = app_server
      ),
      golem_opts = list(...)
    )
  } else {
    with_golem_options(
      app = shinyApp(
        ui = eng_app_ui,
        server = eng_app_server
      ),
      golem_opts = list(...)
    )
  }
}
