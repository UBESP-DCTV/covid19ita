#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_resource(),

    dashboardPage(
      title = " COVID-19 - Italia ",
      # Helpers for dashboard's header and sidebar
      dashboard_header(),
      dashboardSidebar(uiOutput("sidebarpanel")),
      dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(includeScript(app_sys("app/www/google-analytics.js"))),
        uiOutput("body")
      ),
      # Main dashboard skin colour
      skin = "red"
    )
  )
}




#' Header's dashboard
#'
#' Helper function defining the header of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardHeader
#' @noRd
dashboard_header <- function() {
  dashboardHeader(
    title = "COVID-19 - Italia",

    dropdownMenu(
      # type = "messages",
      # messageItem(
      #   from = "In evidenza",
      #   message = "Impatto tamponi su ospedalizzazioni",
      #   icon = icon("search")
      # ),
      messageItem(
        from = "Dati",
        message = glue::glue("Dati aggiornati ({last_data_update})"),
        icon = icon("database")
      ),
      messageItem(
        from = "Language",
        message = "Click here for the English website",
        icon = icon("flag"),
        href = "https://r-ubesp.dctv.unipd.it/shiny/covid19italy/"
      )
    ),
    uiOutput("logoutbtn")
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_resource <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = " {covid19ita} ",
      meta = list(
        title = "{covid19ita} App",
        description = "Piattaforma di monitoraggio e analisi dell'infezione da COVID-19 in Italia.",
        url = "https://r-ubesp.dctv.unipd.it/shiny/covid19ita/",
        image = app_sys("app/www/ubep_covid.png"),
        image_alt = "Emergenza COVID-19 ITA",
        twitter_creator = "@CorradoLanera",
        twitter_card_type = "summary",
        twitter_site = "@ubesppadova",
        og_locale = "it_IT",
        og_author = c(
          "Corrado L.", "Dario G.", "Paola B.", "Danila A.", "Giulia L.",
          "Ilaria P.", "Nicolas D."
        ),
        og_site_name = "covid19ita"
      )
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
