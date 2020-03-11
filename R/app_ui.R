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
    golem_add_external_resources(),

    dashboardPage(
      # Helpers for dashboard's header and sidebar
      dashboard_header(), dashboard_sidebar(), dashboard_body(),
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
dashboard_header <- function() {dashboardHeader(

  title = "COVID-19 - Italia"

)}




#' Sidebars dashboard
#'
#' Helper function defining the sidebar of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
dashboard_sidebar <- function() {dashboardSidebar(sidebarMenu(

  menuItem("Nazionale", tabName = "national", icon = icon("flag")),
  menuItem("Regionale", tabName = "regional", icon = icon("map")),
  menuItem("Provinciale", tabName = "provincial", icon = icon("location-arrow")),

  menuItem(""),

  menuItem("Collegamenti esterni", icon = icon("link"),
    menuSubItem("UBEP", icon = icon("signal"),
      href = "https://ubesp.jimdofree.com/"
    ),
    menuSubItem("Codice sorgente", icon = icon("file-code-o"),
      href = "https://github.com/UBESP-DCTV/covid19ita/"
    ),
    menuSubItem("Segnalazioni", icon = icon("exclamation-triangle"),
      href = "https://github.com/UBESP-DCTV/covid19ita/issues/"
    )
  )

))}




#' Body dashboard
#'
#' Helper function defining the main body of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardBody box
#' @noRd
dashboard_body <- function() {dashboardBody(tabItems(

  tabItem(tabName = "national",
    # List the first level UI elements here
    # `box()`es must be included in `fluidRow()`s
    h2("Eventi nazionali"),

    fluidRow(
      box(title = "Serie storiche degli eventi cumulati", width = 12,
        mod_ts_ita_ui("ts_nat_cum")
      )
    ),

    fluidRow(
      box(title = "Serie storiche dei nuovi eventi giornalieri", width = 12,
        mod_ts_ita_ui("ts_nat_inc")
      )
    )
  ),


  tabItem(tabName = "regional",
    h2("Eventi regionali"),

    h3("Serie storiche per regione"),
    fluidRow(
      box(title = "Eventi cumulati", width = 12,
        mod_ts_reg_ui("ts_reg_cum_mes")
      )
    ),

    fluidRow(
      box(title = "Nuovi eventi giornalieri", width = 12,
        mod_ts_reg_ui("ts_reg_inc_mes")
      )
    ),

    h3("Serie storiche regionali per evento"),
    fluidRow(
      box(title = "Eventi cumulati", width = 12,
        mod_ts_reg_ui("ts_reg_cum_reg")
      )
    ),

    fluidRow(
      box(title = "Nuovi eventi giornalieri", width = 12,
        mod_ts_reg_ui("ts_reg_inc_reg")
      )
    )

  ),


  tabItem(tabName = "provincial",
    h2("Eventi provinciali"),
    h3("In fase di sviluppo...")
  )

))}




#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'covid19ita'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

