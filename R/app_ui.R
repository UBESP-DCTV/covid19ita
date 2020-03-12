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

  menuItem(""),

  menuItem("Impatto", tabName = "impact", icon = icon("bullseye")),

  menuItem("Andamenti", icon = icon("chart-line"),
    menuSubItem("Nazionale", tabName = "national", icon = icon("flag")),
    menuSubItem("Regionale", tabName = "regional", icon = icon("map")),
    menuSubItem("Provinciale", tabName = "provincial", icon = icon("location-arrow"))
  ),

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
  ),

  menuItem("Data", tabName = "data_tab", icon = icon("database")),

  menuItem("Crediti", tabName = "credits", icon = icon("user-friends")),

  mod_info_sidebar_ui("summary_today")

))}




#' Body dashboard
#'
#' Helper function defining the main body of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardBody box
#' @noRd
dashboard_body <- function() {dashboardBody(tabItems(

  tabItem(tabName = "impact",

    h2("A"),

    fluidRow(
      mod_impact_veneto_a_ui("smooth_linear",
        title = "Variazione di tendenza"
      )
    ),

    h2("B"),

    fluidRow(
      mod_impact_veneto_b_ui("loess_veneto",
        title = "Loess",
        width = 6
      ),
      mod_impact_veneto_b_ui("gam_veneto",
        title = "GAM",
        6
      )
    ),
    fluidRow(
      box(width = 6,
        p("Comments")
      ),
      box(
        p("Comments"),
      )
    )



  ),



  tabItem(tabName = "national",
    # List the first level UI elements here
    # `box()`es must be included in `fluidRow()`s
    h2("Eventi nazionali"),
    mod_ts_ita_ui("ts_nat_cum", title = "Serie storiche degli eventi cumulati"),
    mod_ts_ita_ui("ts_nat_inc", title = "Serie storiche dei nuovi eventi giornalieri")
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

    h3("Serie storiche"),
    fluidRow(
      box(title = "Eventi cumulati", width = 12,
        mod_ts_prv_ui("ts_prv_cum")
      )
    ),

    fluidRow(
      box(title = "Nuovi eventi", width = 12,
        mod_ts_prv_ui("ts_prv_inc")
      )
    )
  ),

  tabItem(tabName = "data_tab",
    h2("Data information"),
    p("L'applicazione utilizza i principali dati ufficiali riguardanti la situazione COVID-19 italiana, a livello nazionale, regionale e provinciale."),
    div(p(
      "Per informazioni sulla loro attribuzione, disponibilita, e uso, visitare la",
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = "_blank", "pagina del progetto covid19ita.")
    ))
  ),

  tabItem(tabName = "credits",
    h2("Persone"),
    p(""),

    h2("Software"),
    p("L'applicazione covid19ita sono stati sviluppati come un pacchetto di espansione di R in R ver. 3.6.3, principalmente utilizzando i suoi pacchetti di espansione {shiny} ver. 1.4.0, {shinydashboard} v.0.7.1 e {golem} ver. 0.2.1."),
    p("Le analisi sono state eseguite sfruttando principalmente i pacchetti {stats} ver. 3.6.3, e {gam} ver. 1.16.1"),
    p("I grafici sono stati prodotti grazie principalmente ai pacchetti {ggplot2} ver. 3.3.0 e {plotly} ver. 4.9.2.")

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

