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

  mod_help_plot_ui("help"),

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

    h1("Impatto delle politiche di contenimento in Veneto"),
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
    h2("Informazioni sui dati"),

    p("L'applicazione utilizza i principali dati ufficiali riguardanti la situazione COVID-19 italiana, a livello nazionale, regionale e provinciale."),

    p(
      'Tali dati sono inizialmente gestiti, processati e messi a disposizione dalla ',
      a(href = "http://www.protezionecivile.it/web/guest/home", target = "_blank", "Presidenza del Consiglio dei Ministri - Dipartimento di Protezione Civile"),
      ' con licenza ', a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
      'così come forniti dal ', a(href = "", target = "_blank", "Ministero della Salute,")
    ),

    p('Di norma, i dati sono aggiornati alle ore 18:00 di ogni giorno.'),

    p(
      "Per ulteriori informazioni sulla loro attribuzione, disponibilita, e uso, si consiglia di visitare la",
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = "_blank", "pagina del progetto covid19ita"),
      "."
    ),

    h3('Nota per gli utilizzatori di R'),
    p(
      'Oltre a questa stessa applicazione in sé (che può essere eseguita localmente installando il pacchetto {covid19ita} ed eseguendo l\'istruzione `run_app()`), ll pacchetto R {covid19ita}, disponibile su ',
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = '_blank', 'GitHub'),
      ' e sviluppato anch\'esso sotto licenza ',
      a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
      ', mette a disposizione tali dati, senza alcun processamento ulteriore rispetto a quelli originali, per un loro diretto utilizzo in R.'
    )
  ),

  tabItem(tabName = "credits",
    h2("Persone"),
    p("Dario Gregori"),
    p("Paola Berchialla"),
    p("Corrado Lanera"),
    p("Giulia Lorenzoni"),
    p("Nicolas Destro"),

    h2("Istituzioni e Gruppo di lavoro"),
    p(
      "Il progetto covid19ita è stato sviluppato dall'",
      a(href = 'https://ubesp.jimdofree.com/', target = '_blank', 'Unità di Biostatistica, Epidemiologia, e Salute Pubblica'),
      " del ", a(href = 'https://www.dctv.unipd.it/', target = '_blank', 'Dipartimento di Scienze Cardio- Toraco- Vascolari e Salute Pubblica'),
      " dell'", a(href = 'https://www.unipd.it/', target = '_blank', 'Università degli Studi di Padova'), ",",
      " in collaborazione con il ", a(href = 'https://www.dscb.unito.it/do/gruppi.pl/Tree', target = '_blank', 'Dipartimento di Scienze Cliniche e Biologiche'),
      " dell'", a(href = 'https://www.unito.it/', target = '_blank', 'Università degli Studi di Torino'), "."
    ),

    h2("Software"),
    p(
      "L'applicazione covid19ita è stata sviluppata in R ver. 3.6.3 come un suo pacchetto di espansione, disponibile su GitHub all'indirizzo ",
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = '_blank', 'https://github.com/UBESP-DCTV/covid19ita/'), "."),

    p("Principalmente, per il suo sviluppo, sono stati utilizzati i pacchetti di espansione {shiny} ver. 1.4.0, {shinydashboard} v.0.7.1 e {golem} ver. 0.2.1."),

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

