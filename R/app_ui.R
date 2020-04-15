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

    dashboardPage(title = " COVID-19 - Italia ",
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

  title = "COVID-19 - Italia",

  dropdownMenu(type = "messages",
    messageItem(
      from = "In evidenza",
      message = "Impatto tamponi su ospedalizzazioni",
      icon = icon("search")
    ),
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
  )

)}




#' Sidebars dashboard
#'
#' Helper function defining the sidebar of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
dashboard_sidebar <- function() {dashboardSidebar(sidebarMenu(
  id = "sidebar",

  mod_help_plot_ui("help"),

  dashboard_home_sidebar(),


  menuItem("In evidenza", icon = icon("bullseye"),

    menuSubItem(
      text = "2020-04-15 Impatto tamponi",
      tabName = "20200415TampHosp",
      icon = icon("flag")
    ),

    menuSubItem("2020-04-06 Mortalit\u00E0  Veneto", tabName = "20200406MortVeneto",
                icon = icon("flag")
    ),
    menuSubItem("2020-04-05 Mortalit\u00E0", tabName = "20200405Mort",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-31 Comparativa", tabName = "20200331Comp",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-28 Veneto", tabName = "20200328Tot_hosp",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-25 Veneto", tabName = "20200325Hosp",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-24 Metodologia", tabName = "20200323Picco",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-21 Alessandria", tabName = "20200321Alessandria",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-21 Vercelli", tabName = "20200321Vercelli",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-20 Novara", tabName = "20200320Novara",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-19 Veneto", tabName = "20200319Veneto",
                icon = icon("flag")
    ),
    menuSubItem("2020-03-18 Piemonte", tabName = "20200318Piemonte",
         icon = icon("flag")
    ),
    menuSubItem("2020-03-18 FVG", tabName = "20200318Fvg",
         icon = icon("flag")
    ),
    menuSubItem("2020-03-14 Veneto", tabName = "20200314Veneto",
          icon = icon("flag")
    )
  ),

  menuItem("Andamento epidemia", icon = icon("chart-line"),
    menuSubItem("Nazionale", tabName = "national", icon = icon("flag")),
    menuSubItem("Regionale", tabName = "regional", icon = icon("map")),
    menuSubItem("Provinciale", tabName = "provincial", icon = icon("location-arrow"))
  ),

  menuItem("Indici principali", tabName = "impact", icon = icon("compass")),

  menuItem("Mappe",  tabName = "geo_spatialTot", icon = icon("map-marked-alt") ),

  menuItem("Segnalazioni", icon = icon("exclamation-triangle"),
    href = "https://github.com/UBESP-DCTV/covid19ita/issues/"
  ),

  menuItem("Fonti e informazioni", tabName = "data_tab", icon = icon("database")),

  mod_info_sidebar_ui("summary_today")

))}




#' Body dashboard
#'
#' Helper function defining the main body of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardBody box
#' @noRd
dashboard_body <- function() {dashboardBody(
  tags$head(includeScript(app_sys('app/www/google-analytics.js'))),
  tabItems(
    dashboard_home_body(),

    tabItem(
      tabName = "20200415TampHosp",
      h2("Impatto dei tamponi sulle ospedalizzazioni"),
      mod_focus_20200415_tamponi_ui("tamp_hosp")
    ),
    tabItem(tabName = "20200406MortVeneto",
            h2("Analisi preliminare della mortalit\u00E0  generale in 122 comuni veneti durante il periodo da 1 a 21 Marzo 2020."),
            mod_focus_20200406_mort_veneto_ui("mort_veneto")
    ),
    tabItem(tabName = "20200405Mort",
            h2("Analisi preliminare della mortalit\u00E0  generale in 1084 comuni italiani durante il periodo da 1 a 21 Marzo 2020."),
            mod_focus_20200404_magnani_ui("mortality")
    ),
    tabItem(tabName = "20200331Comp",
            h2("Analisi comparativa tra Regione Piemonte e Regione Veneto dei dati epidemiologici relativi all'infezione da Covid-19."),
            mod_focus_20200331_ui("ven_pie")
    ),
    tabItem(tabName = "20200328Tot_hosp",
            h2("Possibile effetto sulle ospedalizzazioni delle politiche sanitarie in Veneto"),
            mod_focus_20200328_hosp_ui("tot")
    ),
    tabItem(tabName = "20200325Hosp",
            h2("Possibile effetto sulle ospedalizzazioni delle politiche sanitarie in Veneto"),
            mod_focus_20200325_hosp_ui("hosp")
    ),
    tabItem(tabName = "20200323Picco",
            h2("Impatto dell'incertezza statistica sulle previsioni dell'andamento del COVID-19"),
            mod_focus_20200323_picco_ui("picco")
    ),
    tabItem(tabName = "20200321Alessandria",
            h2("Stime previsive sul totale casi ad Alessandria"),
            mod_focus_20200320_novara_ui("da_alessandria")
    ),
    tabItem(tabName = "20200321Vercelli",
            h2("Stime previsive sul totale casi a Vercelli"),
            mod_focus_20200320_novara_ui("da_vercelli")
    ),
    tabItem(tabName = "20200320Novara",
            h2("Stime previsive sul totale casi a Novara"),
            mod_focus_20200320_novara_ui("da_novara")
    ),
    tabItem(tabName = "20200319Veneto",
            h2("Possibile impatto delle politiche sanitarie sull'occupazione dei posti letto nelle terapie intensive in Veneto"),
            mod_focus_20200318_veneto_intensive_ui("21")
    ),
    tabItem(tabName = "20200318Piemonte",
            h2("Possibile effetto delle politiche sanitarie in Piemonte"),
            mod_focus_20200318_piemonte_ui("20200318_piemonte")
    ),
    tabItem(tabName = "20200318Fvg",
            h2("Possibile effetto delle politiche sanitarie in Friuli Venezia Giulia"),
            mod_focus_20200318_friuli_ui("20200318_fvg")
    ),
    tabItem(tabName = "20200314Veneto",
            h2("Possibile effetto delle politiche sanitarie in Veneto"),
            mod_focus_20200314_ui("dapb")
    ),



    tabItem(tabName = "national",
      h2("Eventi nazionali"),
      box(width = 12, title = "Istruzioni",
        p("\u00C8  possibile attivare/disattivare la visualizzazione di una o pi\u00F9  misure dal grafico facendo click sulle corrispondenti voci in legenda. Doppio-click per attivare/disattivare la visualizzazione esclusiva della voce selezionata."),
        p("Fare click sul pulsante autoscale (terzo) per espandere il grafico alla massima grandezza interamente visionabile."),
      ),
      mod_ts_ita_ui("ts_nat_cum", title = "Serie storiche degli eventi cumulati"),
      mod_ts_ita_ui("ts_nat_inc", title = "Serie storiche dei nuovi eventi giornalieri")
    ),


    tabItem(tabName = "regional",
      h2("Eventi regionali"),
      box(width = 12, title = "Istruzioni",
        p("\u00C8  possibile aggiungere o rimuovere la computazione dei grafici per una o pi\u00F9  regione/misura selezionandola o deselezionandola dal corrispondente box."),
        p("NOTA: la misurazione dei tamponi effettuati \u00E8  selezionabile ma disattivata di default in quanto fuori scala rispetto alle altre misure."),
        p(""),
        p("\u00C8  possibile attivare/disattivare la visualizzazione di una o pi\u00F9  regioni/misure dal grafico facendo click sulle corrispondenti voci in legenda. Doppio-click per attivare/disattivare la visualizzazione esclusiva della voce selezionata."),
        p("Fare click sul pulsante autoscale (terzo) per espandere il grafico alla massima grandezza interamente visionabile."),
      ),

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
      p("\u00C8  possibile aggiungere o rimuovere la computazione dei grafici per una o pi\u00F9  regione selezionandola o deselezionandola dal corrispondente box."),
      p("\u00C8  possibile attivare/disattivare la visualizzazione di una o pi\u00F9  regioni dal grafico facendo click sulle corrispondenti voci in legenda. Doppio-click per attivare/disattivare la visualizzazione esclusiva della voce selezionata."),
      p("Fare click sul pulsante autoscale (terzo) per espandere il grafico alla massima grandezza interamente visionabile."),

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

      p("L'applicazione utilizza i dati ufficiali riguardanti la situazione COVID-19 italiana, a livello nazionale, regionale e provinciale."),

      p(
        'Tali dati sono inizialmente gestiti, processati e messi a disposizione dalla ',
        a(href = "http://www.protezionecivile.it/web/guest/home", target = "_blank", "Presidenza del Consiglio dei Ministri - Dipartimento di Protezione Civile"),
        ' con licenza ', a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
        'cos\u00EC  come forniti dal ', a(href = "", target = "_blank", "Ministero della Salute.")
      ),

      p('Di norma, i dati sono aggiornati alle ore 18:00 di ogni giorno.'),

      p(
        "Per ulteriori informazioni sulla loro attribuzione, disponibilita, e uso, si consiglia di visitare la",
        a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = "_blank", "pagina del progetto covid19ita"),
        "."
      ),

      h2("Software"),
      p(HTML("L'applicazione <strong>covid19ita</strong> \u00E8  stata sviluppata in R ver. 3.6.3 come un suo pacchetto di espansione. Il codice sorgente del pacchetto e dell'applicazione\u00E8 ? liberamente disponibile disponibile su GitHub all'indirizzo "),
        a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = '_blank', 'https://github.com/UBESP-DCTV/covid19ita/'), "."
      ),

      p("Per il suo sviluppo sono stati utilizzati i pacchetti di espansione {shiny} ver. 1.4.0, {shinydashboard} v.0.7.1 e {golem} ver. 0.2.1."),

      p("Le analisi sono state eseguite sfruttando le funzioni dei pacchetti {stats} ver. 3.6.3, {gam} ver. 1.16.1, e {mgcv} ver. 1.8-31"),

      p("I grafici sono stati prodotti grazie ai pacchetti {ggplot2} ver. 3.3.0 e {plotly} ver. 4.9.2."),

      h3('Nota per gli utilizzatori di R'),
      p(
        'Oltre a questa stessa applicazione in s\u00E9  (che pu\u00F2  essere eseguita localmente installando il pacchetto {covid19ita} ed eseguendo l\'istruzione `run_app()`), ll pacchetto R {covid19ita}, disponibile su ',
        a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = '_blank', 'GitHub'),
        ' e sviluppato anch\'esso sotto licenza ',
        a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
        ', mette a disposizione tali dati, senza alcun processamento ulteriore rispetto a quelli originali, per un loro diretto utilizzo in R.'
      )

    ),


    tabItem(tabName = "impact",
      h1("Indici principali"),
      mod_ind_ita_ui("20200315")
    ),

    tabItem(tabName = "geo_spatialTot",
     # h1("Mappe 1"),
      mod_maps_ui("geo_1")
    )


  )
)}




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
      app_title = ' {covid19ita} ',
      meta = list(
        title = "{covid19ita} App",
        description = "Piattaforma di monitoraggio e analisi dell'infezione da COVID-19 in Italia.",
        url = "https://r-ubesp.dctv.unipd.it/shiny/covid19ita/",
        image = app_sys('app/www/ubep_covid.png'),
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



