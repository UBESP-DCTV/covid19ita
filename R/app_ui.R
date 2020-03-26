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
      message = "2020-03-25 Veneto: Ospedalizzazioni",
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

  menuItem("Home", tabName = "home", icon = icon("home")),


  menuItem("In evidenza", icon = icon("bullseye"),
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
  tabItem(tabName = "home",
    fluidPage(title = HTML("Progetto <strong>covid19ita</strong>"),

      p('Nota: sito ottimizzato per l\'uso da PC, in caso di utilizzo da cellulare o tablet si consiglia di tenere il dispositivo in modalità "orizzontale".'),

      # box(width = 12, solidHeader = TRUE,
      #   mod_img_header_ui("logo_testa")
      # ),

      box(width = 12,
        p(
          HTML("Il progetto <strong>covid19ita</strong> è stato sviluppato dall'"),
          a(href = 'https://ubesp.jimdofree.com/', target = '_blank', 'Unità di Biostatistica, Epidemiologia, e Sanità Pubblica'),
          " del ", a(href = 'https://www.dctv.unipd.it/', target = '_blank', 'Dipartimento di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica'),
          " dell'", a(href = 'https://www.unipd.it/', target = '_blank', 'Università degli Studi di Padova'), ",",
          " in collaborazione con il ", a(href = 'https://www.dscb.unito.it/do/gruppi.pl/Tree', target = '_blank', 'Dipartimento di Scienze Cliniche e Biologiche'),
          " dell'", a(href = 'https://www.unito.it/', target = '_blank', 'Università degli Studi di Torino'), ",",
          " e del", a(href = 'https://www.dimet.uniupo.it/', target = '_blank', 'Dipartimento di Medicina Traslazionale'),
          " dell'", a(href = 'https://www.uniupo.it/', target = '_blank', 'Università del Piemonte Orientale'), "."
        )
      ),

      h2("Gruppo di lavoro"),
      box(width = 12, title = HTML("<strong>Coordinatore del progetto</strong>"),
        p(
          HTML("Prof. <strong>Dario Gregori</strong>, Ph.D., responsabile dell'Unità di
            Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
            di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
            Università degli studi di Padova."
          ),
          a(href = "https://linkedin.com/in/dario-gregori-2720039/", target = "_blank", "LinkedIn")
        )
      ),

      box(width = 12, title = HTML("<strong>Sviluppo applicazione e R package</strong>"),
        p(HTML("<strong>Corrado Lanera</strong>, Ph.D., Unità di
          Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
          di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
          Università degli studi di Padova. Responsabile del Laboratorio di Intelligenza Artificiale per le Scienze Mediche"),
          a(href = "https://linkedin.com/in/corradolanera/", target = "_blank", "LinkedIn")
        )
      ),

      box(width = 12, title = HTML("<strong>Modellistica Epidemiologica</strong>"),
        p(
          HTML("Prof. <strong>Paola Berchialla</strong>, Ph.D., Dipartimento di Scienze
            Cliniche e Biologiche -- Università degli Studi di Torino"
          ),
          a(href = "https://linkedin.com/in/paola-berchialla-36b44410/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML(
            'Prof. <strong>Dolores Catelan</strong>, Ph.D., Dipartimento di
            Statistica, Informatica, Applicazioni "G. Parenti" (DISIA),
            Università degli Studi di Firenze'
          ),
          a(
            href = "https://www.linkedin.com/in/dolores-catelan-43998b23/",
            target = "_blank", "LinkedIn"
          )
        )
      ),

      box(width = 12, title = HTML("<strong>Modelli Previsivi</strong>"),
          p(HTML("<strong>Danila Azzolina</strong>, Ph.D., Dipartimento di Medicina Traslazionale --
          Università del Piemonte Orientale"),
            a(href = "https://linkedin.com/in/danila-azzolina-862465166/", target = "_blank", "LinkedIn")
          ),
          p(HTML("<strong>Ilaria Prosepe</strong>, MSc., Unità di
           Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
           Università degli studi di Padova."),
            a(href = "https://linkedin.com/in/ilaria-prosepe-1b52371a4/", target = "_blank", "LinkedIn")
          )
      ),

      box(width = 12, title = HTML("<strong>Modelli di Epidemiologia Ambientale e Inquinamento</strong>"),
          p(
            HTML(
              'Prof. <strong>Annibale Biggeri</strong>, MD, MSPH, MSB,
              Dipartimento di Statistica, Informatica, Applicazioni
              "G. Parenti" (DISIA), Università degli Studi di Firenze'
            ),
            a(href = "https://linkedin.com/in/danila-azzolina-862465166/", target = "_blank", "LinkedIn")
          ),
          p(
            HTML(
              'Prof. <strong>Cristina Canova</strong>, Ph.D., Unità di
           Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
           Università degli studi di Padova.'
            ),
            a(href = "https://www.linkedin.com/in/cristina-canova-05448861/", target = "_blank", "LinkedIn")
          ),
          p(HTML("<strong>Elisa Gallo</strong>, MSc., Unità di
           Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
           Università degli studi di Padova."),
            a(href = "https://www.linkedin.com/in/elisa-gallo-9b3933152/", target = "_blank", "LinkedIn")
          ),
          p(HTML("<strong>Francesco Garzotto</strong>, MSc, Unità di
           Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
           Università degli studi di Padova."),
            a(href = "https://www.linkedin.com/in/francesco-garzotto-19907826/", target = "_blank", "LinkedIn")
          )
      ),

      box(width = 12, title = HTML("<strong>Comunicazione del Rischio</strong>"),
        p(
          HTML("<strong>Giulia Lorenzoni</strong>, Ph.D., Unità di
            Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
            di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
            Università degli studi di Padova. Responsabile del Laboratorio di Epidemiologia Clinica e Digital Health"
          ),
          a(href = "https://linkedin.com/in/giulia-lorenzoni-b382a6180", target = "_blank", "LinkedIn")
        ),
        p(
          HTML(
            "<strong>Nicolas Destro</strong>, MA, Unità di
            Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
            di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
            Università degli studi di Padova."
          ),
          a(href = "https://www.linkedin.com/in/nicolas-destro-b2a67212b/", target = "_blank", "LinkedIn")
        )
      ),

            h2("Istruzioni per la navigazione"),
      box(width = 12, title = HTML("<strong>Organizzazione del sito</strong>"),
        HTML(
          "<ol>
            <li><strong>Home</strong>: Questa pagina.</li>
            <li><strong>In evidenza</strong>: Considerazioni su temi specifici locali o regionali.</li>
            <li><strong>Andamento epidemia</strong>: Serie temporali dinamiche e interattive.</li>
            <li><strong>Indici principali</strong>: Principali indici di interesse locale, regionale o nazionale.</li>
            <li><strong>Segnalazioni</strong>: link per segnalazioni tecniche relative all'applicazione.</li>
            <li><strong>Fonti e informazioni</strong>: Descrizione fonti, licenze d'uso e software utilizzati per lo sviluppo.</li>
            <li><strong>Metriche ultima giornata</strong>: Selezionando una regione (o 'Italia', default) vengono visualizzate le principali metriche dell'ultimo aggiornamento dati, che normalmente avviene alle ore 18 da parte della protezione civile.</li>
          </ol>"
        )
      ),

      box(width = 12, title = HTML("<strong>Utilizzo dei grafici dinamici</strong>"),
          p(HTML("La maggior parte dei grafici riportati nel sito sono dinamici in due modi distinti:")),
          p(""),
          p(HTML("1. <strong>rispetto alle informazioni riportate</strong>: all'interno del grafico è possibile <strong>visualizzare ulteriori dettagli</strong> passando il cursore o facendo click sui vari punti/curve riportate, è possibile <strong>zoommare</strong> su alcune zone di interesse tramite i pulsanti +/- in semi-trasparenza in alto a destra nel grafico (o selezionando l'area con il puntatore). Nel caso di informazioni multiple (per esempio più regioni o più misure) riportate nello stesso grafico, è possibile <strong>escludere alcune informazioni</strong> facendo click sulla relativa voce in legenda, o <strong>mantenere attiva una sola informazione di interesse</strong> tramite un doppio click. È inoltre possibile <strong>salvare ciascun grafico</strong>, in modo indipendente e così come visualizzato, selezionando l'icona semitrasparente della macchina fotografica. Tramite la pressione dell'icona a forma di casetta è possibile <strong>ripristinare la visione originale</strong> del grafico visualizzato.")),
          p(HTML("2. <strong>rispetto a quali/quante informazioni elaborare e riportare</strong>: In caso compaiano delle celle sopra il grafico in cui poter selezionare <strong>regioni, province o misure, è possibile sia escluderne che aggiungerne</strong> di ulteriori (tra quelle disponibili selezionando il riquadro). Una volta selezionato o deselezionato quanto di interesse, i grafici si aggiorneranno moltiplicandosi o riducendosi, così come moltiplicando o riducento le infornmazioni contenute in ciascuno di essi.")),
      ),

    )

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

    p("L'applicazione utilizza i dati ufficiali riguardanti la situazione COVID-19 italiana, a livello nazionale, regionale e provinciale."),

    p(
      'Tali dati sono inizialmente gestiti, processati e messi a disposizione dalla ',
      a(href = "http://www.protezionecivile.it/web/guest/home", target = "_blank", "Presidenza del Consiglio dei Ministri - Dipartimento di Protezione Civile"),
      ' con licenza ', a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
      'così come forniti dal ', a(href = "", target = "_blank", "Ministero della Salute.")
    ),

    p('Di norma, i dati sono aggiornati alle ore 18:00 di ogni giorno.'),

    p(
      "Per ulteriori informazioni sulla loro attribuzione, disponibilita, e uso, si consiglia di visitare la",
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = "_blank", "pagina del progetto covid19ita"),
      "."
    ),

    h2("Software"),
    p(HTML("L'applicazione <strong>covid19ita</strong> è stata sviluppata in R ver. 3.6.3 come un suo pacchetto di espansione. Il codice sorgente del pacchetto e dell'applicazione è liberamente disponibile disponibile su GitHub all'indirizzo "),
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = '_blank', 'https://github.com/UBESP-DCTV/covid19ita/'), "."
    ),

    p("Per il suo sviluppo sono stati utilizzati i pacchetti di espansione {shiny} ver. 1.4.0, {shinydashboard} v.0.7.1 e {golem} ver. 0.2.1."),

    p("Le analisi sono state eseguite sfruttando le funzioni dei pacchetti {stats} ver. 3.6.3, {gam} ver. 1.16.1, e {mgcv} ver. 1.8-31"),

    p("I grafici sono stati prodotti grazie ai pacchetti {ggplot2} ver. 3.3.0 e {plotly} ver. 4.9.2."),

    h3('Nota per gli utilizzatori di R'),
    p(
      'Oltre a questa stessa applicazione in sé (che può essere eseguita localmente installando il pacchetto {covid19ita} ed eseguendo l\'istruzione `run_app()`), ll pacchetto R {covid19ita}, disponibile su ',
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = '_blank', 'GitHub'),
      ' e sviluppato anch\'esso sotto licenza ',
      a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
      ', mette a disposizione tali dati, senza alcun processamento ulteriore rispetto a quelli originali, per un loro diretto utilizzo in R.'
    )

  ),


  tabItem(tabName = "impact",
    h1("Indici principali"),
    mod_ind_ita_ui("20200315")
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
