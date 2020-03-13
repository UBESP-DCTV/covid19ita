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

    dashboardPage(title = "COVID-19 - Italia",
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
  id = "sidebar",

  mod_help_plot_ui("help"),

  menuItem("Home", tabName = "home", icon = icon("home")),

  menuItem("In evidenza", tabName = "focus", icon = icon("bullseye")),

  menuItem("Andamento epidemia", icon = icon("chart-line"),
    menuSubItem("Nazionale", tabName = "national", icon = icon("flag")),
    menuSubItem("Regionale", tabName = "regional", icon = icon("map")),
    menuSubItem("Provinciale", tabName = "provincial", icon = icon("location-arrow"))
  ),

  menuItem("Impatto epidemia", tabName = "impact", icon = icon("compass")),

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
          a(href = "https://linkedin.com/in/dario-gregori-2720039", target = "_blank", "LinkedIn")
        )
      ),

      box(width = 12, title = HTML("<strong>Sviluppo applicazione e R package</strong>"),
        p(HTML("<strong>Corrado Lanera</strong>, Ph.D., Unità di
          Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
          di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
          Università degli studi di Padova. Responsabile del Laboratorio di Intelligenza Artificiale per le Scienze Mediche"),
          a(href = "https://linkedin.com/in/corradolanera", target = "_blank", "LinkedIn")
        )
      ),

      box(width = 12, title = HTML("<strong>Modellistica Epidemiologica</strong>"),
        p(
          HTML("Prof. <strong>Paola Berchialla</strong>, Ph.D., Dipartimento di Scienze
            Cliniche e Biologiche -- Università degli Studi di Torino"
          ),
          a(href = "https://linkedin.com/in/paola-berchialla-36b44410", target = "_blank", "LinkedIn")
        ),

        p(HTML("<strong>Danila Azzolina</strong>, Ph.D., Dipartimento di Medicina Traslazionale --
          Università del Piemonte Orientale"),
          a(href = "https://linkedin.com/in/danila-azzolina-862465166", target = "_blank", "LinkedIn")
        ),

        p(HTML("<strong>Ilaria Prosepe</strong>, MS., Unità di
           Biostatistica, Epidemiologia e Sanità Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanità Pubblica --
           Università degli studi di Padova."),
          a(href = "https://linkedin.com/in/ilaria-prosepe-1b52371a4", target = "_blank", "LinkedIn")
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
          a(href = "https://linkedin.com/in/ilaria-prosepe-1b52371a4", target = "_blank", "LinkedIn")
        )
      ),

            h2("Istruzioni per la navigazione"),
      box(width = 12, title = HTML("<strong>Organizzazione del sito</strong>"),
        HTML(
          "<ol>
            <li><strong>Home</strong>: Questa pagina.</li>
            <li><strong>In evidenza</strong>: Principali considerazioni per la regione Veneto.</li>
            <li><strong>Andamento epidemia</strong>: Serie temporali dinamiche e interattive.</li>
            <li><strong>Impatto epidemia</strong>: modelli e previsioni regionali e nazionali.</li>
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

    # fluidRow(
    #   box(width = 6, solidHeader = TRUE,
    #     mod_img_header_ui("logo_coda_torino")
    #   ),
    #   box(width = 6, solidHeader = TRUE,
    #     mod_img_header_ui("logo_coda_novara")
    #   )
    # )


  ),

  tabItem(tabName = "focus",

    h1("Valutazione dell'andamento dell'epidemia in Veneto"),
#
#     fluidRow(
#       mod_impact_veneto_a_ui("smooth_linear",
#         title = "Figura 1: Andamento nuovi casi",
#         footer = HTML("
#           Il grafico mostra il numero di nuovi casi predetti ipotizzando
#           un andamento lineare fino all'8 marzo (retta tratteggiata blu)
#           e il numero di nuovi casi reali grezzi (linea verde) e il
#           loro andamento approssimato (curva rossa).</br>
#           </br>
#           <strong>Interpretazione</strong>:</br>
#           - si osserva una tendenza (curva rossa) che
#           mostra un incremento del numero di nuovi casi reali a partire
#           dall'8 marzo, rispetto a quanto atteso ipotizzando un
#           andamento lineare (retta tratteggiata blu).
#         ")
#       )
#     ),
#
#
#     fluidRow(
#       mod_impact_veneto_b_ui("loess_veneto",
#         title = "Figura 2: Andamento casi totali (andamento esponenziale)",
#         footer = HTML("
#           Il grafico mostra (curva rossa) i casi totali osservati e
#           (curva blu) i casi totali predetti considerando i dati
#           osservati fino al 2 marzo (giornata a partire dalla quale si
#           ipotizza siano osservabili gli effetti dei provvedimenti
#           varati col Decreto-legge del 23/02/2020).</br>
#           I casi totali osservati e predetti sono stati modellati
#           ipotizzando un andamento esponenziale degli stessi.</br>
#           </br>
#           <strong>Interpretazione</strong>:</br>
#
#           - La curva blu mostra come si stima sarebbe progredito il
#             contagio se non fossero state introdotte le misure di
#             contenimento dell’epidemia in data 23 febbraio</br>
#           - La curva blu e la curva rossa si sovrappongono fino al
#             2 marzo (giornata a partire dalla quale si ipotizza siano
#             osservabili gli effetti dei provvedimenti varati col
#             Decreto-legge del 23/02/2020)</br>
# 	        - Lo spazio che separa le due curve a partire dal 2 marzo
# 	          rappresenta il numero di casi potenzialmente risparmiati
# 	          grazie all'introduzione delle misure di contenimento in data
# 	          23 febbraio
#         "),
#         width = 6
#       ),
#       mod_impact_veneto_b_ui("gam_veneto",
#         title = "Figura 2: Andamento casi totali (andamento lineare generalizzato)",
#         footer = HTML("
#           Il grafico mostra (curva rossa) i casi totali osservati e
#           (curva blu) i casi totali predetti considerando i dati
#           osservati fino al 2 marzo (giornata a partire dalla quale si
#           ipotizza siano osservabili gli effetti dei provvedimenti
#           varati col Decreto-legge del 23/02/2020).</br>
#           I casi totali osservati e predetti sono stati modellati
#           ipotizzando un andamento lineare generalizzato degli stessi.</br>
#           </br>
#           <strong>Interpretazione</strong>:</br>
#
#           - La curva blu e la curva rossa si sovrappongono fino al
#             2 marzo (giornata a partire dalla quale si ipotizza siano
#             osservabili gli effetti dei provvedimenti varati col
#             Decreto-legge del 23/02/2020)</br>
#           - A partire dal 2 marzo si inizia a osservare un guadagno in
#             termini di casi risparmiati che nel grafico è rappresentato
#             dal progressivo aumento della distanza tra le due curve</br>
#           - A partire dal 9-10 marzo si osserva un’inversione di
#             tendenza: si riduce la distanza tra le curve che arrivano a
#             intersecarsi e successivamente i casi osservati superano
#             quelli predetti</br>
#           - Questa inversione di tendenza riflette l’aumento dei nuovi
#             casi mostrato nella Figura 1</br>
#           - L’inversione di tendenza si traduce in una perdita del
#             guadagno di casi risparmiati nella settimana precedente
#             (2-8 marzo)
#         "),
#         6
#       )
#     )
#
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

    p("Le analisi sono state eseguite sfruttando le funzioni dei pacchetti {stats} ver. 3.6.3, e {gam} ver. 1.16.1"),

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
    h1("Valutazione dell'andamento dell'epidemia"),
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

