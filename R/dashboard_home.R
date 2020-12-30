#' Dashboard's HOME (sidebar)
#'
#' Helper function defining the sidebar of the dashboard's HOME page
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
dashboard_home_sidebar <- function() {
  menuItem("Home", tabName = "home", icon = icon("home"))
}




#' Dashboard's HOME (body)
#'
#' Helper function defining the body of the dashboard's HOME page
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
dashboard_home_body <- function() {
  tabItem(
    tabName = "home",
    fluidPage(
      title = HTML("Progetto <strong>covid19ita</strong>"),

      p('Nota: sito ottimizzato per l\'uso da PC, in caso di utilizzo da cellulare o tablet si consiglia di tenere il dispositivo in modalit\u00E0  "orizzontale".'),

      box(
        width = 12,
        p(
          HTML("Il progetto <strong>covid19ita</strong> \u00E8  stato sviluppato dall'"),
          a(href = "https://ubesp.jimdofree.com/", target = "_blank", "Unit\u00E0  di Biostatistica, Epidemiologia, e Sanit\u00E0 Pubblica"),
          " del ", a(href = "https://www.dctv.unipd.it/", target = "_blank", "Dipartimento di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0 Pubblica"),
          " dell'", a(href = "https://www.unipd.it/", target = "_blank", "Universit\u00E0  degli Studi di Padova"), ",",
          " in collaborazione con il ", a(href = "https://www.dscb.unito.it/do/gruppi.pl/Tree", target = "_blank", "Dipartimento di Scienze Cliniche e Biologiche"),
          " dell'", a(href = "https://www.unito.it/", target = "_blank", "Universit\u00E0  degli Studi di Torino"), ",",
          " e del", a(href = "https://www.dimet.uniupo.it/", target = "_blank", "Dipartimento di Medicina Traslazionale"),
          " dell'", a(href = "https://www.uniupo.it/", target = "_blank", "Universit\u00E0  del Piemonte Orientale"), "."
        )
      ),

      h2("Gruppo di lavoro"),
      box(
        width = 12, title = HTML("<strong>Coordinatore del progetto</strong>"),
        p(
          HTML("Prof. <strong>Dario Gregori</strong>, Ph.D., responsabile dell'Unit\u00E0  di
            Biostatistica, Epidemiologia e Sanit\u00E0  Pubblica del Dipartimento
            di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0  Pubblica --
            Universit\u00E0  degli studi di Padova."),
          a(href = "https://linkedin.com/in/dario-gregori-2720039/", target = "_blank", "LinkedIn")
        )
      ),

      box(
        width = 12, title = HTML("<strong>Sviluppo applicazione e R package</strong>"),
        p(
          HTML("<strong>Corrado Lanera</strong>, Ph.D., Unit\u00E0  di
          Biostatistica, Epidemiologia e Sanit\u00E0  Pubblica del Dipartimento
          di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0  Pubblica --
          Universit\u00E0  degli studi di Padova. Responsabile del Laboratorio di Intelligenza Artificiale per le Scienze Mediche"),
          a(href = "https://linkedin.com/in/corradolanera/", target = "_blank", "LinkedIn")
        )
      ),

      box(
        width = 12, title = HTML("<strong>Modellistica Epidemiologica</strong>"),
        p(
          HTML("Prof. <strong>Paola Berchialla</strong>, Ph.D., Dipartimento di Scienze
            Cliniche e Biologiche -- Universit\u00E0  degli Studi di Torino"),
          a(href = "https://linkedin.com/in/paola-berchialla-36b44410/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML(
            'Prof. <strong>Dolores Catelan</strong>, Ph.D., Dipartimento di
            Statistica, Informatica, Applicazioni "G. Parenti" (DISIA),
            Universit\u00E0  degli Studi di Firenze'
          ),
          a(
            href = "https://www.linkedin.com/in/dolores-catelan-43998b23/",
            target = "_blank", "LinkedIn"
          )
        )
      ),

      box(
        width = 12, title = HTML("<strong>Modelli Previsivi</strong>"),
        p(
          HTML("<strong>Danila Azzolina</strong>, Ph.D., Dipartimento di Medicina Traslazionale --
          Universit\u00E0  del Piemonte Orientale"),
          a(href = "https://linkedin.com/in/danila-azzolina-862465166/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML("<strong>Ilaria Prosepe</strong>, MSc., Unit\u00E0  di
           Biostatistica, Epidemiologia e Sanit\u00E0  Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0  Pubblica --
           Universit\u00E0  degli studi di Padova."),
          a(href = "https://linkedin.com/in/ilaria-prosepe-1b52371a4/", target = "_blank", "LinkedIn")
        )
      ),

      box(
        width = 12, title = HTML("<strong>Modelli di Epidemiologia Ambientale e Inquinamento</strong>"),
        p(
          HTML(
            'Prof. <strong>Annibale Biggeri</strong>, MD, MSPH, MSB,
              Dipartimento di Statistica, Informatica, Applicazioni
              "G. Parenti" (DISIA), Universit\u00E0  degli Studi di Firenze'
          ),
          a(href = "https://linkedin.com/in/danila-azzolina-862465166/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML(
            "Prof. <strong>Cristina Canova</strong>, Ph.D., Unit\u00E0  di
           Biostatistica, Epidemiologia e Sanit\u00E0  Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0  Pubblica --
           Universit\u00E0  degli studi di Padova."
          ),
          a(href = "https://www.linkedin.com/in/cristina-canova-05448861/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML("<strong>Elisa Gallo</strong>, MSc., Unit\u00E0  di
           Biostatistica, Epidemiologia e Sanit\u00E0  Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0  Pubblica --
           Universit\u00E0  degli studi di Padova."),
          a(href = "https://www.linkedin.com/in/elisa-gallo-9b3933152/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML("<strong>Francesco Garzotto</strong>, MSc, Unit\u00E0  di
           Biostatistica, Epidemiologia e Sanit\u00E0  Pubblica del Dipartimento
           di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0  Pubblica --
           Universit\u00E0  degli studi di Padova."),
          a(href = "https://www.linkedin.com/in/francesco-garzotto-19907826/", target = "_blank", "LinkedIn")
        )
      ),
      box(
        width = 12, title = HTML("<strong>Analisi Geospaziale e Rappresentazione Cartografica</strong>"),
        p(
          HTML("Prof. <strong>Francesco Pirotti</strong>, Ph.D.,
            Dipartimento Territorio e Sistemi Agro-Forestali (TESAF)
            CIRGEO - Centro Interdipartimentale di Ricerca di Geomatica."),
          a(href = "https://www.linkedin.com/in/fpirotti", target = "_blank", "LinkedIn")
        )
      ),
      box(
        width = 12, title = HTML("<strong>Analisi della mortalit\u00E0</strong>"),
        p(
          HTML("Prof. <strong>Corrado Magnani</strong>, Ph.D.,
               Dipartimento di Medicina Traslazionale --- Universit\u00E0
               del Piemonte Orientale, Novara."),
          a(href = "https://www.linkedin.com/in/corrado-magnani-173a50108/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML("<strong>Daniela Ferrante</strong>, Ph.D.,
               Dipartimento di Medicina Traslazionale --- Universit\u00E0
               del Piemonte Orientale, Novara."),
          a(href = "https://www.linkedin.com/in/daniela-ferrante-05507494/", target = "_blank", "LinkedIn")
        )
      ),

      box(
        width = 12, title = HTML("<strong>Comunicazione del Rischio</strong>"),
        p(
          HTML("<strong>Giulia Lorenzoni</strong>, Ph.D., Unit\u00E0  di
            Biostatistica, Epidemiologia e Sanit\u00E0  Pubblica del Dipartimento
            di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0  Pubblica --
            Universit\u00E0  degli studi di Padova. Responsabile del Laboratorio di Epidemiologia Clinica e Digital Health"),
          a(href = "https://linkedin.com/in/giulia-lorenzoni-b382a6180", target = "_blank", "LinkedIn")
        ),
        p(
          HTML(
            "<strong>Nicolas Destro</strong>, MA, Unit\u00E0  di
            Biostatistica, Epidemiologia e Sanit\u00E0  Pubblica del Dipartimento
            di Scienze Cardio- Toraco- Vascolari e Sanit\u00E0  Pubblica --
            Universit\u00E0  degli studi di Padova."
          ),
          a(href = "https://www.linkedin.com/in/nicolas-destro-b2a67212b/", target = "_blank", "LinkedIn")
        )
      ),
      h2("Istruzioni per la navigazione"),
      box(
        width = 12, title = HTML("<strong>Organizzazione del sito</strong>"),
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

      box(
        width = 12, title = HTML("<strong>Utilizzo dei grafici dinamici</strong>"),
        p(HTML("La maggior parte dei grafici riportati nel sito sono dinamici in due modi distinti:")),
        p(""),
        p(HTML("1. <strong>rispetto alle informazioni
               riportate</strong>: all'interno del grafico \u00E8  possibile
               <strong>visualizzare ulteriori dettagli</strong> passando
               il cursore o facendo click sui vari punti/curve
               riportate, \u00E8  possibile <strong>zoommare</strong> su
               alcune zone di interesse tramite i pulsanti +/- in
               semi-trasparenza in alto a destra nel grafico (o
               selezionando l'area con il puntatore). Nel caso di
               informazioni multiple (per esempio pi\u00F9  regioni o pi\u00F9
               misure) riportate nello stesso grafico, \u00E8  possibile
               <strong>escludere alcune informazioni</strong> facendo
               click sulla relativa voce in legenda, o
               <strong>mantenere attiva una sola informazione di
               interesse</strong> tramite un doppio click. \u00C8  inoltre
               possibile <strong>salvare ciascun grafico</strong>, in
               modo indipendente e cos\u00EC  come visualizzato, selezionando
               l'icona semitrasparente della macchina fotografica.
               Tramite la pressione dell'icona a forma di casetta \u00E8
               possibile <strong>ripristinare la visione
               originale</strong> del grafico visualizzato.")),
        p(HTML(
          "2. <strong>rispetto a quali/quante informazioni elaborare e
          riportare</strong>: In caso compaiano delle celle sopra il
          grafico in cui poter selezionare <strong>regioni, province o
          misure, \u00E8  possibile sia escluderne che aggiungerne</strong> di
          ulteriori (tra quelle disponibili selezionando il riquadro).
          Una volta selezionato o deselezionato quanto di interesse, i
          grafici si aggiorneranno moltiplicandosi o riducendosi, cos\u00EC
          come moltiplicando o riducento le infornmazioni contenute in
          ciascuno di essi."
        )),
      ),
    )
  )
}


#' Dashboard's HOME (body)
#'
#' Helper function defining the body of the dashboard's HOME page
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
login_page <- function() {

  div(id = "loginpage",
      style = "width: 500px; max-width: 100%; margin: 0 auto;
               padding: 20px;",
      wellPanel(
        tags$h2("LOG IN",
                class = "text-center",
                style = "padding-top: 0;color:#333; font-weight:600;"),

        textInput("userName",
                  placeholder = "Username",
                  label = tagList(icon("user"), "Username")),

        passwordInput("passwd",
                      placeholder = "Password",
                      label = tagList(icon("unlock-alt"), "Password")),

        br(),

        div(
          style = "text-align: center;",
          actionButton("login", "SIGN IN",
                       style = "color: white; background-color:#3c8dbc;
                                padding: 10px 15px; width: 150px;
                                cursor: pointer; font-size: 18px;
                                font-weight: 600;"),
          shinyjs::hidden(
            div(id = "nomatch",
                tags$p("Oops! Incorrect username or password!",
                       style = "color: red; font-weight: 600;
                                padding-top: 5px;font-size:16px;",
                       class = "text-center"))),
          br(),
          br(),
          tags$code("Lasciare credenziali in bianco e cliccare direttamente \"SIGN IN\" per l'accesso alla piattaforma pubblica")
        ))
  )
}
