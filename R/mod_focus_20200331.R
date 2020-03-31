#' focus_20200331 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200331_ui <- function(id) {
  ns <- NS(id)

  # User interface: WEB side
  fluidPage(
    fluidRow(
      box(width = 12,

        p("
          Sono stati analizzati i dati relativi al numero di casi
          positivi, al numero di tamponi effettuati, al numero di
          ospedalizzazioni, al numero di casi di terapia intensiva e al
          numero di decessi compresi nell'arco temporale dal 28 febbraio
          2020 al 30 marzo 2020 nelle regioni Piemonte e Veneto.
        "),

        p("
          Le due regioni hanno adottato approcci diversi nell'utilizzo
          del tampone naso-faringeo; il Piemonte ha applicato da subito
          le direttive del Consiglio Superiore di Sanità e nelle prime
          fasi dell'epidemia ha sottoposto a tampone solamente le
          persone che presentavano segni clinici da covid-19, ma che al
          contempo avessero i criteri epidemiologici (contatto con
          soggetto proveniente dalle zone a rischio); la regione Veneto,
          di sua iniziativa, ha ritenuto di sottoporre al tampone tutti
          i pazienti che presentavano sintomi da covid-19 e,
          probabilmente, i relativi contatti. Questo differente modus
          operandi ha portato nelle prime fasi dell'epidemia (dal 28
          febbraio al 8 marzo 2020) a evidenziare maggiori casi nella
          Regione Veneto (Figura 1).
        "),

        p("
          In data 8 marzo 2020 è stata dichiarata la zona rossa estesa a
          tutta Italia con conseguente lock-down (progressivo) della
          circolazione e delle attività.
        "),

        p("
          Sebbene in una prima fase (circa una settimana) si sia
          continuato a vedere un numero inferiore di casi in Piemonte,
          le curve del numero di ospedalizzazioni delle due regioni,
          prese in esame, hanno subito una netta biforcazione a carico
          della Regione Piemonte, in cui si è osservato un aumento
          significativo dei pazienti ospedalizzati (Figura 2), e
          parimenti anche le curve relative al ricorso alle terapie
          intensive (Figura 3) e il numero di decessi si sono
          distanziate nelle due realtà (Figura 4).
        "),

        p("
          La separazione delle curve di ospedalizzazione delle due
          regioni attorno al 12 marzo (quando il Piemonte ha cominciato
          a crescere più rapidamente del Veneto, pur avendo
          apparentemente meno casi) potrebbe essere l’effetto di un
          ridotto numero di tamponi che ha reso il numero delle persone
          individuate molto più basso rispetto a quello reale,
          sovrastimando il dato di letalità (morti per la
          malattia/numero totale di affetti dalla malattia). La politica
          del Veneto di fare subito più tamponi, probabilmente
          stimolata dalla individuazione del primo focolaio di Vo’
          Euganeo, ha incrementato, per lo meno nella fase iniziale,
          la possibilità di isolare i positivi, andando così a
          proteggere le fasce di età più a rischio e riducendo la
          circolazione del virus ancora prima del lock-down
          dell'8 marzo. Questa dinamica di separazione delle curve di
          crescita delle due regioni si è riprodotta sui decessi con uno
          shift temporale di 5/6 giorni.
        "),

        p(HTML("
          Nella fase attuale dell’epidemia questo molto probabilmente
          comporta che:
          <br>
          <ol>
            <li> anche pazienti con sintomi, ma non risultanti positivi
                 vengono lasciati a casa e non avviano precocemente
                 alcuna terapia;
            <li> quando la situazione peggiora i pazienti si recano in
                 PS dove, se positivi, vengono ricoverati e possono
                 finalmente iniziare una terapia;
            <li> tuttavia iniziano la terapia più tardivamente di quanto
                 forse necessario per prevenire la progressione verso la
                 ARDS e, probabilmente, l’uso tardivo del tocilizumab ha
                 meno possibilità di successo;
            <li> con conseguente aumento dei decessi.
          </ol>
        ")),

        p("
          La nostra analisi suggerisce che per modificare questa
          sequenza di eventi e ridurre la necessità di ricovero e
          soprattutto ottimizzare il ricorso alla terapia intensiva, è
          necessario implementare il numero delle diagnosi domiciliari,
          estendendo l’indicazione all’esecuzione del tampone ai
          pazienti pauci-sintomatici. Questo consentirebbe non solo di
          ridurre i contagi ma anche di avviare una terapia
          antiinfiammatoria e/o antivirale a domicilio sotto il
          controllo di equipe di medici dedicati, con l’intento di
          prevenire o almeno di attenuare e ritardare la progressione
          della malattia, ridimensionando la letalità di covid-19. A
          nostro avviso la gestione integrata tra ospedale e territorio
          potrebbe essere un approccio vincente.
        "),

        p("
          *Hanno contribuito all’Interpretazione dei dati Carmen Fava,
          Giuseppe Saglio (Dipartimento di Scienze Cliniche e
          Biologiche, Università di Torino) e Andrea Ricotti
          (Dipartimento di Scienze della Sanità Pubblica e Pediatriche,
          Università di Torino
        ")
      )
    ),

    fluidRow(
      ## qui i nomi dei plot vanno inclusi dentro `ns(<id>)`,
      ## lato server invece saranno messi solo come `output$<id>`
      box(width = 12, plotlyOutput(ns("fig1")),
          footer = "Figure 1. Andamento del totale casi con la sovrapposizione del numero di tamponi giornaliero (linea tratteggiata) in Piemonte ed in Veneto."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig2")),
          footer = "Figure 2. Andamento del numero di ospedalizzati (totale ospedalizzati) in Piemonte ed in Veneto."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig3")),
          footer = "Figure 3. Andamento del numero dei ricoveri in terapia intensiva in Piemonte ed in Veneto."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig4")),
          footer = "Figure 4. Andamento del totale decessi in Piemonte ed in Veneto."
      )
    )
  )
}

#' focus_20200331 Server Function
#'
#' @importFrom plotly ggplotly config
#' @noRd
mod_focus_20200331_server <- function(id) {
  ## non interactive codes


  # data preparation ------------------------------------------------

  db <- dpc_covid19_ita_regioni %>%
    dplyr::filter(
      .data$denominazione_regione %in% c('Piemonte', 'Veneto')
    ) %>%
    dplyr::mutate(
      day  = lubridate::ymd_hms(.data$data),
      days = seq_along(.data$data)
    ) %>%
    # region_population is an internal data (see `data-raw/covid_ita.R`)
    # NOTE: replaced every `pop` w/ `residenti` accordingly
    dplyr::left_join(region_population) %>%
    dplyr::group_by(.data$denominazione_regione) %>%
    dplyr::arrange(.data$day) %>%
    dplyr::mutate(
      tamponi_day = .data$tamponi -
                    dplyr::lag(.data$tamponi, default = 0),
      casi_day = .data$totale_casi -
                 dplyr::lag(.data$totale_casi, default = 0)
    ) %>%
    # DRY: Do not Repeat yourself function `regional_poiss()` added to
    #      the internal (not exported, nor tested) functions, in
    #      `R/utils_regional_poiss.R`
    tidyr::nest() %>%
    ## all in a single nest call to save CPU time :-)
    dplyr::mutate(
      # hospitalization
      hosp_poiss = .data$data %>%
        purrr::map(regional_poiss, response = "totale_ospedalizzati"),
      hosp_cases = .data$hosp_poiss %>%
        purrr::map(stats::predict, type = 'response'),

      # ICU
      icu_poiss = .data$data %>%
        purrr::map(regional_poiss, response = "terapia_intensiva"),
      icu_cases = .data$icu_poiss %>%
        purrr::map(stats::predict, type = 'response'),

      # Death
      death_poiss = .data$data %>%
        purrr::map(regional_poiss, response = "deceduti"),
      death_cases = .data$death_poiss %>%
        purrr::map(stats::predict, type = 'response')
    ) %>%
    ## non mi pare servano, inutile lasciarli ridondanti
    dplyr::select(- dplyr::ends_with("poiss")) %>%
    tidyr::unnest(
      cols = c(.data$data, dplyr::ends_with("cases"))
    )

  global_theme <- theme_bw() +
    theme(
      legend.title = element_blank(),
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 60, vjust = 0.5),
      axis.line        = element_line(colour = "black")
    )


  #---- totale casi con tamponi ----
  gg_fig_1 <- db %>%
    ggplot(aes(
      x = .data$day,
      y = .data$totale_casi,
      col = .data$denominazione_regione
    )) +
    geom_point() + geom_smooth() +
    geom_line(
      aes(y = .data$tamponi_day),
      linetype = 'dashed'
    ) +
    labs(y = 'Totale casi') +
    global_theme



  #----- totale ospedalizzati ----
  gg_fig_2 <- db %>%
    ggplot(aes(
      x = .data$day,
      y = .data$totale_ospedalizzati,
      colour = .data$denominazione_regione
    )) +
    geom_point() +
    geom_line(aes(y = .data$hosp_cases)) +
    labs(x = 'Giorno', y = 'Totale ospedalizzati') +
    global_theme


  #-------- terapia intensiva----------
  gg_fig_3 <- db %>%
    ggplot(aes(
      x = .data$day,
      y = .data$terapia_intensiva,
      colour = .data$denominazione_regione
    ))+
    geom_point() +
    geom_line(aes(y = .data$icu_cases)) +
    labs(x = 'Giorno', y = 'Totale ricoveri in terapia intensiva') +
    global_theme


  #----  decessi ------
  gg_fig_4 <- db %>%
    ggplot(aes(
      x = .data$day,
      y = .data$deceduti,
      colour = .data$denominazione_regione
    )) +
    geom_point() +
    geom_line(aes(y = .data$death_cases)) +
    labs(x = 'Giorno', y = 'Totale decessi') +
    global_theme




    callModule(id = id, function(input, output, session) {
      ns <- session$ns
      ## interactive code

      ## The only interactive code here are the plots

      output$fig1 <- renderPlotly({
        ggplotly(gg_fig_1) %>%
          config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")
          ) %>%
          config(displaylogo = FALSE)
      })

      output$fig2 <- renderPlotly({
        ggplotly(gg_fig_2) %>%
          config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")
          ) %>%
          config(displaylogo = FALSE)
      })

      output$fig3 <- renderPlotly({
        ggplotly(gg_fig_3) %>%
          config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")
          ) %>%
          config(displaylogo = FALSE)
      })

      output$fig4 <- renderPlotly({
        ggplotly(gg_fig_4) %>%
          config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")
          ) %>%
          config(displaylogo = FALSE)
      })

    })
}

## To be copied in the UI
# mod_focus_20200331_ui("focus_20200331_ui_1")

## To be copied in the server
# callModule(mod_focus_20200331_server, "focus_20200331_ui_1")

