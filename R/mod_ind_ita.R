#' indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ind_ita_ui <- function(id){
  ns <- NS(id)

  fluidPage(

    h2(HTML("<strong>Decessi</strong>")),
    fluidRow(
      box(plotlyOutput(ns("dsp")),
        width = 6, title = "Deceduti su positivi"
      ),
      box(plotlyOutput(ns("dso")),
        width = 6, title = "Deceduti su ospedalizzati"
      )
    ),


    h2(HTML("<strong>Guariti</strong>")),
    box(plotlyOutput(ns("dgso")),
        width = 12, title = "Dimessi guariti su ospedalizzati"
    ),

    h2(HTML("<strong>Isolamento domiciliare</strong>")),
    box(plotlyOutput(ns("idso")),
        width = 12, title = "Isolamento domiciliare su ospedalizzati"
    ),


    h2(HTML("
      <strong>Percentuali (%) rispetto al giorno precedente</strong></br>
      (100% = nessuna variazione, >100% incremento, <100% decremento)
    ")),
    fluidRow(
      box(plotlyOutput(ns("cpt")),
        width = 4, title = "Casi positivi"
      ),
      box(plotlyOutput(ns("ddt")),
        width = 4, title = "Deceduti"
      ),
      box(plotlyOutput(ns("tit")),
        width = 4, title = "Terapia intensiva"
      )
    ),

    h2(HTML("Terapie intensive")),
    fluidRow(
      selectInput(width = "45%", ns("whichRegion"),
        label = "Selezionare le regioni da visualizzare",
        choices = regions(),
        selectize = TRUE,
        selected = c("Lombardia", "Veneto", "Emilia Romagna"),
        multiple = TRUE
      ),
      sliderInput(width = "45%", ns("lastDate"),
        label = "Selezionare l'ultima data da considerare per la stima del modello",
        value = max(dpc_covid19_ita_regioni$data),
        min = min(dpc_covid19_ita_regioni$data) + lubridate::days(6),
        max = max(dpc_covid19_ita_regioni$data),
        step = lubridate::days(1),
        animate = animationOptions(interval = 1100)
      )
    ),
    box(plotlyOutput(ns("titamponi")),
      width = 12,
      title = "Andamento (fit loess, span = 1.5, degree = 2) regionale della percentuale di popolazione che, rispettivamente, è ricoverata in terapia intensiva (asse verticale) e non è (alla data considerata) ospedalizzata nonostante sia stata soggetta a tampone (dato approssimato dal numero totale di tamponi effettuati sottratti dei pazienti ospedalizzati), fino al giorno indicato dallo slider. È possibile riprodurre l'intero andamento dinamicamente in automatico tramite il tasto play.",
      footer = "NOTE: Le curve riportano andamento cumulato giornaliero sull'asse orizzontale (quindi non potranno mai 'tornare indietro') ma netto giornaliero su quello verticale (quindi possono sia 'salire' che 'scendere'). Importante notare che le curve sono 'temporalmente lunghe' tutte lo stesso numero di giorni (ovvero, dal 24 febbraio, alla data indicata dallo slider). Correndo lungo le curve, i punti si susseguono temporalmente, e ciascuno rappresenta i dati osservati nel giorno corrispondente."
    ),
    box(DT::DTOutput(ns("dt_tamponi")),
      width = 12,
      title = "Tabella andamento regionale della percentuale di popolazione che, rispettivamente, è ricoverata in terapia intensiva (intensiva_pesati) e non è (alla data considerata) ospedalizzata nonostante sia stata soggetta a tampone (tamp_asint_pesati)."
    )


  )
}








#' indicators Server Function
#'
#' @import ggplot2
#' @noRd
mod_ind_ita_server <- function(id) {


  data_to_use <- dpc_covid19_ita_andamento_nazionale %>%
    dplyr::mutate(zona = .data$stato) %>%
    dplyr::group_by(.data$zona) %>%
    dplyr::transmute(
      data = .data$data,

      ## Ex casi
      dsp  = .data$deceduti / .data$totale_casi,
      dso  = .data$deceduti / .data$totale_ospedalizzati,
      dgso = .data$dimessi_guariti / .data$totale_ospedalizzati,

      ## Isolamento domiciliare
      idso = .data$isolamento_domiciliare / .data$totale_ospedalizzati,

      ## Incrementi proporzionali
      inc_pos = .data$totale_positivi,
      cpt = ((.data$inc_pos / dplyr::lag(.data$inc_pos)) * 100) %>% round(2),

      dec_inc = .data$deceduti - dplyr::lag(.data$deceduti),
      ddt = ((.data$dec_inc / dplyr::lag(.data$dec_inc)) * 100) %>% round(2),

      int_inc = .data$terapia_intensiva - dplyr::lag(.data$terapia_intensiva),
      tit = ((.data$int_inc / dplyr::lag(.data$int_inc)) * 100) %>% round(2),
    ) %>%
    dplyr::ungroup()

  gg_ind_plot <- function(y) {

    data_to_use %>%
      ggplot(aes(x = .data$data, y = .data[[ {{y}} ]], colour = .data$zona)) +
      geom_point() + geom_line() +
      labs(title = "", x = "Giorno", y = "Proporzione") +
      scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b") +
      theme_bw() +
      theme(
        panel.border     = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 60, hjust = 1, vjust = 0.5),
        axis.line        = element_line(colour = "black"),
        legend.position = "none"
      )

  }


  ## dati per tamponi
  dati_tamponi <- dpc_covid19_ita_regioni %>%
    dplyr::left_join(region_population) %>%
    dplyr::group_by(.data$denominazione_regione) %>%
    dplyr::mutate(
      tamponi_no_sintomi = .data$tamponi -
                           .data$ricoverati_con_sintomi -
                           .data$terapia_intensiva,
      tamp_asint_pesati = 100 * (
        .data$tamponi_no_sintomi/.data$residenti
      ),
      intensiva_pesati  = 100 * (
        .data$terapia_intensiva/.data$residenti
      )
    ) %>%
    dplyr::ungroup()


  callModule(id = id, function(input, output, session){
    ns <- session$ns

    ## Ex casi
    output$dsp <- renderPlotly({
      ggplotly(gg_ind_plot("dsp"))
    })

    output$dso <- renderPlotly({
      ggplotly(gg_ind_plot("dso"))
    })

    output$dgso <- renderPlotly({
      ggplotly(gg_ind_plot("dgso"))
    })


    ## Isolamento domiciliare
    output$idso <- renderPlotly({
      ggplotly(gg_ind_plot("idso"))
    })


    ## Incrementi proporzionali
    output$cpt <- renderPlotly({
      ggplotly(gg_ind_plot("cpt") +
        ylab("% (rispetto il giorno precedente)")
      )
    })

    output$ddt <- renderPlotly({
      ggplotly(gg_ind_plot("ddt") +
        ylab("% (rispetto il giorno precedente)")
      )
    })

    output$tit <- renderPlotly({
      ggplotly(gg_ind_plot("tit") +
        ylab("% (rispetto il giorno precedente)")
      )
    })


    ## terapia intensiva per tamponi
    data_to_use <- reactive({
      req(input$whichRegion)
      req(input$lastDate)

      dati_tamponi %>%
        dplyr::filter(
          .data$data <= input$lastDate,
          .data$denominazione_regione %in% input$whichRegion
        )
    })



    output$titamponi <- renderPlotly({

      gg_titamponi <- data_to_use() %>%
        ggplot(aes(
          x = .data$tamp_asint_pesati,
          y = .data$intensiva_pesati,
          colour = .data$denominazione_regione,
          label = .data$denominazione_regione
        )) +
        geom_point() +
        geom_smooth(method = stats::loess, span = 1.5, se = FALSE) +
        ylab("% soggetti in t. intensiva") +
        xlab("% soggetti sottoposti a tampone 'attualmente' non ospedalizzati") +
        coord_cartesian(
          xlim = c(0, max(dati_tamponi$tamp_asint_pesati) + 1L),
          ylim = c(0, max(dati_tamponi$intensiva_pesati))
        ) +
        scale_color_discrete(name = "Regione")

      ggplotly(gg_titamponi, originalData = FALSE) %>%
        plotly::add_fun(function(p) {
          p %>%
            dplyr::slice(which.min(.data$x)) %>%
            plotly::add_annotations(as.Date(min(data_to_use()[["data"]])), ax = 60)
        }) %>%
        plotly::add_fun(function(p) {
          p %>%
            dplyr::group_by(.data$label) %>%
            dplyr::slice(which.max(.data$x)) %>%
            dplyr::ungroup() %>%
            plotly::add_annotations(as.Date(max(data_to_use()[["data"]])), ax = 60)
        })
    })

    output$dt_tamponi <- DT::renderDT({
      data_to_use() %>%
        dplyr::select(
          .data$data, .data$denominazione_regione,
          .data$tamp_asint_pesati,
          .data$intensiva_pesati
        ) %>%
        dplyr::mutate(data = as.Date(.data$data)) %>%
        dplyr::mutate_if(is.numeric, round, digits = 5)
    })

  })
}

## To be copied in the UI
# mod_indicators_ui("indicators_ui_1")

## To be copied in the server
# callModule(mod_indicators_server, "indicators_ui_1")

