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
      inc_pos = .data$totale_attualmente_positivi,
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

  })
}

## To be copied in the UI
# mod_indicators_ui("indicators_ui_1")

## To be copied in the server
# callModule(mod_indicators_server, "indicators_ui_1")

