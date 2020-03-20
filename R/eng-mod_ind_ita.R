#' indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_ind_ita_ui <- function(id){
  ns <- NS(id)

  fluidPage(

    h2(HTML("<strong>Deaths</strong>")),
    fluidRow(
      box(plotlyOutput(ns("dsp")),
        width = 6, title = "Decesead over positive cases"
      ),
      box(plotlyOutput(ns("dso")),
        width = 6, title = "Decesead over hospitalized"
      )
    ),


    h2(HTML("<strong>Recovered</strong>")),
    box(plotlyOutput(ns("dgso")),
        width = 12, title = "Recovered over hospitalized"
    ),

    h2(HTML("<strong>Self Isolation</strong>")),
    box(plotlyOutput(ns("idso")),
        width = 12, title = "Home confined over hospitalized"
    ),


    h2(HTML("
      <strong>Growth rate (%) with respect to the previous day</strong></br>
      (100% = no growth, >100% increasing, <100% decresing)
    ")),
    fluidRow(
      box(plotlyOutput(ns("cpt")),
        width = 4, title = "Cases"
      ),
      box(plotlyOutput(ns("ddt")),
        width = 4, title = "Deaths"
      ),
      box(plotlyOutput(ns("tit")),
        width = 4, title = "ICU"
      )
    )

  )
}








#' indicators Server Function
#'
#' @import ggplot2
#' @noRd
eng_mod_ind_ita_server <- function(id) {


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
      labs(title = "", x = "Day", y = "Percentage") +
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
        ylab("% (with respect to the previous day)")
      )
    })

    output$ddt <- renderPlotly({
      ggplotly(gg_ind_plot("ddt") +
        ylab("% (with respect to the previous day)")
      )
    })

    output$tit <- renderPlotly({
      ggplotly(gg_ind_plot("tit") +
        ylab("% (with respect to the previous day)")
      )
    })

  })
}

## To be copied in the UI
# mod_indicators_ui("indicators_ui_1")

## To be copied in the server
# callModule(mod_indicators_server, "indicators_ui_1")

