#' ts_reg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ts_reg_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(6,
        shiny::selectInput(ns("whichRegion"),  "Selezionare le regioni da visualizzare",
          choices  = regions(),
          selectize = TRUE,
          selected = "Veneto",
          multiple = TRUE,
          width = "100%"
        )
      ),
      column(6,
        shiny::selectInput(ns("whichMeasure"), "Selezionare le misure di interesse",
          choices  = measures("regional"),
          selectize = TRUE,
          selected = setdiff(measures("regional"), "tamponi"),
          multiple = TRUE,
          width = "100%"
        )
      )
    ),
    fluidRow(shiny::checkboxInput(ns("y_log"), "Scala logaritmica")),
    fluidRow(plotlyOutput(ns("ts_plot"), height = "200%"))
  )
}

#' ts_reg Server Function
#'
#' @noRd
mod_ts_reg_server <- function(id, type = c("cum", "inc"), color_var = c("measure", "region")) {

  type <- match.arg(type)
  color_var <- switch(match.arg(color_var),
    measure = "Measure",
    region  = "denominazione_regione"
  )
  facet_var <- c("Measure", "denominazione_regione")[color_var != c("Measure", "denominazione_regione")]


  color_name <- color_var %>%
    switch(
      Measure = "Misura",
      denominazione_regione  = "Regione"
    )


  dpc_data <- dpc_covid19_ita_regioni %>%
    dplyr::mutate(data = as.Date(.data$data))



  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    which_region <- reactive({
      req(input$whichRegion)
    })
    which_measure <- reactive({
      req(input$whichMeasure)
    })

    data_to_plot <- reactive({

      data_tmp <- dplyr::filter(dpc_data, .data$denominazione_regione %in% which_region()) %>%
      tidyr::pivot_longer(which_measure(),
        names_to = "Measure",
        values_to = "N"
      ) %>%
      dplyr::mutate(
        Measure = factor(.data$Measure,
          levels = measures("regional"),
          labels = measures("regional") %>%
            measure_to_labels(lang = "ita")
      )
      )

      if (type == "inc") {
        data_tmp <- data_tmp %>%
          dplyr::group_by(.data$Measure, .data$denominazione_regione) %>%
          dplyr::arrange(.data$data) %>%
          dplyr::mutate(N = .data$N - dplyr::lag(.data$N, default = 0)) %>%
          dplyr::ungroup()
      }

      data_tmp
    })

    y_lab <- reactive({
      if (type == "cum") "N" else "N (differenza giorno-giorno)"
    })


    output$ts_plot <- renderPlotly({

      gg <- data_to_plot() %>%
        ggplot(aes(x = .data$data, y = .data$N, colour = .data[[{{color_var}}]])) +
        geom_line() + geom_point() +
        facet_wrap(~.data[[{{facet_var}}]], scales = "free_y") +
        xlab("Data") +
        ylab(y_lab()) +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        scale_colour_discrete(name = color_name) +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
          panel.spacing.y = unit(2, "lines")
        )

      if (input$y_log) {
        gg <- gg + scale_y_continuous(
          trans = 'log2',
          breaks = scales::trans_breaks("log2", function(x) 2^x),
          labels = scales::trans_format("log2", scales::math_format(2^.data[[".x"]]))
        ) +
          ylab(paste0(y_lab()," - log2"))
      }

      ggplotly(gg) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")) %>%
        config(displaylogo = FALSE)
    })

  })
}

## To be copied in the UI
# mod_ts_reg_ui("ts_reg_ui_1")

## To be copied in the server
# mod_ts_reg_server("ts_reg_ui_1")
