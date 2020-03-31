#' ts_prv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ts_prv_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(12,
        shiny::selectInput(ns("whichProvince"),  "Selezionare le province da visualizzare",
          choices  = provinces(),
          selectize = TRUE,
          selected = c(
            "Belluno", "Padova", "Rovigo", "Treviso", "Venezia",
            "Verona", "Vicenza"
          ),
          multiple = TRUE,
          width = "100%"
        )
      )
    ),
    fluidRow(shiny::checkboxInput(ns("y_log"), "Scala logaritmica")),
    fluidRow(plotlyOutput(ns("ts_plot"), height = "200%"))
  )
}

#' ts_prv Server Function
#'
#' @noRd
mod_ts_prv_server <- function(id, type = c("cum", "inc")) {

  type <- match.arg(type)

  dpc_data <- dpc_covid19_ita_province %>%
    dplyr::mutate(data = as.Date(.data$data))


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    which_province <- reactive({
      req(input$whichProvince)
    })

    data_to_plot <- reactive({

      data_tmp <- dpc_data %>%
        dplyr::filter(.data$denominazione_provincia %in% which_province())

      if (type == "inc") {
        data_tmp <- data_tmp %>%
          dplyr::group_by(.data$denominazione_provincia) %>%
          dplyr::arrange(.data$data) %>%
          dplyr::mutate(
            totale_casi = .data$totale_casi -
                          dplyr::lag(.data$totale_casi, default = 0)
          )
      }

      data_tmp
    })

    y_lab <- reactive({
      if (type == "cum") "Casi Totali" else "Casi Totali (differenza giorno-giorno)"
    })


    output$ts_plot <- renderPlotly({

      gg <- data_to_plot() %>%
        ggplot(aes(
          x = .data$data,
          y = .data$totale_casi,
          colour = .data$denominazione_provincia
        )) +
        geom_line() + geom_point() +
        xlab("Data") +
        ylab(y_lab()) +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        scale_colour_discrete(name = "Provincia") +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
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
