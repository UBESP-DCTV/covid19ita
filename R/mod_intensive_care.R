#' intensive_care UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intensive_care_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      shiny::selectInput(ns("whichRegion"),  "Selezionare le regioni da visualizzare",
        choices  = regions(),
        selected = "Veneto",
        multiple = TRUE,
        width = "100%"
      )
    ),

    fluidRow(plotlyOutput(ns("ic")))
  )
}

#' intensive_care Server Function
#'
#' @noRd
mod_intensive_care_server <- function(id) {

  soglie_ti <- tibble::tribble(
    ~regione, ~posti,
    "Veneto", 90L,
    "Lombardia", 100L
  )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    which_region <- reactive({
      req(input$whichRegion)
    })

    soglie <- reactive({
      soglie_ti %>%
        dplyr::filter(.data$regione %in% which_region())
    })

    output$ic <- renderPlotly({

      db_used <- dpc_covid19_ita_regioni %>%
        dplyr::filter(denominazione_regione == "Veneto") %>%
        dplyr::mutate(data = as.Date(.data$data))

      gg <- db_used %>%
        ggplot(aes(
          x = .data$data,
          y = .data$terapia_intensiva,
          colour = .data$denominazione_regione
        )) +
        geom_point() + geom_line()  +
        geom_hline(data = soglie(),
          aes(yintercept = .data$posti, colour = .data$regione),
          linetype = "dashed"
        ) +
        # geom_label(data = soglie_ti,
        #   x = sample(db_used$data, nrow(soglie_ti)),
        #   aes(
        #     y = posti,
        #     label = regione
        #   ),
        #   inherit.aes = FALSE,
        #   colour = "white"
        # ) +
        xlab("Data") + ylab("Posti letto") +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        scale_colour_discrete(name = "Regione") +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        )

      ggplotly(gg)
    })

  })
}

## To be copied in the UI
# mod_intensive_care_ui("intensive_care_ui_1")

## To be copied in the server
# callModule(mod_intensive_care_server, "intensive_care_ui_1")

