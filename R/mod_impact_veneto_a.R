#' impact_veneto_a UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_impact_veneto_a_ui <- function(id, title, footer = NULL, width = 12){
  ns <- NS(id)

  box(title = title, footer = footer, width = width,
    plotOutput(ns("gg"))
  )

}

#' impact_veneto_a Server Function
#'
#' @noRd
mod_impact_veneto_a_server <- function(id) {

  veneto <- dpc_covid19_ita_regioni %>%
    dplyr::filter(.data$denominazione_regione == 'Veneto') %>%
    dplyr::mutate(
      day = lubridate::ymd_hms(.data$data),
      data = as.Date(.data$data),
      time_point = ifelse(.data$day <= lubridate::ymd('2020-03-02'),
        yes = 0,
        no  = 1
      ),
      pre = .data$time_point == 0
    )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns


    output$gg <- renderPlot({
      cut_date <- lubridate::ymd('2020-03-08')

      veneto %>%
        ggplot(aes(x = .data$data, y = .data$nuovi_attualmente_positivi)) +
        geom_point() + geom_line(colour = "darkgreen") +
        geom_smooth(
          data = dplyr::filter(veneto, .data$data <= cut_date),
          colour = "blue",
          method = stats::lm,
          fullrange = TRUE,
          linetype = "dashed"
        ) +
        geom_smooth(colour = "red") +
        scale_x_date(date_breaks = "1 day", date_labels = "%a %b %d") +
        labs(title = "", y = "Numero nuovi casi positivi", x = "Data") +
        theme(
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 0.5)
        )
    })

  })

}

## To be copied in the UI
# mod_impact_veneto_a_ui("impact_veneto_a_ui_1")

## To be copied in the server
# callModule(mod_impact_veneto_a_server, "impact_veneto_a_ui_1")

