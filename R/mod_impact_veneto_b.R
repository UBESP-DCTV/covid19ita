#' impact_veneto_b UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_impact_veneto_b_ui <- function(id, title, footer = NULL, width = 12){
  ns <- NS(id)

  box(title = title, footer = footer, width = width,
    plotOutput(ns("gg"))
  )

}

#' impact_veneto_b Server Function
#'
#' @import gam
#' @noRd
mod_impact_veneto_b_server <- function(id, type = c("loess", "gam")) {

  veneto <- dpc_covid19_ita_regioni %>%
    dplyr::select(
      .data$data, .data$totale_casi, .data$denominazione_regione
    ) %>%
    dplyr::filter(.data$denominazione_regione == 'Veneto') %>%
    dplyr::mutate(
      days = lubridate::ymd_hms(.data$data),
      time_point = ifelse(.data$days <= lubridate::ymd('2020-03-02'),
                          yes = 0,
                          no  = 1
      )
    )

  veneto_0 <- dplyr::filter(veneto, .data$time_point == 0)
  veneto_0[["days"]] <- seq_len(nrow(veneto_0))

  db_true <- data.frame(
    day = veneto$days,
    totale_casi = veneto$totale_casi,
    series = 'dati'
  )
  new_data <- data.frame(days = seq_len(nrow(veneto)))

  db_pred <- switch(type,
    loess = {
      fit_loess <- stats::loess(totale_casi ~ days,
        data    = veneto_0,
        control = stats::loess.control(surface = "direct")
      )
      data.frame(
        day = veneto$days,
        totale_casi = stats::predict(fit_loess, new_data),
        series = 'previsione'
      )
    },

    gam   = {
      fit_gam <- gam(totale_casi ~ s(days, 5),
        data = veneto_0
      )
      data.frame(
        day = veneto$days,
        totale_casi = predict.Gam(fit_gam, newdata = new_data),
        series = 'previsione'
      )
    }
  )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns


    output$gg <- renderPlot({

      dplyr::bind_rows(db_pred, db_true) %>%
        ggplot(aes(
          x = as.Date(.data$day),
          y = .data$totale_casi,
          col = .data$series
        )) +
        geom_smooth() + geom_point() +
        scale_x_date(date_breaks = "1 day", date_labels = "%a %b %d") +
        labs(title = "", y = "Numero casi totali", x = "Data") +
        theme(
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 0.5)
        )
    })

  })
}

## To be copied in the UI
# mod_impact_veneto_b_ui("impact_veneto_b_ui_1")

## To be copied in the server
# callModule(mod_impact_veneto_b_server, "impact_veneto_b_ui_1")

