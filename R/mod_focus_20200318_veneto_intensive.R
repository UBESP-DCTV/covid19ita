#' focus_20200318_veneto_intensive UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200318_veneto_intensive_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(width = 12,
          p(
            "Un paragrafo"
          ),
          p(
            "Un altro paragrafo"
          )
      )
    ),

    fluidRow(
      box(width = 6,
          sliderInput(ns("n_days"),
            label = "Permanenza media in terapia intensiva (gg)",
            min = 7,
            max = 28,
            value = 10
          )
      ),
      box(width = 12, plotlyOutput(ns("fig1")),
          title = "Figure 1. Didascalia"
      )
    ),

  )
}

#' focus_20200318_veneto_intensive Server Function
#'
#' @noRd
mod_focus_20200318_veneto_intensive_server <- function(id) {

  regione <- dpc_covid19_ita_regioni %>%
    dplyr::filter(.data$denominazione_regione == "Veneto") %>%
    dplyr::mutate(
      day = lubridate::ymd_hms(.data$data),
      time_point = ifelse(day <= lubridate::ymd('2020-03-04'),
                          yes = 0,
                          no  = 1
      )
    )

  n_seq_regione <- seq_len(nrow(regione))

  db_true <- tibble::tibble(
    day         = regione[["day"]],
    totale_casi = regione[["terapia_intensiva"]],
    lower       = NA_real_,
    upper       = NA_real_,
    series      = 'Osservato'
  )

  train <- regione %>%
    dplyr::filter((.data$time_point == 0)) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())

  fit_loess <- loess(terapia_intensiva ~ days,
                     data = train, span = 0.7,
                     control = loess.control(surface = "direct")
  )

  y_loess <- stats::predict(fit_loess, n_seq_regione, se = TRUE)
  ci_ray <- qt(0.975, y_loess[["df"]]) * y_loess[["se.fit"]]

  db_loess <- tibble::tibble(
    day         = regione[["day"]],
    totale_casi = y_loess[["fit"]],
    lower       = y_loess[["fit"]] - ci_ray,
    upper       = y_loess[["fit"]] + ci_ray,
    series      = 'Predetto'
  )


  db_full <- db_true %>%
    dplyr::left_join(db_loess,
                     by = "day",
                     suffix = c("_real", "_pred")
    )


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    data_to_use <- reactive({
      req(input$n_days)

      leading_nas <- rep(NA, input$n_days - 1)

      pred <- db_full[["totale_casi_real"]] %>%
        `-`(dplyr::lag(.))
      attesi <-  db_full[["totale_casi_pred"]] %>%
        `-`(dplyr::lag(.))


      tibble::tibble(
        day = db_full[["day"]][[1]] +
          lubridate::days(seq_len(nrow(db_full) - 1 + input$n_days)),
        n_osservati = c(pred, leading_nas),
        n_attesi    = c(attesi, leading_nas),
        cumulati_osservati = cumulate_for_days(pred, input$n_days),
        cumulati_attesi    = cumulate_for_days(attesi, input$n_days)
      ) %>%
        tidyr::pivot_longer(-day, names_to = c("mode", "type"), names_sep = "_")
    })


    output$fig1 <- renderPlotly({

      gg_fig1 <- data_to_use() %>%
          ggplot(aes(
            x = .data$day,
            y = .data$value,
            shape = .data$mode,
            colour = .data$type
          )) +
          geom_point() +
          geom_line(
            data = data_to_use() %>%
              dplyr::filter(.data$mode == "cumulati")
          ) +
          scale_y_continuous(
            name   = "Numero di posti occupati in terapia intensiva",
            limits = c(0, 300),
            breaks = seq(
              from = 0,
              to   = 300,
              by   = 25
            )
          ) +
          theme(
            legend.title     = element_blank(),
            panel.border     = element_blank(),
            axis.text.x      = element_text(angle = 60, vjust = 0.5),
            axis.line        = element_line(colour = "black")
          ) +
          xlab("Giorno") +
          scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b")

      ggplotly(gg_fig1)
    })

  })
}

## To be copied in the UI
# mod_focus_20200318_veneto_intensive_ui("focus_20200318_veneto_intensive_ui_1")

## To be copied in the server
# callModule(mod_focus_20200318_veneto_intensive_server, "focus_20200318_veneto_intensive_ui_1")

