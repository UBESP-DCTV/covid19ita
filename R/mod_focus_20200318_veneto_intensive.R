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
          title = "Figure 1. Numero di ricoveri in terapia intensiva
          osservati e predetti al 23 marzo (fit loess 0.7 span,
          2 degree). La curva in rosso si riferisce all'andamento
          previsto in assenza di contenimento al 04 marzo, quella verde
          all'andamento effettivamente osservato fino al 18 marzo e
          proiettato in avanti di 5 giorni (fino al 23 marzo).
          Di default, la stima ipotizza un numero di giorni di
          permanenza medio pari a 10 giorni. Lo slider permette di
          osservare le previsioni simulando tempi diversi di degenza
          media in terapia intensiva (da 7 giorni minimo a 28 giorni
          massimo)"
      )
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
      time_point = ifelse(.data$day <= lubridate::ymd('2020-03-04'),
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

  train_next <- regione %>%
    dplyr::filter((.data$time_point != 0)) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())

  fit_loess <-stats::loess(terapia_intensiva ~ days,
                     data = train, span = 0.7,
                     control =stats::loess.control(surface = "direct")
  )

  fit_loess_next <-stats::loess(terapia_intensiva ~ days,
      data = train_next, span = 0.7,
      control =stats::loess.control(surface = "direct")
  )

  y_loess <- stats::predict(fit_loess, n_seq_regione, se = TRUE)
  ci_ray <- stats::qt(0.975, y_loess[["df"]]) * y_loess[["se.fit"]]

  db_loess <- tibble::tibble(
    day         = regione[["day"]],
    totale_casi = y_loess[["fit"]],
    lower       = y_loess[["fit"]] - ci_ray,
    upper       = y_loess[["fit"]] + ci_ray,
    series      = 'Predetto'
  )

  y_loess_next <- stats::predict(fit_loess_next,
    (max(train_next$days) + 1):(max(train_next$days) + 5),
    se = TRUE
  )
  ci_ray_next <- stats::qt(0.975, y_loess_next[["df"]]) * y_loess_next[["se.fit"]]

  db_loess_next <- tibble::tibble(
    day         = max(regione[["day"]]) + lubridate::days(1:5),
    totale_casi = y_loess_next[["fit"]],
    lower       = y_loess_next[["fit"]] - ci_ray_next,
    upper       = y_loess_next[["fit"]] + ci_ray_next,
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

      real <- c(
        db_full[["totale_casi_real"]],
        db_loess_next$totale_casi
      )
      real <- real - dplyr::lag(real)

      attesi <- c(
        db_full[["totale_casi_pred"]],
        db_loess_next$totale_casi
      )
      attesi <- attesi - dplyr::lag(attesi)
      attesi <- ifelse(attesi < 0, real, attesi)


      tibble::tibble(
        day = c(db_full[["day"]], db_loess_next$day),
        n_osservati = real,
        n_attesi    = attesi,
        cumulati_osservati = accumulate_for_days(real, input$n_days)[seq_along(.data$n_attesi)],
        cumulati_attesi    = accumulate_for_days(attesi, input$n_days)[seq_along(.data$n_attesi)]
      ) %>%
        tidyr::pivot_longer(-.data$day, names_to = c("mode", "type"), names_sep = "_")
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
        geom_line() +
        geom_vline(
          xintercept = lubridate::now() - lubridate::days(1),
          colour = "red"
        ) +
        scale_y_continuous(
            name   = "Numero di posti occupati in terapia intensiva",
            limits = c(0, 450),
            breaks = seq(
              from = 0,
              to   = 450,
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

