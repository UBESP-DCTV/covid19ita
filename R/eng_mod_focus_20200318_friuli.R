#' focus_20200318_friuli UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_focus_20200318_friuli_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      box(width = 12,
          p("This works aimes at giving a first impression of the
           possible effect of the health policies implemented by the Friuli
           Venezia Giulia region in order to contain the spread of COVID-19."
            ),
            p(
              "In order to understand whether the containing measures helped
          slow down the spread of COVID-19, a predictive model based on the
          data collected until the 10th of March was compared to what was
          actually observed."
          ),
          p(
            "Figure 1 shows that there was a slowdown after the 10th of
          March: this day represents an epidemic change-point."),
          p(HTML(
            "Thanks to the comparison between the predicted and actual
          values it was possible to estimate some quantities:</br>
          <ol>
            <li>1.	The number of avoided cases in the Veneto region as of the 14th of March: 118 (95% C.I. 102 – 135) (Figure 2)</li>
            <li>2.	How much the epidemic is slowing down compared to what was expected:
              <ul>
                <li>a.	1.61 (95% C.I. 0.57 - 2.66) days were “gained” as of the 14th of March (Figure 3)</li>
                <li>b.	the epidemic velocity registered a drop equal to 44 cases/day (95% C.I. 40 – 47) (Figure 4)</li>
              </ul>
            </li>
          </ol>"
        ))
      )
    ),

    fluidRow(
      box(width = 12, plotlyOutput(ns("fig1")),
          title = "Figure 1. Estimated cases (bold green curve) based on course of the epidemic as registered until the 10th of March. Actual values (red dots) observed in the following day."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig2")),
          title = "Figure 2. Avoided cases in the Friuli Venezia Giulia region compared to what was expected from the data gathered until the 10th of March. The grey area indicates the 95% confidence interval."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig3")),
          title = "Figure 3. Gained days, estimated by looking at the shift to the right of the curve (predicted vs observed). The grey area indicates the 95% confidence interval."
              ),
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig4")),
          title = "Figure 4. Slowdown of the epidemic velocity (predicted vs observed). The grey area indicates the 95% confidence interval."
      )
    ),


    fluidRow(
      box(width = 12, title = "Technical details regarding the estimation of the model",
          p("
          The estimation of the model was based on the number series of the cases that
          were observed until the 10th of March. This day represents a
          change-point in terms of growth of the epidemics. This change in the
          number series was detected by a Bayesian Changepoint
          Detection Method (1). The polynomial regression model is based on a
          local approximation of the regression function (smoothing parameter
          equal to 1.5). The shape of the curve fits the quadratic trend
          of the early stage of the outbreak.
        "),
          p("
          Recent studies showed that the curve of cases could be of a quadratic
          nature rather than exponential, especially in the early stage of the outbreak
          (2).
        ")
      )
    ),
    fluidRow(
      box(width = 12, title = "References",
          p(HTML("
          <ol>
            <li>Barry D, Hartigan JA. A Bayesian Analysis for Change Point Problems. J Am Stat Assoc. 1993;88(421):309–19.</li>
            <li>Brandenburg A. Quadratic growth during the 2019 novel coronavirus epidemic. 2020.</li>
          </ol>
        "))
      )
    )
  )
}

#' focus_20200318_friuli Server Function
#'
#' @noRd
eng_mod_focus_20200318_friuli_server <- function(id, region = "Friuli Venezia Giulia"){

  # data_plot <- mtcars[1:2]

  regione <- dpc_covid19_ita_regioni %>%
    dplyr::filter(
      .data$denominazione_regione == region,
      (.data$data <= lubridate::ymd('2020-03-14'))
    ) %>%
    dplyr::mutate(
      day = lubridate::ymd_hms(.data$data),
      time_point = ifelse(
        (.data$day >= lubridate::ymd('2020-03-06')) &
        (.data$day <= lubridate::ymd('2020-03-10')),
        yes = 0,
        no  = ifelse(.data$day > lubridate::ymd('2020-03-10'),
          yes = 1,
          no  = 2
        )
      )
    )

  n_seq_regione <- seq_len(nrow(regione))

  train <- regione %>%
    dplyr::filter(.data$time_point == 0) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())

  train_2 <- regione %>%
    dplyr::filter(.data$day <= lubridate::ymd('2020-03-06')) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())

  prediction <- regione %>%
    dplyr::filter(
      (.data$time_point != 2) & (.data$day <= lubridate::ymd('2020-03-14'))
    ) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())


  prediction_2 <- regione %>%
    dplyr::filter(.data$day <= lubridate::ymd('2020-03-07')) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())


  db_true <- tibble::tibble(
    day         = regione[["day"]],
    totale_casi = regione[["totale_casi"]],
    lower       = NA_real_,
    upper       = NA_real_,
    series      = 'Osservato'
  )

  #------------- fit loess ------------------

  fit_loess <- stats::loess(totale_casi ~ days,
    data = train,
    span = 2,
    control = stats::loess.control(surface = "direct")
  )

  y_loess <- stats::predict(fit_loess, prediction[["days"]], se = TRUE)
  y_fit <- y_loess[["fit"]]

  db_loess <- tibble::tibble(
    day         = prediction[["day"]],
    totale_casi = y_fit,
    series      = 'Predetto'
  )

  fit2_loess <- stats::loess(totale_casi ~ days,
    data = train_2,
    span = 1.5,
    control = stats::loess.control(surface = "direct"))

  y2_loess <- stats::predict(fit2_loess, prediction_2[["days"]],
    se = TRUE
  )
  y2_fit <- y2_loess[["fit"]]

  db2_loess <- tibble::tibble(
    day         = prediction_2[["day"]],
    totale_casi = y2_fit,
    series = 'Predetto'
  )

  global_theme <- theme_bw() +
    theme(
      legend.title = element_blank(),
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 60, vjust = 0.5),
      axis.line        = element_line(colour = "black")
    )



  ## FIG 1

  gg_fig_1 <- db_loess %>%
    ggplot(aes(x = .data$day, y = .data$totale_casi, colour = .data$series)) +
    geom_smooth() + geom_point(data = db_true) +
    geom_smooth(data = db2_loess) +
    labs(title = "", x = "Day", y = "Cases") +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
    scale_y_continuous(breaks = seq(0, 2000, 200)) +
    global_theme




  ## FIG 2: difference by cases

  db3_full_loess <- dplyr::bind_rows(db2_loess, db_loess)

  dt_fig_2 <- db3_full_loess %>%
    dplyr::left_join(db_true,
       by = "day",
       suffix = c("_pred", "_true")
  ) %>%
    dplyr::mutate(
      difference = .data$totale_casi_pred - .data$totale_casi_true
    )


  gg_fig_2 <- dt_fig_2 %>%
    ggplot(aes(x = .data$day, .data$difference)) +
    stat_smooth() + geom_point() +
    labs(
      title = "",
      x = "Day",
      y = "Difference of cases"
    ) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
    scale_y_continuous(breaks = seq(0, 400, 50)) +
    global_theme

  ci_txt_f2 <- extract_ci_from_gg_txt(gg_fig_2)

  gg_fig_2 <- gg_fig_2 +
    geom_text(
      x = gg_fig_2$data$day[7],
      y = ci_txt_f2[["est"]],
      label = ci_txt_f2[["label"]]
    )



  ## FIG 3: difference by days

  seq_len_m <- seq_len(which.max(dt_fig_2[["totale_casi_pred"]]))

  evaluate_inverse_1 <- stats::approxfun(
    n_seq_regione[seq_len_m] ~ dt_fig_2[["totale_casi_pred"]][seq_len_m]
  )

  dt_fig_3 <- dt_fig_2 %>%
    dplyr::mutate(
      day_pred = .data$totale_casi_true %>%
        purrr::map_dbl(evaluate_inverse_1),
      daygain  = dplyr::row_number() - .data$day_pred
    )

  gg_fig_3 <- dt_fig_3 %>%
    ggplot(aes(x = .data$day, y = .data$daygain)) +
    stat_smooth() + geom_point() +
    labs(
      title = "",
      x = "Day",
      y = "Number of gained days"
    ) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
    scale_y_continuous(breaks = seq(0, 3, 1)) +
    global_theme



  ci_txt_f3 <- extract_ci_from_gg_txt(gg_fig_3)

  gg_fig_3 <- gg_fig_3 +
    geom_text(
      x = gg_fig_3$data$day[7],
      y = ci_txt_f3[["est"]],
      label = ci_txt_f3[["label"]]
    )



  # Fig 4: derivative at each time
  #
  # note:  diff(n_seq_regione) here are all ones! hence we exlude it
  #        from  diff(y_fit) / diff(n_seq_regione)

  dt_fig_4 <- dt_fig_3 %>%
    dplyr::mutate(days = dplyr::row_number())

  fit_full <- stats::loess(totale_casi_true ~ days,
    data    = dt_fig_4,
    control = stats::loess.control(surface = "direct")
  )
  y_pred_full <- stats::predict(fit_full, dt_fig_4[["days"]])

  dt_fig_4 <- dt_fig_4 %>%
    dplyr::mutate(
      dY = c(NA_real_,
             diff(dt_fig_4$totale_casi_pred) - diff(y_pred_full)
      )
    )


  gg_fig_4 <- dt_fig_4 %>%
    ggplot(aes(x = .data$day, y = .data$dY)) +
    stat_smooth() + geom_point() +
    labs(
      title = "",
      x = "Data",
      y = "Change of rate in cases"
    ) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
    scale_y_continuous(breaks = seq(-50, 50, 10)) +
    global_theme

  ci_txt_f4 <- extract_ci_from_gg_txt(gg_fig_4)

  gg_fig_4 <- gg_fig_4 +
    geom_text(
      x = gg_fig_4$data$day[11],
      y = ci_txt_f4[["est"]],
      label = ci_txt_f4[["label"]]
    )








  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$fig1 <- renderPlotly({
      ggplotly(gg_fig_1)
    })

    output$fig2 <- renderPlotly({
      ggplotly(gg_fig_2)
    })

    output$fig3 <- renderPlotly({
      ggplotly(gg_fig_3)
    })

    output$fig4 <- renderPlotly({
      ggplotly(gg_fig_4)
    })

  })
}

## To be copied in the UI
# mod_focus_20200318_friuli_ui("focus_20200318_friuli_ui_1")

## To be copied in the server
# callModule(mod_focus_20200318_friuli_server, "focus_20200318_friuli_ui_1")

