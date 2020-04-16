#' focus_20200318_piemonte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200318_piemonte_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        p(
          "Obiettivo \u00E8  quello di avere una first-look impression sul
            possibile effetto delle politiche sanitarie implementate in
            Piemonte a contenimento dell' epidemia COVID-19."
        ),
        p(
          "Si \u00E8  confrontato l'andamento prevedibile in base ai dati al
            9 marzo con l'andamento effettivamente riscontrato in
            Piemonte, per capire se parte o tutte delle
            azioni implementate abbiano avuto un effetto plausibile di
            rallentamento sull'evolversi dell'epidemia."
        ),
        p(
          "La Figura 1 mostra che vi \u00E8  stato un rallentamento dopo il
            9 marzo, giorno in cui si \u00E8  osservato un changepoint
            nell'andamento epidemico."
        ),
        p(HTML(
          "In base a questo confronto (curva stimata al 9 marzo e
            dati osservati nei giorni seguenti) \u00E8  stato possibile
            stimare alcune grandezze:</br>
          <ol>
            <li>1.	Il numero di casi positivi che si sono evitati al 15 marzo in Piemonte: 545 (95% C.I. 461 -- 629) (Figura 2)</li>
            <li>2.	Il rallentamento dell'evolversi della epidemia rispetto al previsto:
              <ul>
                <li>a.	a.	2.43 (95% C.I. 1.59 -3.27) giorni \"guadagnati\" a parit\u00E0 di livelli di casi positivi, complessivamente al 15 marzo (Figura 3)</li>
                <li>b.	b.	Rallentamento dell'epidemia al 15 marzo pari a 95 casi/giorno (95% C.I. 84 -- 106).</li>
              </ul>
            </li>
          </ol>"
        ))
      )
    ),

    fluidRow(
      box(
        width = 12, plotlyOutput(ns("fig1")),
        title = "Figure 1. Casi stimati (curva azzurra in grassetto) in base all'andamento della epidemia al 9 marzo. Andamento osservato (punti rossi) nei giorni successivi."
      )
    ),
    fluidRow(
      box(
        width = 12, plotlyOutput(ns("fig2")),
        title = "Figure 2. Numero di casi evitati in Piemonte rispetto all'andamento previsto al giorno 9 marzo. L'area grigia indica l'intervallo di confidenza al 95%)."
      )
    ),
    fluidRow(
      box(
        width = 12, plotlyOutput(ns("fig3")),
        title = "Figure 3. Giorni di \"ritardo\", stimati in base allo shift a destra della curva di crescita (stimato al 9 marzo vs. osservato). L'area grigia indica l'intervallo di confidenza al 95%)."
      )
    ),
    fluidRow(
      box(
        width = 12, plotlyOutput(ns("fig4")),
        title = "Figure 4. Rallentamento dell'osservato rispetto al previsto al 9 marzo. L'area grigia indica l'intervallo di confidenza al 95%)."
      )
    ),


    fluidRow(
      box(
        width = 12, title = "Dati tecnici sulla stima del Modello",
        p("
          La stima del modello \u00E8  stata effettuata sulla serie del numero
          di casi osservati fino al 9 marzo. Tale giorno \u00E8  stato
          identificato in base ad un BCPDM (Bayesian Changepoint
          Detection Method) (1). Il modello di regressione polinomiale
          si basa su un'approssimazione locale della funzione di
          regressione (smoothing pari a 1.5).  L'andamento della curva
          stimata si adatta allo shape tendenzialmente quadratico
          dell'andamento epidemico nelle prime fasi della diffusione.
        "),
        p("
          Recenti studi hanno dimostrato che la curva dei contagi dai
          casi di COVID-19 potrebbe avere crescita quadratica piuttosto
          che di natura esponenziale, soprattutto nelle prime fasi del
          contagio (2).
        ")
      )
    ),
    fluidRow(
      box(
        width = 12, title = "Bibliografia",
        p(HTML("
          <ol>
            <li>Barry D, Hartigan JA. A Bayesian Analysis for Change Point Problems. J Am Stat Assoc. 1993;88(421):309--19.</li>
            <li>Brandenburg A. Quadratic growth during the 2019 novel coronavirus epidemic. 2020.</li>
          </ol>
        "))
      )
    )
  )
}

#' focus_20200318_piemonte Server Function
#'
#' @noRd
mod_focus_20200318_piemonte_server <- function(id,
                                               region = "Piemonte",
                                               min_date = lubridate::ymd("2020-03-01"),
                                               max_date = lubridate::ymd("2020-03-15"),
                                               max_train_date = lubridate::ymd("2020-03-09")) {
  regione <- dpc_covid19_ita_regioni %>%
    dplyr::filter(
      .data$denominazione_regione == region,
      (.data$data >= min_date),
      (.data$data <= max_date)
    ) %>%
    dplyr::mutate(
      day = lubridate::ymd_hms(.data$data),
      time_point = ifelse(.data$day <= max_train_date,
        yes = 0,
        no = ifelse(.data$day > max_train_date, yes = 1, no = 2)
      )
    )

  n_seq_regione <- seq_len(nrow(regione))

  db_true <- tibble::tibble(
    day = regione[["day"]],
    totale_casi = regione[["totale_casi"]],
    lower = NA_real_,
    upper = NA_real_,
    series = "Osservato"
  )

  train <- regione %>%
    dplyr::filter((.data$time_point == 0)) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())

  prediction <- regione %>%
    dplyr::filter(.data$time_point != 2) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())



  #------------- fit loess ------------------

  fit_loess <- stats::loess(totale_casi ~ days,
    data = train,
    span = 1.5,
    control = stats::loess.control(surface = "direct")
  )

  y_loess <- stats::predict(fit_loess, prediction[["days"]], se = TRUE)
  ci_ray <- stats::qt(0.975, y_loess[["df"]]) * y_loess[["se.fit"]]

  db_loess <- tibble::tibble(
    day = prediction[["day"]],
    totale_casi = y_loess[["fit"]],
    lower = y_loess[["fit"]] - ci_ray,
    upper = y_loess[["fit"]] + ci_ray,
    series = "Predetto"
  )

  global_theme <- theme_bw() +
    theme(
      legend.title = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 60, vjust = 0.5),
      axis.line = element_line(colour = "black")
    )



  ## FIG 1

  gg_fig_1 <- db_loess %>%
    ggplot(aes(x = .data$day, y = .data$totale_casi, colour = .data$series)) +
    geom_smooth() +
    geom_point(data = db_true) +
    geom_line(data = db_loess, aes(x = .data$day, y = .data$lower)) +
    geom_line(data = db_loess, aes(x = .data$day, y = .data$upper)) +
    labs(title = "", x = "Giorno", y = "Totale casi") +
    global_theme




  ## FIG 2: difference by cases


  dt_fig_2 <- db_loess %>%
    dplyr::left_join(db_true,
      by = "day",
      suffix = c("_pred", "_true")
    ) %>%
    dplyr::mutate(
      difference = .data$totale_casi_pred - .data$totale_casi_true
    )


  gg_fig_2 <- dt_fig_2 %>%
    ggplot(aes(x = .data$day, .data$difference)) +
    stat_smooth() +
    geom_point() +
    labs(
      title = "",
      x = "Giorno",
      y = "Differenziale di casi totali"
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
      daygain = dplyr::row_number() - .data$day_pred
    )

  gg_fig_3 <- dt_fig_3 %>%
    ggplot(aes(x = .data$day, y = .data$daygain)) +
    stat_smooth() +
    geom_point() +
    labs(
      title = "",
      x = "Giorno",
      y = "Numero giorni guadagnati"
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
  #        from  diff(y_loess[["fit"]]) / diff(n_seq_regione)

  dt_fig_4 <- dt_fig_3 %>%
    dplyr::mutate(days = dplyr::row_number())

  fit_full <- stats::loess(totale_casi_true ~ days,
    data = dt_fig_4,
    control = stats::loess.control(surface = "direct")
  )
  y_pred_full <- stats::predict(fit_full, dt_fig_4[["days"]])

  dt_fig_4 <- dt_fig_4 %>%
    dplyr::mutate(
      dY = c(
        NA_real_,
        diff(dt_fig_4$totale_casi_pred) - diff(y_pred_full)
      )
    )


  gg_fig_4 <- dt_fig_4 %>%
    ggplot(aes(x = .data$day, y = .data$dY)) +
    stat_smooth() +
    geom_point() +
    labs(
      title = "",
      x = "Data",
      y = "Variazione nei nuovi casi"
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
# mod_focus_20200318_piemonte_ui("focus_20200318_piemonte_ui_1")

## To be copied in the server
# callModule(mod_focus_20200318_piemonte_server, "focus_20200318_piemonte_ui_1")
