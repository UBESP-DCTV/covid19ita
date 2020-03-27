#' focus_20200325_hosp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200325_hosp_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      box(width = 12,
        p(
          "Obiettivo è quello di avere un'aggiornamento sul
           possibile effetto delle politiche sanitarie implementate in
           Veneto a contenimento dell’epidemia COVID-19."
        ),
        p(
          "Si è confrontato l'andamento prevedibile in base ai dati al
          12 marzo con l'andamento effettivamente riscontrato in Veneto, per
          capire se parte o tutte delle azioni implementate abbiano avuto un
          effetto plausibile di rallentamento sull'evolversi dell'epidemia."
        ),
        p(
          "La Figura 1 mostra che vi è stato un rallentamento dopo il
          12 marzo, giorno in cui si è osservato un changepoint
          nell'andamento epidemico."),
        p(HTML(
          "In base a questo confronto (curva stimata al 24 marzo e dati
          osservati nei giorni seguenti) è stato possibile stimare alcune
          grandezze:</br>
          <ol>
            <li>Il numero di ospedalizzazioni che si sono evitate al 24 marzo in Veneto: 561 (95% C.I. 533 – 589) (Figura 2)</li>
            <li>Il rallentamento dell’evolversi della epidemia rispetto al previsto:
              <ul>
                <li>3.23 (95% C.I. 2.53 - 3.93 giorni “guadagnati” a parità di livelli di ospedalizzazione, complessivamente al 24 marzo (Figura 3)</li>
                <li>Rallentamento dell’epidemia al 24 marzo pari a 64.84 ospedalizzazioni/giorno (95% C.I. 61.06 – 68.63) (Figura 4)</li>
              </ul>
            </li>
          </ol>"
        ))
      )
    ),

    fluidRow(
      box(width = 12, plotlyOutput(ns("fig1")),
        title = "Figure 1. Ospedalizzazioni stimate (curva rossa in grassetto, le curve rosse non in grassetto indicano i livelli di confidenza al 95%.) in base all’andamento della epidemia al 12 marzo. Andamento osservato (punti verdi) nei giorni successivi."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig2")),
        title = "Figure 2. Numero di ospedalizzazioni evitate in Veneto rispetto all’andamento previsto al giorno 12 marzo. L’area grigia indica l’intervallo di confidenza al 95%)."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig3")),
        title = "Figure 3. Giorni di “ritardo”, stimati in base allo shift a destra della curva di crescita (stimato al 12 marzo vs. osservato). L’area grigia indica l’intervallo di confidenza al 95%)."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig4")),
        title = "Figure 4. Rallentamento dell’osservato rispetto al previsto al 12 marzo. L’area grigia indica l’intervallo di confidenza al 95%)."
      )
    ),


    fluidRow(
      box(width = 12, title = "Dati tecnici sulla stima del Modello",
        p("
          La stima del modello è stata effettuata sulla serie del numero
          di ospedalizzazioni osservate fino al 12 marzo. Tale giorno è stato
          identificato in base ad un BCPDM (Bayesian Changepoint
          Detection Method) (1). Il modello di regressione polinomiale si
          basa su un’approssimazione locale della funzione di regressione
          (smoothing pari a 0.7). L’andamento della curva stimata si
          adatta allo shape tendenzialmente quadratico dell’andamento
          epidemico nelle prime fasi della diffusione.
        "),
        p("
          Recenti studi hanno dimostrato che la curva dei contagi dai casi
          di COVID-19 potrebbe avere crescita quadratica piuttosto che di
          natura esponenziale, soprattutto nelle prime fasi del contagio
          (2).
        "),
        p("
          Si ipotizza che l'andamento del numero di ospedalizzazioni segua
          in forma quello dei contagi.
        ")
      )
    ),
    fluidRow(
      box(width = 12, title = "Bibliografia",
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

#' focus_20200325_hosp Server Function
#'
#' @noRd
mod_focus_20200325_hosp_server <- function(id, region = "Veneto") {

  # data_plot <- mtcars[1:2]

  regione <- dpc_covid19_ita_regioni %>%
    dplyr::filter(
      .data$denominazione_regione == region,
      (.data$data <= lubridate::ymd('2020-03-24'))
    ) %>%
    dplyr::mutate(
      day = lubridate::ymd_hms(.data$data),
      time_point = ifelse(.data$day <= lubridate::ymd('2020-03-13'),
        yes = 0,
        no  = 1
      )
    )

  n_seq_regione <- seq_len(nrow(regione))

  regione_0 <- regione %>%
    dplyr::filter((.data$time_point == 0)) %>%
    dplyr::arrange(.data$data) %>%
    dplyr::mutate(days = dplyr::row_number())


  #------------- fit loess ------------------

  fit_loess <- stats::loess(ricoverati_con_sintomi ~ days,
    data = regione_0,
    span = 0.7,
    control = stats::loess.control(surface = "direct")
  )

  y_loess <- stats::predict(fit_loess, n_seq_regione,
    se = TRUE
  )
  y_fit <- y_loess[["fit"]]

  db_pred_loess <- tibble::tibble(
    day         = regione[["day"]],
    ricoverati_con_sintomi = y_fit,
    lower       = y_fit -
                  stats::qt(0.975, y_loess[["df"]]) * y_loess[["se.fit"]],
    upper       = y_fit +
                  stats::qt(0.975, y_loess[["df"]]) * y_loess[["se.fit"]],
    series      = 'Previsione'
  )

  db_true_loess <- tibble::tibble(
    day         = regione[["day"]],
    ricoverati_con_sintomi = regione[["ricoverati_con_sintomi"]],
    lower       = NA_real_,
    upper       = NA_real_,
    series      = 'Osservato'
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

  gg_fig_1 <- db_pred_loess %>%
    ggplot(aes(x = .data$day, y = .data$ricoverati_con_sintomi, colour = .data$series)) +
    geom_smooth() + geom_point(data = db_true_loess) +
    geom_line(aes(x = .data$day, y = .data$lower)) +
    geom_line(aes(x = .data$day, y = .data$upper)) +
    labs(title = "", x = "Giorno", y = "Totale ospedalizzazioni") +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
    scale_y_continuous(breaks = seq(0, 2200, 200)) +
    global_theme




  ## FIG 2

  dt_fig_2 <- db_pred_loess %>%
    dplyr::left_join(db_true_loess,
      by = "day",
      suffix = c("_pred", "_true")
    ) %>%
    dplyr::mutate(
      difference = .data$ricoverati_con_sintomi_pred - .data$ricoverati_con_sintomi_true
    )


  gg_fig_2 <- dt_fig_2 %>%
    ggplot(aes(x = .data$day, .data$difference)) +
    stat_smooth() + geom_point() +
    labs(
      title = "",
      x = "Giorno",
      y = "Differenziale di ospedalizzazioni"
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



  ## FIG 3
  #
  ## difference by days

  seq_len_m <- seq_len(which.max(y_fit))

  evaluate_inverse_1 = stats::approxfun(
    n_seq_regione[seq_len_m] ~ y_fit[seq_len_m]
  )

  dt_fig_3 <- dt_fig_2 %>%
    dplyr::mutate(
      day_pred = .data$ricoverati_con_sintomi_true %>%
        purrr::map_dbl(evaluate_inverse_1),
      daygain  = dplyr::row_number() - .data$day_pred
    )


  gg_fig_3 <- dt_fig_3 %>%
    ggplot(aes(x = .data$day, y = .data$daygain)) +
    stat_smooth() + geom_point() +
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



  # compute the derivative.
  # note:  diff(n_seq_regione) here are all ones! hence we exlude it
  #        from  diff(y_fit) / diff(n_seq_regione)


  fit_full <- stats::loess(ricoverati_con_sintomi_true ~ n_seq_regione,
    data    = dt_fig_3,
    control = stats::loess.control(surface = "direct")
  )
  y_pred_full <- stats::predict(fit_full, n_seq_regione)

  dt_fig_4 <- dt_fig_3 %>%
    dplyr::mutate(
      dY = c(NA_real_,
             diff(dt_fig_3$ricoverati_con_sintomi_pred) - diff(y_pred_full)
      )
    )


  gg_fig_4 <- dt_fig_4 %>%
    ggplot(aes(x = .data$day, y = .data$dY)) +
    stat_smooth() + geom_point() +
    labs(
      title = "",
       x = "Data",
       y = "Variazione nelle nuove ospedalizzazioni"
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
# mod_focus_20200325_hosp_ui("focus_20200325_ui_1")

## To be copied in the server
# callModule(mod_focus_20200325_hosp_server, "focus_20200325_ui_1")

