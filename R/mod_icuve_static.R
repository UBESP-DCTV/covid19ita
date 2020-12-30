#' icuve_ts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fluidRow fluidPage
#' @importFrom shinydashboard box
mod_icuve_static_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig1")),
        title = "Figure 1. Proporzione mensile di decessi in
        pazienti COVID ammessi in terapia intensiva. I punti indicano
        la stima, le linee i relativi intervalli di confidenza al 95%.
        La linea blu indica la proporzione stimata totale."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig2")),
        title = "Figure 2. Andamento stimato (linea blu,
        l'area grigia indica gli intervalli di confidenza al 95%) del
        tempo mediano (in giorni) di degenza in terapia intensiva
        dall'inizio della pandemia."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig3")),
        title = "Figure 3. Andamento stimato (linea blu,
        l'area grigia indica gli intervalli di confidenza al 95%) dell'et\u00E0
        mediana dei pazienti COVID ricoverati in terapia intensiva
        dall'inizio della pandemia."
      )
    ),
    fluidRow(
      box(
        width = 12,
        plotly::plotlyOutput(ns("fig4")),
        title = "Figure 4. Andamento del numero di comorbidit\u00E0 mediano
        dei pazienti COVID ricoverati in terapia intensiva dall'inizio
        della pandemia."
      )
    )
  )
}

#' icuve_ts Server Function
#'
#' @import ggplot2
#' @import mgcv
#' @noRd
mod_icuve_static_server <- function(id) {

  stopifnot(`package {covid19.icuve} required for this function` =
              requireNamespace("covid19.icuve"))
  icuve_static <- covid19.icuve::icuve %>%
    # Arrange db by date of ICU admission
    dplyr::arrange(.data$icu_addmission)

  # 1) Data preparation ------------------------------------------------
  # Weekly db ----------------------------------------------------------
  df_weekly <- icuve_static %>%
    # Select only relevant columns
    dplyr::select(
      .data$icu_addmission, .data$icu_discharge, .data$age,
      dplyr::starts_with("comorb")
    ) %>%
    # Compute Length of stay (LOS) in days and N comorb
    dplyr::mutate(
      los = difftime(
        .data$icu_discharge, .data$icu_addmission,  units = "days"
      )
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("comorb")), ~ as.double(.)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n_comorb = sum(
        dplyr::across(dplyr::starts_with("comorb")),
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    # Column with the week and prepare the data
    dplyr::mutate(week = lubridate::week(.data$icu_addmission)) %>%
    dplyr::group_by(.data$week) %>%
    dplyr::mutate(
      min_date = min(.data$icu_addmission, na.rm = TRUE),
      max_date = max(.data$icu_addmission, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$max_date, .data$los, .data$age, .data$n_comorb
    ) %>%
    # Aggregate to 25-02-2020 the first observations
    dplyr::mutate(
      max_date = dplyr::if_else(
        .data$max_date <= lubridate::as_date("2020-02-25"),
        lubridate::as_date("2020-02-25"),
        .data$max_date
      )
    ) %>%
    dplyr::group_by(.data$max_date) %>%
    dplyr::summarise(
      week_age = stats::median(.data$age, na.rm = TRUE),
      week_los = stats::median(.data$los, na.rm = TRUE),
      week_comorb = stats::median(.data$n_comorb, na.rm = TRUE)
    )

  # Monthly db ---------------------------------------------------------
  df_month <- icuve_static %>%
    # Arrange db by date of ICU admission
    dplyr::arrange(.data$icu_addmission) %>%
    # Get the month and aggregate them
    dplyr::mutate(
      mese = lubridate::month(
        .data$icu_addmission, label = FALSE, abbr = FALSE
      ),
      mese = factor(
        dplyr::case_when(
           .data$mese %in% c(1:2) ~ "gennaio/febbraio",
           .data$mese == 3 ~ "marzo",
           .data$mese == 4 ~ "aprile",
           .data$mese == 5 ~ "maggio",
           .data$mese %in% c(6:7) ~ "giugno/luglio",
           .data$mese == 8 ~ "agosto",
           .data$mese %in% c(9:10) ~ "settembre/ottobre",
           .data$mese == 11 ~ "novembre",
           .data$mese == 12 ~ "dicembre",
           TRUE ~ as.character(.data$mese)
        ), levels = c(
          "gennaio/febbraio", "marzo", "aprile", "maggio",
          "giugno/luglio", "agosto", "settembre/ottobre", "novembre",
          "dicembre"
        )
      ),
      # Death in ICU
      death_icu = dplyr::if_else(
        !.data$icu_is_exited & !is.na(.data$icu_discharge), 1, 0
      )
    ) %>%
    # Compute Length of stay (LOS) in days and N comorb
    dplyr::mutate(
      los = difftime(
        .data$icu_discharge, .data$icu_addmission,  units = "days"
      )
    ) %>%
    # Compute the number of deaths, admission, and the los by month
    dplyr::group_by(.data$mese) %>%
    dplyr::summarise(
      n_deaths = sum(.data$final_is_dead, na.rm = TRUE),
      n_deaths_icu = sum(.data$death_icu, na.rm = TRUE),
      n_adm = dplyr::n(),
      los = sum(.data$los, na.rm = TRUE) / 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    # Proportion of deaths
    dplyr::mutate(
      prop_death = .data$n_deaths / .data$n_adm,
      prop_lower = stats::prop.test(
        x = .data$n_deaths, n = .data$n_adm
      )$conf.int[1],
      prop_upper = stats::prop.test(
        x = .data$n_deaths, n = .data$n_adm
      )$conf.int[2]
    )

  overall_p_death <- sum(icuve_static$final_is_dead) /
    nrow(icuve_static)

  # 2) Proportion of deaths in patients admitted to ICU ----------------
  ggprop_death <- ggplot(
    data = df_month,
    mapping = aes(x = .data$mese, y = .data$prop_death)
  ) +
    geom_point(size = 1.8) +
    geom_pointrange(
      mapping = aes(ymin = .data$prop_lower, ymax = .data$prop_upper)
    ) +
    geom_hline(
      yintercept = overall_p_death, color = "royalblue", size = 1.1
    ) +
    xlab("Mese") +
    ylab("Proporzione")


  # 3) Weekly ICU lengths of stay (median) -----------------------------
  ggweek_los <- ggplot(
    data = df_weekly,
    mapping = aes(x = .data$max_date, y = .data$week_los)
  ) +
    geom_point(size = 1.8) +
    geom_smooth() +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      ),
      panel.spacing.y = unit(2, "lines")
    ) +
    ylab("Giorni di degenza") +
    xlab("")

  # 4) Weekly age ICU (median) -----------------------------------------
  ggweek_age <- ggplot(
    data = df_weekly,
    mapping = aes(x = .data$max_date, y = .data$week_age)
  ) +
    geom_point(size = 1.8) +
    geom_smooth() +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      ),
      panel.spacing.y = unit(2, "lines")
    ) +
    ylab("Et\u00E0 (anni)") +
    xlab("")

  # 5) Weekly number of comorbidities ----------------------------------
  ggweek_comorb <- ggplot(
    data = df_weekly,
    mapping = aes(x = .data$max_date, y = .data$week_comorb)
  ) +
    geom_point(size = 1.8) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
    theme(
      axis.text.x = element_text(
        angle = 60, hjust = 1, vjust = 0.5
      ),
      panel.spacing.y = unit(2, "lines")
    ) +
    ylab("Numero di comorbidit\u00E0") +
    xlab("")


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$fig1 <- plotly::renderPlotly({
      plotly::ggplotly(ggprop_death)
    })

    output$fig2 <- plotly::renderPlotly({
      plotly::ggplotly(ggweek_los)
    })

    output$fig3 <- plotly::renderPlotly({
      plotly::ggplotly(ggweek_age)
    })

    output$fig4 <- plotly::renderPlotly({
      plotly::ggplotly(ggweek_comorb)
    })

  })
}

## To be copied in the UI
# mod_icuve_ts_ui("icuve_ts_cl")

## To be copied in the server
# mod_icuve_ts_server("icuve_ts_cl")
