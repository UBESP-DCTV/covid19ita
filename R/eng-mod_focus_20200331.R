#' focus_20200331 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_focus_20200331_ui <- function(id) {
  ns <- NS(id)

  # User interface: WEB side
  fluidPage(
    fluidRow(
      box(width = 12,

        p("
          Analysis of data related to number of cases, number of tests,
          number of hospitalizations, number of ICU admissions and number
          of deaths from the 28th of February to the 30th of March in the
          Piemonte Region and the Veneto Region.
        "),

        p("
          The two regions took different approaches to the use of the
          nasopharyngeal swab. The Piemonte Region followed right away
          the guidelines provided by the Italian Superior Council of Health,
          testing, during the early stage of the outbreak, only people who
          both presented clinical signs of Covid-19 infection and satisfied
          some epidemiological criteria (i.e. came in contact with someone
          who visited areas considered at risk). The Veneto Region, on its
          own initiative, decided to act differently: all people that showed
          symptoms of Covid-19 were tested, as well as people who came in
          contact with them. This different approach during the early stage
          of the epidemics (from 28th February to 8th March) drew attention
          to a higher number of cases in the Veneto Region (Figure 1).
        "),

        p("
          On the 8th on March the so-called “Red Zone” was extended to the
          whole country placing Italy on lockdown, which led to a progressive
          reduction of circulation and everyday activities.
        "),

        p("
          Although in the early stage of the outbreak (approximately one
          week) the Piemonte Region showed a lower number of cases,
          it is possible to see how Veneto gained in terms of hospitalizations.
          Looking at the curves that plot the number of hospitalizations
          in the two regions it is possible to notice a clear bifurcation,
          showing that the Piemonte region was heavily burdened with a
          significant increase in hospitalizations (Figure 2). The same
          observations can be made looking at the number of ICU admissions
          (Figure 3) and the number of deaths (Figure 4).
        "),

        p("
          The parting of the hospitalization curves of the two regions
          around the 12th of March (when the Piemonte Region started to
          grow faster than the Veneto Region, though apparently having
          less cases) could be the effect of a smaller number of tests
          performed: people that were identified were way less than the
          real number, which also led to overestimate the fatality rate
          (deaths for the disease/people with the disease).The health
          policies implemented by the Veneto Region paid off. The Region
          decided to test more, probably motivated by the first outbreak
          in Vo’ Euganeo, increasing, at least at the beginning, the
          chances of finding and isolating positive cases, hence
          protecting the most vulnerable age groups and reducing the
          spread of the virus even before the lockdown of the 8th of
          March. As the curves plotting the number of cases divided more
          and more markedly, the deaths curves also did so, with a
          temporal shift of 5/6 days.
        "),

        p(HTML("
          In the current stage of the epidemics, this probably means
          that:
          <br>
          <ol>
            <li> patients who present symptoms but do not test
                 positive are left at home and do not start
                 any therapies right away;
            <li> When the situations worsens, patients visit the ER where,
                 if they are positive, they are hospitalized and can
                 finally start a therapy;
            <li> Still, they start therapy too late to prevent ARSD and,
                 probably, the late use of tocilizumab is less likely to succeed;
            <li> with consequent increase in terms of deaths.
          </ol>
        ")),

        p("
          Our analysis suggests that in order to modify this sequence
          of events, reduce hospitalizations and most importantly optimize
          ICU admissions it is necessary to increase the number of at-home
          diagnoses, extending testing to paucisymptomatic patients. This
          would allow not only to reduce the number of infected people but
          also to start an anti-inflammatory and/or antiviral at-home therapy
          under the direct control of trained doctors. The aim would be to
          prevent or at least mitigate and slow down the disease progression,
          downsizing the fatality rate of Covid-19. It is our belief that the
          integrated management of hospital and territory could be the winning
          strategy.
        "),

        p("
          *The following people contributed to the interpretation of data:
          Carmen Fava, Giuseppe Saglio (Department of Clinical and Biological
          Sciences, Università di Torino) and Andrea Ricotti
          (Department of Public Health and Pedriatic Sciences,
          Università di Torino
        ")
      )
    ),

    fluidRow(
      ## qui i nomi dei plot vanno inclusi dentro `ns(<id>)`,
      ## lato server invece saranno messi solo come `output$<id>`
      box(width = 12, plotlyOutput(ns("fig1")),
          footer = "Figure 1. Total cases and number of daily test (dashed line) in the Piemonte Region and the Veneto Region."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig2")),
          footer = "Figure 2. Number of hospitalizations (total hospitalized) in the Piemonte Region and the Veneto Region."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig3")),
          footer = "Figure 3. Number of ICU admissions in the Piemonte Region and the Veneto Region."
      )
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig4")),
          footer = "Figure 4. Total Deaths in the Piemonte Region and Veneto Region."
      )
    )
  )
}

#' focus_20200331 Server Function
#'
#' @importFrom plotly ggplotly config
#' @noRd
eng_mod_focus_20200331_server <- function(id) {
  ## non interactive codes


  # data preparation ------------------------------------------------

  db <- dpc_covid19_ita_regioni %>%
    dplyr::filter(
      .data$denominazione_regione %in% c('Piemonte', 'Veneto')
    ) %>%
    dplyr::mutate(
      day  = lubridate::ymd_hms(.data$data),
      days = seq_along(.data$data)
    ) %>%
    # region_population is an internal data (see `data-raw/covid_ita.R`)
    # NOTE: replaced every `pop` w/ `residenti` accordingly
    dplyr::left_join(region_population) %>%
    dplyr::group_by(.data$denominazione_regione) %>%
    dplyr::arrange(.data$day) %>%
    dplyr::mutate(
      tamponi_day = .data$tamponi -
                    dplyr::lag(.data$tamponi, default = 0),
      casi_day = .data$totale_casi -
                 dplyr::lag(.data$totale_casi, default = 0)
    ) %>%
    # DRY: Do not Repeat yourself function `regional_poiss()` added to
    #      the internal (not exported, nor tested) functions, in
    #      `R/utils_regional_poiss.R`
    tidyr::nest() %>%
    ## all in a single nest call to save CPU time :-)
    dplyr::mutate(
      # hospitalization
      hosp_poiss = .data$data %>%
        purrr::map(regional_poiss, response = "totale_ospedalizzati"),
      hosp_cases = .data$hosp_poiss %>%
        purrr::map(stats::predict, type = 'response'),

      # ICU
      icu_poiss = .data$data %>%
        purrr::map(regional_poiss, response = "terapia_intensiva"),
      icu_cases = .data$icu_poiss %>%
        purrr::map(stats::predict, type = 'response'),

      # Death
      death_poiss = .data$data %>%
        purrr::map(regional_poiss, response = "deceduti"),
      death_cases = .data$death_poiss %>%
        purrr::map(stats::predict, type = 'response')
    ) %>%
    ## non mi pare servano, inutile lasciarli ridondanti
    dplyr::select(- dplyr::ends_with("poiss")) %>%
    tidyr::unnest(
      cols = c(.data$data, dplyr::ends_with("cases"))
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


  #---- totale casi con tamponi ----
  gg_fig_1 <- db %>%
    ggplot(aes(
      x = .data$day,
      y = .data$totale_casi,
      col = .data$denominazione_regione
    )) +
    geom_point() + geom_smooth() +
    geom_line(
      aes(y = .data$tamponi_day),
      linetype = 'dashed'
    ) +
    labs(y = 'Total cases') +
    global_theme



  #----- totale ospedalizzati ----
  gg_fig_2 <- db %>%
    ggplot(aes(
      x = .data$day,
      y = .data$totale_ospedalizzati,
      colour = .data$denominazione_regione
    )) +
    geom_point() +
    geom_line(aes(y = .data$hosp_cases)) +
    labs(x = 'Day', y = 'Total hospitalized') +
    global_theme


  #-------- terapia intensiva----------
  gg_fig_3 <- db %>%
    ggplot(aes(
      x = .data$day,
      y = .data$terapia_intensiva,
      colour = .data$denominazione_regione
    ))+
    geom_point() +
    geom_line(aes(y = .data$icu_cases)) +
    labs(x = 'Day', y = 'Total ICU admissions') +
    global_theme


  #----  decessi ------
  gg_fig_4 <- db %>%
    ggplot(aes(
      x = .data$day,
      y = .data$deceduti,
      colour = .data$denominazione_regione
    )) +
    geom_point() +
    geom_line(aes(y = .data$death_cases)) +
    labs(x = 'Day', y = 'Total deaths') +
    global_theme




    callModule(id = id, function(input, output, session) {
      ns <- session$ns
      ## interactive code

      ## The only interactive code here are the plots

      output$fig1 <- renderPlotly({
        ggplotly(gg_fig_1) %>%
          config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")
          ) %>%
          config(displaylogo = FALSE)
      })

      output$fig2 <- renderPlotly({
        ggplotly(gg_fig_2) %>%
          config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")
          ) %>%
          config(displaylogo = FALSE)
      })

      output$fig3 <- renderPlotly({
        ggplotly(gg_fig_3) %>%
          config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")
          ) %>%
          config(displaylogo = FALSE)
      })

      output$fig4 <- renderPlotly({
        ggplotly(gg_fig_4) %>%
          config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d")
          ) %>%
          config(displaylogo = FALSE)
      })

    })
}

## To be copied in the UI
# mod_focus_20200331_ui("focus_20200331_ui_1")

## To be copied in the server
# callModule(mod_focus_20200331_server, "focus_20200331_ui_1")

