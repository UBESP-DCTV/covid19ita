#' focus_20200415_tamponi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_focus_20200415_tamponi_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        p(HTML("
          The Veneto region and the Piemonte region followed two
          different testing policies. In order to estimate the impact
          of a more wide testing policy we compared these two regions. </br>

          In particular we tried to apply to the Piemonte region the
          model embraced by the Veneto region.
        ")),

        p(HTML("
          Figure 1 shows that, based on the number of confirmed cases,
          the two regions should register more or less the same number
          of hospitalizations: it the figure here below this is represented
          by the green and the red curve, which are almost overlapping.   </br>

          Still, if we take a look at the real data we can see that
          the Piemonte region (red dots) actually registers a much
          higher number of hospitalizations then the Veneto region.
        ")),

        width = 12
      ),

      box(
        plotlyOutput(ns("fig_1")),
        width = 12,
        footer = "Figure 1: The Veneto model applied to the Piemonte
          region not accounting for the number of swabs performed.
          In the Piemonte region (red curve) the expected number
          of hospitalizations according to the model should
          follow a trend very similar to the one shown by the
          Veneto region. In reality, the observed trend is very
          different form the expected one."
      ),

      box(
        p(HTML("
          If we include in the model the number of swabs performed
          throughout time, as shown in Figure 2, we can see that the
          Veneto model can predict well the number of hospitalizations
          in the Piemonte region, hence explaining the difference observed
          in Figure 1.
        ")),
        width = 12
      ),

      box(
        plotlyOutput(ns("fig_2")),
        width = 12,
        footer = "Figure 2: The Veneto model applied to the Piemonte
          region accounting for the number of swabs performed.
          The expected number of hospitalizations (red curve), according
          to the model that accounts for the number fo swabs performed,
          is a good approximation of the actual data."
      ),

      box(
        title = "Methodology",
        p(HTML("
          We used a Poisson model with a natural spline of degree 3
          on the days and a swabs x days interaction. We used the
          resident population as an offset.
        ")),
        width = 12
      )
    )
  )
}

#' focus_20200415_tamponi Server Function
#'
#' @noRd
eng_mod_focus_20200415_tamponi_server <- function(id) {

  global_theme <- theme_bw() +
    theme(
      legend.title = element_blank(),
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 60, vjust = 0.5),
      axis.line        = element_line(colour = "black")
    )

  setup_region <- function(region) {
    dpc_covid19_ita_regioni %>%
      dplyr::filter(
        .data$denominazione_regione == region
      ) %>%
      dplyr::mutate(
        day = lubridate::ymd_hms(.data$data),
        days = seq_along(.data$data),
        pop = dplyr::filter(
          region_population,
          .data$denominazione_regione == region
        )[["residenti"]]
      )
  }

  piemonte <- setup_region("Piemonte")
  veneto   <- setup_region("Veneto")


# fig 1 -----------------------------------------------------------


  v_poiss_1 <- stats::glm(
    totale_ospedalizzati ~ splines::ns(days, 3) +
                           totale_casi + offset(log(pop)),
    family = "poisson",
    data = veneto
  )

  p_pred_1 <- stats::predict(v_poiss_1,
                             newdata = piemonte,
                             type = 'response'
  )

  db_1 <- tibble::tibble(
    day = piemonte$day,
    predicted = p_pred_1,
    denominazione_regione = 'Piemonte'
  )


  gg_fig_1 <- veneto %>%
    ggplot(aes(
      x = .data$day,
      y = .data$totale_ospedalizzati,
      colour = .data$denominazione_regione
    ))+
    geom_point() +
    geom_smooth() +
    geom_smooth(data = db_1, aes(x = .data$day, y = .data$predicted)) +
    geom_point(
      data = piemonte,
      aes(
        x = .data$day,
        y = .data$totale_ospedalizzati,
        colour = .data$denominazione_regione
      )
    ) +
    labs(
      title = '',
      y = 'Total hospitalized',
      x = 'Day',
      colour = "Region"
    ) +
    global_theme



# fig 2 -----------------------------------------------------------

  v_poiss_2 <- stats::glm(
    totale_ospedalizzati ~ splines::ns(days, 3) +
                           totale_casi +
                           tamponi*days +
                           offset(log(pop)),
    family = "poisson",
    data = veneto
  )

  p_pred_2 <- stats::predict(v_poiss_2, newdata = piemonte, type = 'response')

  db_2 <- tibble::tibble(day = piemonte$day, predicted = p_pred_2,
                     denominazione_regione='Piemonte')



  gg_fig_2 <- veneto %>%
    ggplot(aes(
      x = .data$day,
      y = .data$totale_ospedalizzati,
      colour = .data$denominazione_regione
    )) +
    geom_point() + geom_smooth() +
    geom_smooth(data = db_2, aes(x = .data$day, y = .data$predicted)) +
    geom_point(
      data = piemonte,
      aes(
        x = .data$day,
        y = .data$totale_ospedalizzati,
        colour = .data$denominazione_regione
      )
    )+
    labs(title = '', y = 'Total hospitalized', x = 'Day', col = "Region") +
    global_theme




  callModule(id = id, function(input, output, session){
    ns <- session$ns

    output$fig_1 <- renderPlotly({
      clean_ggplotly(gg_fig_1)
    })

    output$fig_2 <- renderPlotly({
      clean_ggplotly(gg_fig_2)
    })

  })
}

## To be copied in the UI
# mod_focus_20200415_tamponi_ui("focus_20200415_tamponi_ui_1")

## To be copied in the server
# callModule(mod_focus_20200415_tamponi_server, "focus_20200415_tamponi_ui_1")

