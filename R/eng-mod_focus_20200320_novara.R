#' focus_20200320_novara UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_focus_20200320_novara_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, p(
        "The estimation of the models are based on the data
        collected until the 19th of March; the models are used
        to predict the number of total cases for the next three days."
      ))
    ),

    fluidRow(
      box(width = 12, plotlyOutput(ns("fig1")),
          title = "Scenario 1. The first scenario uses Local Polinomial Regression Estimation with smoothing parameter alpha equal to 0.75. The expected number of total cases for the next days are reported on the graph together with the 95% CI."
      ),
      box(width = 12, DT::DTOutput(ns("data1")))
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig2")),
          title = "Scenario 2. The second scenario is based on a Generalized Additive Model (GAM) where a natural spline was used to account for non-linearity. The expected number of total cases for the next days are reported on the graph together with the 95% CI."
      ),
      box(width = 12, DT::DTOutput(ns("data2")))
    ),
    fluidRow(
      box(width = 12, plotlyOutput(ns("fig3")),
          title = "Scenario 3. The third scenario is based on a Poisson Model (using an offset to account for the place of residency) with a natural spline to account for non-linearity. The expected number of total cases for the next days are reported on the graph together with the 95% CI."
      ),
      box(width = 12, DT::DTOutput(ns("data3")))
    )
  )
}

#' focus_20200320_novara Server Function
#'
#' @noRd
eng_mod_focus_20200320_novara_server <- function(
  id,
  loc = c("Novara", "Vercelli", "Alessandria"),
  pop = 104284 # Novara
  ) {
  loc <- match.arg(loc)
  usethis::ui_todo("loc = {loc}, pop = {pop}")

  loc_db <- dpc_covid19_ita_province  %>%
    dplyr::filter(.data$denominazione_provincia == loc) %>%
    dplyr::mutate(
      day  = lubridate::ymd_hms(.data$data),
      days = dplyr::row_number()
    )

  # tre giorni successivi all'ultimo
  new_to_predict <- tibble::tibble(
    days = seq_len(nrow(loc_db) + 3)
  )

  db_true <- tibble::tibble(
    day         = c(
      loc_db[["day"]],
      max(loc_db$day) + lubridate::days(1:3)
    ),
    totale_casi = c(loc_db[["totale_casi"]], rep(NA, 3)),
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



  ## scenario 1: loess

  db_loess <- stats::loess(totale_casi ~ days,
      data = loc_db,
      control = stats::loess.control(surface = "direct")
    ) %>%
    stats::predict(new_to_predict[["days"]], se = TRUE) %>%
    predict_to_tbl(loc_db)

  # fig 1
  gg_fig_1 <- db_loess %>%
    gg_novara(db_true) +
    global_theme



  ## scenario 2 GAM
  db_gam <- mgcv::gam(totale_casi ~ splines::ns(days, 3),
      data = loc_db
    ) %>%
    stats::predict(
      newdata = new_to_predict,
      se = TRUE,
      type = "response"
    ) %>%
    predict_to_tbl(loc_db)

  # fig 2
  gg_fig_2 <- db_gam %>%
    gg_novara(db_true) +
    global_theme


  ## scenario 3 poisson offset log population
  loc_db$pop <- pop

  db_poisson <- stats::glm(totale_casi ~ splines::ns(days, 3),
      data = loc_db,
      family = "poisson",
      offset = log(pop)
    ) %>%
    stats::predict(
      newdata = new_to_predict %>%
        dplyr::mutate(pop = pop),
      se = TRUE,
      type = "response"
    ) %>%
    predict_to_tbl(loc_db)

  # fig 3
  gg_fig_3 <- db_poisson %>%
    gg_novara(db_true) +
    global_theme






  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$data1 <- DT::renderDT({
      db_loess
    })
    output$fig1 <- renderPlotly({
      ggplotly(gg_fig_1)
    })

    output$data2 <- DT::renderDT({
      db_gam
    })
    output$fig2 <- renderPlotly({
      ggplotly(gg_fig_2)
    })

    output$data3 <- DT::renderDT({
      db_poisson
    })
    output$fig3 <- renderPlotly({
      ggplotly(gg_fig_3)
    })

  })
}

## To be copied in the UI
# mod_focus_20200320_novara_ui("focus_20200320_novara_ui_1")

## To be copied in the server
# callModule(mod_focus_20200320_novara_server, "focus_20200320_novara_ui_1")

