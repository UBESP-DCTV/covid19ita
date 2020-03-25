#' focus_20200323_picco UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_focus_20200323_picco_ui <- function(id){
  ns <- NS(id)

  obs_t <- dpc_covid19_ita_andamento_nazionale[["data"]]
  obs_y <- dpc_covid19_ita_andamento_nazionale[["totale_casi"]]
  pred_val <- growthcurver::SummarizeGrowth(
    data_t = seq_along(obs_t),
    data_n = obs_y
  )$vals


  fluidPage(
    box(width = 12, title = "How to read and use the graphs",
      p('In the graphs the daily new confirmed cases (red dots), by region or for the whole country, vs the prediction of what will happen in the future if we assume logistic growth (black dots).'),
      p('It is possible to see how changing the parameters modifies the predictions. The range given for each parameter is the 99% CI of the values that best explain the data gathered until now.'),
      p('Varying the parameters for Italy (within the 99% CI) also results in a proportional change in all respective regional parameters.'),
      actionButton(ns("reset"), "Rispristino parametri iniziali")
    ),
    fluidRow(
      box(width = 4, footer = "Carrying capacity for the population: maximum number of cases that can be active for an indefinite time.",
        sliderInput(ns("k"), "k parameter",
          min = round(pred_val$k - 2.576 * pred_val$k_se),
          max = round(pred_val$k + 2.576 * pred_val$k_se),
          value = round(pred_val$k),
          step  = round(pred_val$k_se / 10)
        )
      ),
      box(width = 4, footer = "Initial cases.",
        sliderInput( ns("n0"), "N0 parameter",
          min = round(pred_val$n0 - 2.576 * pred_val$n0_se),
          max = round(pred_val$n0 + 2.576 * pred_val$n0_se),
          value = round(pred_val$n0),
          step  = round(pred_val$n0_se / 10)
        )
      ),
      box(width = 4, footer =  "Exponential growth rate.",
        sliderInput(ns("r"), "r parameter",
          min = round(pred_val$r - 2.576 * pred_val$r_se, 4),
          max = round(pred_val$r + 2.576 * pred_val$r_se, 4),
          value = round(pred_val$r, 4),
          step  = round(pred_val$r_se / 10, 4)
        )
      )
    ),

    plotlyOutput(ns("picco")),

    shiny::selectInput(ns("whichRegion"),  "Select regions",
      choices  = regions(),
      selectize = TRUE,
      selected = c("Veneto", "Lombardia", "Sicilia"),
      multiple = TRUE,
      width = "100%"
    ),

    plotlyOutput(ns("picco_reg")),

  )
}

#' focus_20200323_picco Server Function
#'
#' @noRd
eng_mod_focus_20200323_picco_server <- function(id) {

  # national setup
  #
  obs_t  <- dpc_covid19_ita_andamento_nazionale[["data"]]
  obs_y <- dpc_covid19_ita_andamento_nazionale[["totale_casi"]]
  pred_val_origin <- growthcurver::SummarizeGrowth(
    data_t = seq_along(obs_t),
    data_n = obs_y
  )$vals

  obs_db <- tibble::tibble(
    t = as.Date(obs_t),
    y = (obs_y - dplyr::lag(obs_y, default = 0))
  )

  pred_t <- c(obs_t, obs_t[[length(obs_t)]] + lubridate::days(1:28))


  # regional setup
  #
  obs_reg <- dpc_covid19_ita_regioni %>%
    dplyr::transmute(
      t = as.Date(.data$data),
      regione = .data$denominazione_regione,
      totale_casi = .data$totale_casi
    ) %>%
    dplyr::group_by(.data$regione) %>%
    dplyr::arrange(.data$t) %>%
    dplyr::mutate(
      y = (.data$totale_casi - dplyr::lag(.data$totale_casi, default = 0))
    ) %>%
    dplyr::ungroup()


  obs_reg_plate <- obs_reg %>%
    dplyr::select(-.data$y) %>%
    tidyr::pivot_wider(
      names_from = .data$regione,
      values_from = .data$totale_casi
    ) %>%
    dplyr::mutate(time = seq_along(.data$t)) %>%
    dplyr::select(-.data$t)

  pred_db_reg <- obs_reg_plate %>%
    growthcurver::SummarizeGrowthByPlate() %>%
    dplyr::rename(regione = .data$sample) %>%
    dplyr::select(.data$regione, .data$k, .data$n0, .data$r)


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$reset, {
      updateNumericInput(session, "k" , value = pred_val_origin[["k" ]])
      updateNumericInput(session, "n0", value = pred_val_origin[["n0"]])
      updateNumericInput(session, "r" , value = pred_val_origin[["r" ]])
    })



    # national plot
    #
    n0 <- reactive({
      req(input$n0)
    })

    k <- reactive({
      req(input$k)
    })

    r <- reactive({
     req(input$r)
    })

    pred_n <- reactive({
      res <- growthcurver::NAtT(
        k = k(),
        n0 = n0(),
        r = r(),
        t = seq_along(pred_t)
      )

      res - dplyr::lag(res, default = 0)
    })

    output$picco <- renderPlotly({

      gg_ita <- tibble::tibble(t = as.Date(pred_t), y = pred_n()) %>%
        ggplot(aes(x = .data$t, y = .data$y)) +
        geom_point() +
        geom_point(data = obs_db, colour = "red") +
        geom_line() +
        ylab("Number of new cases") +
        xlab("") +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
        )

      ggplotly(gg_ita)

    })



    # regional plot
    #
    pred_val_reg <- reactive({
      k_ita  <- req(input$k)
      n0_ita <- req(input$n0)
      r_ita  <- req(input$r)

      pred_db_reg %>%
       dplyr::mutate(
         k  = (.data$k  * k_ita ) / pred_val_origin[["k" ]],
         n0 = (.data$n0 * n0_ita) / pred_val_origin[["n0"]],
         r  = (.data$r  * r_ita ) / pred_val_origin[["r" ]],

         natt = purrr::pmap(
            list(.data$k, .data$n0, .data$r),
            function(k, n0, r) {
              tibble::tibble(
                t = as.Date(pred_t),
                y = growthcurver::NAtT(k, n0, r, t = seq_along(pred_t))
              )
            }
          )
        ) %>%
        tidyr::unnest(cols = .data$natt) %>%
        dplyr::group_by(.data$regione) %>%
        dplyr::arrange(.data$t) %>%
        dplyr::mutate(y = .data$y - dplyr::lag(.data$y, default = 0)) %>%
        dplyr::ungroup()
    })




    output$picco_reg <- renderPlotly({

      reg <- req(input$whichRegion)

      gg_reg <- pred_val_reg() %>%
        dplyr::filter(.data$regione %in% reg) %>%
        ggplot(aes(x = .data$t, y = .data$y, colour = .data$regione)) +
        geom_point(colour = "black") +
        geom_point(
          data = dplyr::filter(obs_reg, .data$regione %in% reg)
        ) +
        geom_line() +
        facet_wrap(~.data$regione, scales = "free_y") +
        ylab("Number of new cases") +
        xlab("") +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        theme(
          axis.text.x = element_text(
            angle = 60,
            hjust = 1,
            vjust = 0.5
          ),
          panel.spacing.y = unit(2, "lines"),
          legend.position = "none"
        )

      ggplotly(gg_reg)

    })

  })
}

## To be copied in the UI
# mod_focus_20200323_picco_ui("focus_20200323_picco_ui_1")

## To be copied in the server
# callModule(mod_focus_20200323_picco_server, "focus_20200323_picco_ui_1")

