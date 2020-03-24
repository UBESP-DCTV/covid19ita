#' focus_20200323_picco UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200323_picco_ui <- function(id){
  ns <- NS(id)

  obs_t <- dpc_covid19_ita_andamento_nazionale[["data"]]
  obs_y <- dpc_covid19_ita_andamento_nazionale[["totale_casi"]]
  pred_val <- growthcurver::SummarizeGrowth(
    data_t = seq_along(obs_t),
    data_n = obs_y
  )$vals


  tagList(
    p('Nuovi casi giornalieri positivi italiani e regionali (punti in colore) e stima previsiva ipotizzando un andamento logistico (punti in nero).'),
    p('È possibile visualizzare le variazioni di previsione in funzione dei paramentri selezionati, a partire da quelli di migliore approssimazione.'),
    p('Variando i paramentri nazionali rispetto a quelli di migliore approssimazione (escursione ammessa entro l\'intervallo di confidenza al 99%), varieranno modificati, in proporzione, i corrispondenti parametri per le stime regionali.'),
    sliderInput(ns("k"), "Scegli il tuo parametro k (capacità portante popolazione: massimo numero di casi positivi che possono essere presenti per un tempo indefinito)",
      min = round(pred_val$k - 2.576 * pred_val$k_se),
      max = round(pred_val$k + 2.576 * pred_val$k_se),
      value = round(pred_val$k),
      step  = round(pred_val$k_se / 10)
    ),
    sliderInput(ns("n0"), "Scegli il tuo parametro N0 (Casi iniziali)",
      min = round(pred_val$n0 - 2.576 * pred_val$n0_se),
      max = round(pred_val$n0 + 2.576 * pred_val$n0_se),
      value = round(pred_val$n0),
      step  = round(pred_val$n0_se / 10)
    ),
    sliderInput(ns("r"), "Scegli il tuo parametro r (tasso esponenziale di crescita)",
      min = round(pred_val$r - 2.576 * pred_val$r_se, 4),
      max = round(pred_val$r + 2.576 * pred_val$r_se, 4),
      value = round(pred_val$r, 4),
      step  = round(pred_val$r_se / 10, 4)
    ),
    actionButton(ns("reset"), "Reset"),

    plotlyOutput(ns("picco")),

    shiny::selectInput(ns("whichRegion"),  "Selezionare le regioni da visualizzare",
      choices  = regions(),
      selectize = TRUE,
      selected = c("Veneto", "Lombardia"),
      multiple = TRUE,
      width = "100%"
    ),

    plotlyOutput(ns("picco_reg")),

  )
}

#' focus_20200323_picco Server Function
#'
#' @noRd
mod_focus_20200323_picco_server <- function(id) {

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
        ggplot(aes(x = t, y = y)) +
        geom_point() +
        geom_point(data = obs_db, colour = "red") +
        ylab("Numero di nuovi casi") +
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
        facet_wrap(~.data$regione, scales = "free") +
        ylab("Numero di nuovi casi") +
        xlab("") +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
        )

      ggplotly(gg_reg)

    })

  })
}

## To be copied in the UI
# mod_focus_20200323_picco_ui("focus_20200323_picco_ui_1")

## To be copied in the server
# callModule(mod_focus_20200323_picco_server, "focus_20200323_picco_ui_1")

