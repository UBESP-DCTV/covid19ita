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
    sliderInput(ns("n0"), "Scegli il tuo parametro N0",
      min = round(pred_val$n0 - 2.576 * pred_val$n0_se),
      max = round(pred_val$n0 + 2.576 * pred_val$n0_se),
      value = round(pred_val$n0),
      step  = round(pred_val$n0_se / 10)
    ),
    sliderInput(ns("k"), "Scegli il tuo parametro k",
      min = round(pred_val$k - 2.576 * pred_val$k_se),
      max = round(pred_val$k + 2.576 * pred_val$k_se),
      value = round(pred_val$k),
      step  = round(pred_val$k_se / 10)
    ),
    sliderInput(ns("r"), "Scegli il tuo parametro r",
      min = round(pred_val$r - 2.576 * pred_val$r_se, 4),
      max = round(pred_val$r + 2.576 * pred_val$r_se, 4),
      value = round(pred_val$r, 4),
      step  = round(pred_val$r_se / 10, 4)
    ),

    plotOutput(ns("picco"))
  )
}

#' focus_20200323_picco Server Function
#'
#' @noRd
mod_focus_20200323_picco_server <- function(id) {

  obs_t  <- dpc_covid19_ita_andamento_nazionale[["data"]]
  obs_y <- dpc_covid19_ita_andamento_nazionale[["totale_casi"]]

  obs_db <- tibble::tibble(
    t = as.Date(obs_t),
    y = (obs_y - dplyr::lag(obs_y, default = 0))
  )

  pred_t <- c(obs_t, obs_t[[length(obs_t)]] + lubridate::days(1:28))


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

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



    output$picco <- renderPlot({

      tibble::tibble(t = as.Date(pred_t), y = pred_n()) %>%
        ggplot(aes(x = t, y = y)) +
        geom_point() +
        geom_point(data = obs_db, colour = "red") +
        ylab("Numero di nuovi casi") +
        xlab("Data") +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
        )

    })

  })
}

## To be copied in the UI
# mod_focus_20200323_picco_ui("focus_20200323_picco_ui_1")

## To be copied in the server
# callModule(mod_focus_20200323_picco_server, "focus_20200323_picco_ui_1")

