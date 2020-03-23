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
  tagList(
    sliderInput(ns("foo"), "Scegli il tuo parametro p",
      min = 10,
      max = 35,
      value = 5,
      step  = 1
    ),

    plotlyOutput(ns("picco"))
  )
}

#' focus_20200323_picco Server Function
#'
#' @noRd
mod_focus_20200323_picco_server <- function(id) {


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    gg_picco <- reactive({
      req(input$foo)

      mtcars %>%
        dplyr::filter(.data$mpg > input$foo) %>%
        ggplot(aes(x = mpg, y = cyl)) +
        geom_point()
    })

    output$picco <- renderPlotly({
      ggplotly(gg_picco())
    })

  })
}

## To be copied in the UI
# mod_focus_20200323_picco_ui("focus_20200323_picco_ui_1")

## To be copied in the server
# callModule(mod_focus_20200323_picco_server, "focus_20200323_picco_ui_1")

