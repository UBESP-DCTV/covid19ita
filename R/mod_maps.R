#' maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_maps_ui <- function(id){
  ns <- NS(id) # non cancellare

  ## Da qui in poi inserire il contenuto lato UI del modulo, in
  ## particolare la definizione degli input (ricordarsi di inserire i
  ## relativi id all'interno di una chiamata a `ns(<input_id>)`)
  fluidPage(
    ## sfruttare i `box()` per quanto possibile, ed eventuali
    ## `fludRow()`
    fluidRow(
      box(textOutput(ns("foo")),
        title = "Testo generato dal server",
        width = 3 # integer, max 12(/12) = full width of the page body
      ),
      box(
        sliderInput(ns("n_min"),
          label = "Numero minimo di casi da considerare",
          min = 0L,
          max = max(dpc_covid19_ita_regioni[["totale_casi"]],
            na.rm = TRUE
          ),
          value = 0L,
          step = 1L
        ),
        width = 9
      )
    ),
    fluidRow(
      box(plotlyOutput(ns("baz")),
        title = "Andamento dei casi nel tempo per regione",
        footer = "Fare click sul nome di una regione per disattivarla",
        width = 12
      )
    )
  )
}

#' maps Server Function
#'
#' @noRd
mod_maps_server <- function(id) {

  ## Zona dedicata alle computazioni preliminari, non reattive

  data_to_use <- dpc_covid19_ita_regioni %>%
    dplyr::select(
      .data$data, .data$totale_casi, .data$denominazione_regione
    )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    ## zona dedicata alle computazioni reattive del modulo, in
    ## particolare la definizione degli output (ricordarsi che tali nomi
    ## NON vanno inseriti (a differenza della controparte in input)
    ## all'interno della chiamata a `ns()`)
    output$foo <- renderText("Bar.")

    current_data <- reactive({
      req(input[["n_min"]])

      data_to_use %>%
        dplyr::filter(.data$totale_casi >= input[["n_min"]])

    })

    output$baz <- renderPlotly({
      req(input[["n_min"]])

      gg <- current_data() %>%
        ggplot(aes(
          x = .data$data,
          y = .data$totale_casi,
          colour = .data$denominazione_regione
        )) +
        geom_point() +
        geom_line() +
        xlab("Data") +
        ylab(glue::glue("N casi (> {input[['n_min']]})")) +
        scale_colour_discrete(name = "Regione") +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
        )


      ggplotly(gg) %>%
        config(
          modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d"
          ),
          displaylogo = FALSE
        )
    })
  })

}

## To be copied in the UI
# mod_maps_ui("maps_1")

## To be copied in the server
# mod_maps_server("maps_1")

