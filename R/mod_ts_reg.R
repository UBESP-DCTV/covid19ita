#' ts_reg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ts_reg_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(6,
        shiny::selectInput(ns("whichRegion"),  "Regions to show",
          choices  = regions(),
          selected = c("Lombardia", "Veneto"),
          multiple = TRUE,
          width = "100%"
        )
      ),
      column(6,
        shiny::selectInput(ns("whichMeasure"), "Measures to show",
          choices  = measures("regional"),
          selected = setdiff(measures(), c("totale_attualmente_positivi", "tamponi")),
          multiple = TRUE,
          width = "100%"
        )
      )
    ),
    fluidRow(plotlyOutput(ns("ts_plot"), height = "200%"))
  )
}

#' ts_reg Server Function
#'
#' @noRd
mod_ts_reg_server <- function(id, type = c("cum", "inc"), color_var = c("measure", "region")) {

  type <- match.arg(type)
  color_var <- switch(match.arg(color_var),
    measure = "Measure",
    region  = "denominazione_regione"
  )
  facet_var <- c("Measure", "denominazione_regione")[color_var != c("Measure", "denominazione_regione")]


  color_name <- color_var %>%
    stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

  dpc_data <- dpc_covid19_ita_regioni %>%
    dplyr::mutate(data = as.Date(.data$data))



  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    which_region <- reactive({
      req(input$whichRegion)
    })
    which_measure <- reactive({
      req(input$whichMeasure)
    })

    data_to_plot <- reactive({

      data_tmp <- dplyr::filter(dpc_data, .data$denominazione_regione %in% which_region()) %>%
      tidyr::pivot_longer(which_measure(),
        names_to = "Measure",
        values_to = "N"
      ) %>%
      dplyr::mutate(
        Measure = factor(.data$Measure,
          levels = which_measure(),
          labels = which_measure() %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        )
      )

      if (type == "inc") {
        data_tmp <- data_tmp %>%
          dplyr::group_by(.data$Measure, .data$denominazione_regione) %>%
          dplyr::arrange(.data$data) %>%
          dplyr::mutate(N = .data$N - dplyr::lag(.data$N, default = 0)) %>%
          dplyr::ungroup()
      }

      data_tmp
    })

    y_lab <- reactive({
      if (type == "cum") "N" else "N (difference)"
    })


    output$ts_plot <- renderPlotly({

      gg <- data_to_plot() %>%
        ggplot(aes(x = .data$data, y = .data$N, colour = .data[[{{color_var}}]])) +
        geom_line() + geom_point() +
        facet_wrap(~.data[[{{facet_var}}]], scales = "free_y") +
        xlab("Data") +
        ylab(y_lab()) +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        scale_colour_discrete(name = color_name) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          panel.spacing.y = unit(2, "lines")
        )

      ggplotly(gg)
    })

  })
}

## To be copied in the UI
# mod_ts_reg_ui("ts_reg_ui_1")

## To be copied in the server
# callModule(mod_ts_reg_server, "ts_reg_ui_1")
























#
# mod_ts_ita_server <- function(id,
#                               level = c("nat", "reg", "prov"),
#                               type = c("cum", "inc")
# ) {
#   level <- match.arg(level)
#   type <- match.arg(type)
#
#   dpc_data <- switch(level,
#     nat  = dpc_covid19_ita_andamento_nazionale,
#     reg  = dpc_covid19_ita_regioni,
#     prov = dpc_covid19_ita_province
#   ) %>%
#     dplyr::mutate(data = as.Date(.data$data))
#
#   var_to_exclude <- c(
#     "stato", "codice_regione", "codice_provincia", "sigle_provincia",
#     "lat", "long",
#     "tamponi", "nuovi_attualmente_positivi"
#   )
#   var_of_interest <- setdiff(names(dpc_data), var_to_exclude)
#   exclude_from_pivoting <- switch(level,
#     nat  = "data",
#     reg  = c("data", "denominazione_regione"),
#     prov = c("data", "denominazione_regione")
#   )
#
#   ts_data_to_plot <- dpc_data[var_of_interest] %>%
#     tidyr::pivot_longer( -{{exclude_from_pivoting}},
#       names_to = "Measure",
#       values_to = "N"
#     ) %>%
#     dplyr::mutate(
#       Measure = factor(.data$Measure,
#         levels = var_of_interest,
#         labels = var_of_interest %>%
#           stringr::str_replace_all("_", " ") %>%
#           stringr::str_to_title()
#       )
#     )
#
#   y_lab <- "N"
#
#   denominazione <- switch (level,
#     nat  = NULL,
#     reg  = "denominazione_regione",
#     prov = "denominazione_provincia"
#   )
#
#   if (type == "inc") {
#     groups <- c("Measure", denominazione)
#
#     ts_data_to_plot <- ts_data_to_plot %>%
#       dplyr::group_by_at(groups) %>%
#       dplyr::arrange(.data$data) %>%
#       dplyr::mutate(N = .data$N - dplyr::lag(.data$N)) %>%
#       dplyr::ungroup()
#
#     y_lab <- paste(y_lab, "(differences)")
#   }
#
#
#   color_var <- switch(measure_to,
#     colour = "Measure",
#     facet  = denominazione %||% "Measure"
#   )
#
#   gg <- ts_data_to_plot %>%
#     ggplot(
#       aes(x = .data$data, y = .data$N, colour = .data[[{{color_var}}]])
#     ) +
#     geom_line() + geom_point() +
#     xlab("Data") + ylab(y_lab) +
#     scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
#     scale_colour_discrete(name = "Measure") +
#     theme(
#       axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
#     )
#
#   if (level != "nat") {
#   facet_var <- switch(measure_to,
#     colour = denominazione,
#     facet  = "Measure"
#   )
#
#     gg <- gg +
#       facet_wrap(~.data[[{{facet_var}}]], scales = "free_y")
#   }
#
#   callModule(id = id, function(input, output, session) {
#     ns <- session$ns
#     output$ts_plot <- renderPlotly(ggplotly(gg))
#   })
# }
