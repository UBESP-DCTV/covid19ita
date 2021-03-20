#' test_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h2 plotOutput fluidPage
mod_azero_ui <- function(id) {
  ns <- NS(id)
  critica <- read_azero("critica", from = "local")
  ulss_uo <- critica[["ulss_uo"]] %>%
    as.character() %>%
    unique() %>%
    sort(na.last = TRUE)

  age_range <- range(critica$eta)

  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(width = 3,
          fluidRow(
            column(width = 12,
                   sliderInput(
                     ns("age"),
                     ("Seleziona il range di et\u00E0 da considerare"),
                     min = age_range[[1L]],
                     max = age_range[[2L]],
                     value = age_range,
                     step = 1L
                   )
            )
          ),
          fluidRow(
            column(width = 4,
                   checkboxInput(ns("by_sex"),
                                 "Stratificazione per sesso")
            ),
            column(width = 4,
                   checkboxInput(ns("facet_ulss"),
                                 "Visualizzazione per ULSS")
            ),
            column(width = 4,
                   checkboxInput(
                     ns("permanenza_cumulata"),
                     HTML("Permanenze cumulate per soggetto",
                          as.character(
                            actionLink(ns("i_perm"), "",
                                       icon("question"))
                          )
                     )
                   )
            )
          ),
          fluidRow(
            column(width = 12,
                   selectInput(
                     ns("which_ulss"),
                     HTML(
                       "Seleziona l'ULSS di interesse",
                       as.character(
                         actionLink(ns("i_ulss"), "", icon("question"))
                       )
                     ),
                     multiple = TRUE,
                     choices = ulss_uo,
                     selected = setdiff(ulss_uo, NA),
                     width = "100%"
                   )
            )
          ),
          fluidRow(
            checkboxGroupInput(
              ns("comob_or"),
              HTML(
                "Comorbidit\u00E0 \"OR\"",
                as.character(
                  actionLink(ns("i_or"), "", icon("question"))
                )
              ),
              inline = TRUE,
              choiceNames = list(
                "tumore", "diabete", "cardio", "immunitarie",
                "respiratorie", "renali", "metaboliche",
                "obesit\u00E0 (BMI 30-40)", "obesit\u00E0 (BMI 40+)",
                "altre", "non note"
              ),
              choiceValues = c(
                "tumore", "diabete",
                "mal_cardiovascolari", "deficit_immunitari",
                "mal_respiratorie", "mal_renali", "mal_metaboliche",
                "obesita_bmi_30_40", "obesita_bmi_sup40", "altra_patologia",
                "no_patologie_note"
              ),
              selected = c(
                "tumore", "diabete",
                "mal_cardiovascolari", "deficit_immunitari",
                "mal_respiratorie", "mal_renali", "mal_metaboliche",
                "obesita_bmi_30_40", "obesita_bmi_sup40", "altra_patologia",
                "no_patologie_note"
              ),
              width = "100%"
            )
          ),
          fluidRow(
            checkboxGroupInput(
              ns("comob"),
              HTML(
                "Comorbidit\u00E0 \"AND\"",
                as.character(
                  actionLink(ns("i_and"), "", icon("question"))
                )
              ),
              inline = TRUE,
              choiceNames = list(
                "tumore", "diabete", "cardio", "immunitarie",
                "respiratorie", "renali", "metaboliche",
                "obesit\u00E0 (BMI 30-40)", "obesit\u00E0 (BMI 40+)",
                "altre", "non note"
              ),
              choiceValues = c(
                "tumore", "diabete",
                "mal_cardiovascolari", "deficit_immunitari",
                "mal_respiratorie", "mal_renali", "mal_metaboliche",
                "obesita_bmi_30_40", "obesita_bmi_sup40", "altra_patologia",
                "no_patologie_note"
              ),
              width = "100%"
            )
          ),
          fluidRow(
            column(width = 12,
                   actionButton(
                     ns("button"),
                     HTML("Click per generare/aggiornare i grafici"),
                     icon = icon("refresh"),
                     class = "btn-primary btn-lg"
                   ), align = "center"
            )
          )
        ),
        mainPanel(width = 9,
          plotly::plotlyOutput(
              ns("critica_daily_plot"),
              height = "500px"
            ) %>%
            shinycssloaders::withSpinner(),
          plotly::plotlyOutput(
              ns("critica_permanenza"),
              height = "500px"
            ) %>%
            shinycssloaders::withSpinner()
        )
      )
    )
  )
}








#' test_module Server Function
#'
#' @noRd
#'
#' @import ggplot2
#' @importFrom shiny renderPlot callModule
mod_azero_server <- function(id) {

  critica <- read_azero("critica", from = "local")


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input[["i_perm"]], {
      showNotification(HTML("Se selezionato, i tempi di permanenza saranno considerati cumulati su tutti gli ingressi di ciascun paziente, e riportati in corrispondenza della data del primo accesso.<br><br>Se non selezionato, i tempi di permanenza saranno considerati per accesso indipendentemente che si riferiscano ad accessi successivi dello stesso paziente, e riportati alla data di ogni singolo accesso."), duration = 10, type = "message")
    })
    observeEvent(input[["i_ulss"]], {
      showNotification("Le province selezionate sono quelle di residenza dei pazienti (e non quelle delle terapie intensive considerate.", duration = 10, type = "message")
    })
    observeEvent(input[["i_or"]], {
      showNotification(HTML("DE-selezionare una comorbidit\u00E0 esclude i pazienti che ce l'hanno</br>
    Es:</br>
      - nessuna selezione = nessun paziente;</br>
      - due selezioni = tutti (e soli) i pazienti con ALMENO una delle due comorbidit\u00E0;</br>
      - DE-selezionare \"non note\" = esclude tutti i pazienti senza patologie croniche note"), duration = 10, type = "message")
    })

    observeEvent(input[["i_and"]], {
      showNotification(HTML("Selezionare una comorbidit\u00E0 esclude i pazienti che non ce l'hanno</br>
    Es:</br>
      - nessuna selezione = tutti i pazienti;</br>
      - due selezioni = tutti (e soli) i pazienti con entrambe le comorbidit\u00E0;</br>
      - selezionare \"non note\" = include solo pazienti senza patologie croniche note"), duration = 10, type = "message")
    })


    observeEvent(input[["button"]], ignoreNULL = FALSE, {

      by_sex <- input[["by_sex"]]
      by_ulss <- input[["facet_ulss"]]
      used_strata <- {
        c("sesso", "ulss_uo")[c(by_sex, by_ulss)]
      }

      comob <- input[["comob"]]
      comob_or <- input[["comob_or"]]
      age_range <- input[["age"]]
      which_ulss <- input[["which_ulss"]]
      permanenza_cumulata <- input[["permanenza_cumulata"]]


      db_used <- critica %>%
        dplyr::mutate(
          no_patologie_note = dplyr::if_all(
            c(
              "tumore", "diabete",
              "mal_cardiovascolari", "deficit_immunitari",
              "mal_respiratorie", "mal_renali", "mal_metaboliche",
              "obesita_bmi_30_40", "obesita_bmi_sup40",
              "altra_patologia"
            ),
            is.na
          )
        ) %>%
        dplyr::filter(
          .data[["eta"]] >= age_range[[1L]],
          .data[["eta"]] <= age_range[[2L]]
        ) %>%
        filter_or_checked(comob_or) %>%
        filter_checked(comob) %>%
        dplyr::filter(
          .data[["ulss_uo"]] %in% which_ulss
        )

      output$critica_daily_plot <- plotly::renderPlotly({
        critica_daily_df <- db_used %>%
          extract_cum_by(used_strata) %>%
          fill_missing_dates("data_primo_ingresso_ti", used_strata) %>%
          dplyr::mutate(n = tidyr::replace_na(.data[["n"]], 0L)) %>%
          add_mov_avr_k(k = 7,
                        "data_primo_ingresso_ti",
                        "n",
                        used_strata
          )


        p <- ggplot(critica_daily_df) +
          aes(
            x = .data[["data_primo_ingresso_ti"]],
            y = .data[["n"]]
          )

        if (by_sex) {
          p <- p + aes(color = .data[["sesso"]]) +
            scale_color_manual(values = c("blue", "red"))
        }

        p <- p +
          geom_point(alpha = 0.4, size = 0.5) +
          geom_line(aes(y = .data[["mov_avr"]])) +
          scale_x_datetime(date_breaks = "3 weeks", date_labels = "%b %d") +
          labs(
            title = "Andamento giornaliero totale ingressi in area critica (Punti: valori netti; linee: media mobile a 7 giorni)",
            x = "Data",
            y = "Numero posti letto occupati"
          ) +
          theme_bw() +
          theme(
            axis.text.x = element_text(angle = 60, hjust = 0, vjust = 0),
            legend.position = "bottom"
          )

        if (by_ulss) {
          p <- p +
            facet_wrap(
              ~.data[["ulss_uo"]],
              scales = "free_y",
              ncol = 3
            )
        }

        plotly::ggplotly(p)

      })

      output$critica_permanenza <- plotly::renderPlotly({

        title <- "Andamento mediane permanenza in area critica"
        by <- "per ingresso"
        subtitle <- "(Punti: valori mediani; linee: media mobile a 7 giorni delle mediane)"

        critica_permanenza_df <- db_used

        if (permanenza_cumulata) {
          critica_permanenza_df <- critica_permanenza_df %>%
            dplyr::group_by(.data[["id_paziente"]]) %>%
            dplyr::mutate(
              permanenza_uo = sum(.data[["permanenza_uo"]],
                                  na.rm = TRUE)
            ) %>%
            dplyr::filter(
              .data[["data"]] == min(.data[["data"]], na.rm = TRUE)
            ) %>%
            dplyr::ungroup()
          by <- "per paziente"
        }


        critica_permanenza_df <- critica_permanenza_df %>%
          summarize_median_by(used_strata) %>%
          fill_missing_dates("data", used_strata) %>%
          dplyr::mutate(
            valori = tidyr::replace_na(.data[["valori"]], 0L)
          ) %>%
          add_mov_avr_k(k = 7, "data", "valori", used_strata) %>%
          dplyr::filter(.data[["valori"]] > 0)

        p <- critica_permanenza_df %>%
          ggplot() +
          aes(x = .data[["data"]], y = .data[["valori"]])

        if (by_sex) {
          p <- p + aes(color = .data[["sesso"]]) +
            scale_color_manual(values = c("blue", "red"))
        }

        p <- p +
          geom_point(alpha = 0.4, size = 0.5) +
          geom_line(
            aes(y = .data[["mov_avr"]]),
            size  = 0.5
          ) +
          scale_x_datetime(date_breaks = "3 weeks", date_labels = "%b %d") +
          labs(
            title = paste(title, by, subtitle),
            x = "Data ingressi in area critica",
            y = "Giorni di permanenza in area critica"
          ) +
          theme_bw() +
          theme(
            axis.text.x = element_text(angle = 60, hjust = 0, vjust = 0),
            legend.position = "bottom"
          )

        if (by_ulss) {
          p <- p +
            facet_wrap(
              ~.data[["ulss_uo"]],
              scales = "free_y",
              ncol = 3
            )
        }

        plotly::ggplotly(p)

      })
    })
  })
}
















## To be copied in the UI
# mod_test_module_ui("test_module_ui_1")

## To be copied in the server
# callModule(mod_test_module_server, "test_module_ui_1")

