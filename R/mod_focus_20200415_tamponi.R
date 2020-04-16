#' focus_20200415_tamponi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200415_tamponi_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        p(HTML("
          Per valutare il potenziale impatto dell'uso dei tamponi
          abbiamo confrontato le regioni Veneto e Piemonte che hanno
          seguito modelli diversi.</br>

          In particolare, abbiamo provato ad applicare al Piemonte il
          modello Veneto.
        ")),

        p(HTML("
          La Figura 1 mostra che sulla base dei totale casi Piemonte
          e Veneto dovrebbero avere circa lo stesso numero di
          ospedalizzazioni: nella seguente figura questo \u00E8 rappresentato
          dalle due curve rossa e verde quasi sovrapposte.</br>

          Eppure se visualizziamo sul grafico anche il dato reale degli
          ospedalizzati, osserviamo che in Piemonte (circoletti rossi)
          questo \u00E8 molto pi\u00F9 elevato che per il Veneto.
        ")),
        width = 12
      ),

      box(
        plotlyOutput(ns("fig_1")),
        width = 12,
        footer = "Figura 1: Modello Veneto applicato al Piemonte senza
          tenere conto del dato relativo ai tamponi effettuati.
          In Piemonte (curva rossa) il numero di ospedalizzazioni
          attese secondo il modello dovrebbe essere simile a quello del
          Veneto (curva verde), ma si discosta molto dato osservato
          (circoletti rossi)"
      ),

      box(
        p(HTML("
          Se nel modello inseriamo anche il numero di tamponi effettuati
          nel corso del tempo, come riportato in Figura 2, osserviamo
          che il modello Veneto predice con una buona approssimazione
          le ospedalizzazioni in Piemonte, andando spiegare quindi
          la differenza osservata.
        ")),
        width = 12
      ),

      box(
        plotlyOutput(ns("fig_2")),
        width = 12,
        footer = "Figura 2: Modello Veneto applicato al Piemonte
          considerando il dato relativo ai tamponi effettuati.
          Il numero di ospedalizzazioni attese (curva rossa), tenendo
          conto del numero di tamponi, approssima bene il dato reale
          (circoletti rossi)."
      ),

      box(
        title = "Metodologia",
        p(HTML("
          Abbiamo utilizzato un modello di Poisson con una spline
          naturale di grado 3 sui giorni e una interazione tamponi x
          giorni. E' stato inserito come offset la popolazione
          residente.
        ")),
        width = 12
      )
    )
  )
}

#' focus_20200415_tamponi Server Function
#'
#' @noRd
mod_focus_20200415_tamponi_server <- function(id) {
  global_theme <- theme_bw() +
    theme(
      legend.title = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 60, vjust = 0.5),
      axis.line = element_line(colour = "black")
    )

  piemonte <- pull_region_w_pop("Piemonte")
  veneto <- pull_region_w_pop("Veneto")


  # fig 1 -----------------------------------------------------------


  v_poiss_1 <- stats::glm(
    totale_ospedalizzati ~ splines::ns(days, 3) +
      totale_casi + offset(log(pop)),
    family = "poisson",
    data = veneto
  )

  p_pred_1 <- stats::predict(v_poiss_1,
    newdata = piemonte,
    type = "response"
  )

  db_1 <- tibble::tibble(
    day = piemonte$day,
    predicted = p_pred_1,
    denominazione_regione = "Piemonte"
  )


  gg_fig_1 <- veneto %>%
    ggplot(aes(
      x = .data$day,
      y = .data$totale_ospedalizzati,
      colour = .data$denominazione_regione
    )) +
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
      title = "",
      y = "Totale ospedalizzati",
      x = "Giorno",
      colour = "Regione"
    ) +
    global_theme



  # fig 2 -----------------------------------------------------------

  v_poiss_2 <- stats::glm(
    totale_ospedalizzati ~ splines::ns(days, 3) +
      totale_casi +
      tamponi * days +
      offset(log(pop)),
    family = "poisson",
    data = veneto
  )

  p_pred_2 <- stats::predict(v_poiss_2, newdata = piemonte, type = "response")

  db_2 <- tibble::tibble(
    day = piemonte$day, predicted = p_pred_2,
    denominazione_regione = "Piemonte"
  )



  gg_fig_2 <- veneto %>%
    ggplot(aes(
      x = .data$day,
      y = .data$totale_ospedalizzati,
      colour = .data$denominazione_regione
    )) +
    geom_point() +
    geom_smooth() +
    geom_smooth(data = db_2, aes(x = .data$day, y = .data$predicted)) +
    geom_point(
      data = piemonte,
      aes(
        x = .data$day,
        y = .data$totale_ospedalizzati,
        colour = .data$denominazione_regione
      )
    ) +
    labs(title = "", y = "Totale ospedalizzati", x = "Giorno", col = "Regione") +
    global_theme




  callModule(id = id, function(input, output, session) {
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
