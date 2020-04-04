#' focus_20200404_magnani UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200404_magnani_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(box(width = 12,
      p(HTML("
        L'Istituto Nazionale di Statistica (Istat) ha messo a
        disposizione su proprio sito web
        (https://www.istat.it/it/archivio/240401) i risultati della
        rilevazione della mortalità effettuata in 1084 comuni italiani,
        con dati aggiornati al 21 marzo 2020.<sup>1</sup>
      ")),

      p(HTML("
        La mortalità complessiva costituisce un indicatore estremamente
        rilevante perché è poco suscettibile a errori o difformità di
        valutazione e tiene conto sia della mortalità direttamente
        indotta da una patologia sia della mortalità causata in modo
        indiretto, a esempio per le possibili difficoltà nell’accesso
        ai servizi ospedalieri per persone affette da altre patologie.
      ")),

      p(HTML("
        La mortalità complessiva inoltre non risente di quesiti
        diagnostici o di difficoltà nella codifica delle cause di morte
        e quindi è una utile base su cui andrà costruita la valutazione
        più dettagliata degli effetti della epidemia di COVID-19 in
        atto.
      ")),

      p(HTML("
        I dati sono forniti da Istat in diverse tabelle, accessibili e
        scaricabili dal sito istituzionale, che consentono una immediata
        lettura e che possono anche essere utilizzate per la
        preparazione di ulteriori analisi. Abbiamo utilizzato questa
        seconda opportunità per preparare alcune analisi descrittive,
        che sono presentate essenzialmente sotto forma di grafici, per
        illustrare l’andamento della mortalità complessiva per area
        geografica, sesso, classe di età e periodo.
      ")),

      p(HTML("
        Si tratta di analisi preliminari, finalizzate alla condivisione
        di informazioni in un momento di emergenza, che saranno
        migliorate ed approfondite nel prossimo periodo. In particolare
        l’obiettivo attuale è limitato alla presentazione ragionata dei
        valori assoluti e dei coefficienti di variazione percentuali.
        Saranno condotte analisi supplementari per giungere a una
        migliore modellizzazione dei trend e per arricchire gli indici
        dei relativi intervalli di confidenza.
      ")),

      p(HTML("
        Le analisi sono state condotte per rispondere ai seguenti
        quesiti:
        <ul>
          <li> Di quale entità è la variazione di mortalità osservata
               confrontando il periodo tra il 1 ed il 21 marzo 2019 con
               il periodo tra il 1 ed il 21 marzo 2020?
          <li> La variazione osservata come è distribuita in relazione
               al sesso, alla classe di età ed alla regione di
               residenza?
          <li> Estendendo la valutazione agli anni precedenti, partendo
               dal 2015, esistono variazioni tra i diversi anni e di
               quale entità, sempre considerando sesso, classe di età e
               regione di residenza?
          <li> A partire da quale settimana di rilevazione (considerando
               il periodo dal 1 gennaio 2020 al 21 marzo 2020) è
               osservabile una variazione della mortalità complessiva?
        </ul>
      ")),

      p(HTML("
        In relazione all’aggregazione dei dati nelle tabelle fornite da
        Istat e alla numerosità dei dati osservati alcune variabili
        sono state raggruppate in categorie più ampie, come indicato
        nella presentazione dei risultati delle diverse analisi.
      ")),

      p(HTML("
        Di quale entità è la variazione di mortalità osservata
        confrontando il periodo tra il 1 e il 21 marzo 2019 con il
        periodo tra il 1 e il 21 marzo 2020?. La variazione osservata
        come è distribuita in relazione al sesso, alla classe di età e
        alla regione di residenza?
      ")),

      p(HTML("
        L’indicatore della variazione percentuale tra il numero di
        morti osservate nel periodo dal 1 al 21 marzo 2019 e nel
        corrispondente periodo del 2020 è stato calcolato a partire dai
        dati aggregati per regione, sesso e classe di età. Sono state
        usate le categorizzazioni presenti nella tabella fornita
        dall’Istat
        (https://www.istat.it/it/files//2020/03/Tavola-sintetica-decessi.xlsx).
        Le classi di età considerate sono: 65-74 anni, 75-84 anni,
        85 anni e oltre, già presenti nei dati.
      ")),

      p(HTML("
        L’indice di variazione percentuale è calcolato come:
      ")),

      p(HTML("
        variazione<sub>%</sub> =
          100 * (
            morti<sub>2020</sub> –
            morti<sub>2019</sub>
          ) /
          morti<sub>2019</sub>
      ")),

      p(HTML("
        L’indice è presente nella tabella dei dati originali calcolato a
        livello comunale. In queste analisi è stato calcolato con
        aggregazione regionale, per ridurre la variabilità conseguente
        alle fluttuazioni casuali che sono particolarmente marcate nel
        caso di comuni di piccole dimensioni, che costituiscono una
        parte rilevante della base dati. Sono riportati i numeri
        assoluti di decessi, il numero di comuni inclusi nella
        rilevazione per ciascuna regione e le variazioni percentuali
        dal 2019 al 2020, nel periodo considerato.<sup>2</sup>
      ")),

      p(HTML("
        L’analisi è stata condotta anche separatamente per classe di
        età, ed i risultati sono presentati nel grafico seguente
        (figura 1):
      "))
    )),

    fluidRow(box(width = 12, Title = "Figura 1: XXXXX",
      plotlyOutput(ns("fig_1"))
    )),

    fluidRow(box(width = 12,
      p(HTML("
        Tutte le regioni presentano un aumento, di entità variabile,
        nelle due classi di età più avanzate e la maggioranza delle
        regioni presenta anche un aumento nella prima classe di età,
        da 65 a 74 anni. Nella lettura del valore di variazione
        percentuale occorre tenere conto della numerosità delle morti,
        che è molto diversa tra le regioni a causa della diversa
        numerosità del campione. In alcune regioni una variazione
        appare importante ma è causata da piccoli numeri di morti in
        più o in meno (Tabella 2).
      "))
    )),



    fluidRow(box(width = 12, title = "Notes",
      p(HTML("
        <sup>1</sup> Per maggiori informazioni sulla rilevazione si
        rimanda alla metodologia descritta da Istat.
        <br>
        <sup>2</sup> Un indice del 100% indica che la mortalità è
        raddoppiata tra i due periodi a confronto.
      "))
    ))
  )
}



















#' focus_20200404_magnani Server Function
#'
#' @noRd
mod_focus_20200404_magnani_server <- function(id) {

  # Data preparation ------------------------------------------------
  data_sex <- mort_data_reg("sex")
  data_age <- mort_data_reg("age")

  gg_fig_1 <- data_age %>%
    ggplot(aes(
      x = .data$nome_reg,
      y = .data$variation,
      fill = .data$strata
    )) +
    geom_bar(position = "dodge", stat = "identity") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1)
    ) +
    ggtitle("Mortalità totale per classe di età",
      subtitle = "Confronto 1-21 marzo 2019 vs 2020"
    ) +
    ylab("Variazione Percentuale") +
    theme(panel.background = element_blank())




# Output (reactive) objects ---------------------------------------

  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$fig_1 <- renderPlotly({
      ggplotly(gg_fig_1)
    })

  })
}

## To be copied in the UI
# mod_focus_20200404_magnani_ui("magnani_1")

## To be copied in the server
# mod_focus_20200404_magnani_server("magnani_1")

