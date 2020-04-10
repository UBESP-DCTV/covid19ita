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
        Come comunicato sul sito dell'Istat,
        sono stati considerati i comuni che \"presentano almeno dieci
        decessi da gennaio al 28 marzo 2020(perché meno esposti a eccessive
        variazioni  nei  dati  giornalieri)  e che hanno  fatto  registrare
        un  aumento  dei morti pari o superiore al 20 percento nei primi
        21 o 28 giorni di marzo 2020, rispetto al dato medio dello
        stesso periodo degli anni 2015-2019\"
      ")),

      p(HTML("
        In sintesi sono compresi nella rilevazione 1084 comuni che
        includono complessivamente 6.177.016 uomini e 6.496.805 donne,
        così distribuiti per regione di residenza. La rappresentazione
        è diversa nelle diverse regioni (Tabella 1).
      ")),
    )),

    fluidRow(box(width = 12,
      DT::DTOutput(ns("tab_0_residenti")),
      title = "Tabella 1: Popolazione residente al 1 gennaio 2019 nei 1084 comuni inclusi nei dati."
    )),

    fluidRow(box(width = 12,
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
      "))
    )),





    fluidRow(box(width = 12,
      h2(HTML("
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
        dati aggregati per regione, sesso e classe di età. In questo caso
        l'aggregazione per regione comprende soltanto i comuni di cui Istat
        ha messo a disposizione i dati. Sono state usate le categorizzazioni
        presenti nella tabella fornita dall’Istat
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
            numero decessi<sub>2020</sub> –
            numero decessi<sub>2019</sub>
          ) /
          numero decessi<sub>2019</sub>
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
        età, e per sesso, i risultati sono presentati nei grafici
        seguenti (figura 1 e figura 2):
      "))
    )),

    fluidRow(box(plotlyOutput(ns("fig_1_age")),
      title = "Figura 1: Variazione percentuale per classi di età e regione. Periodo 1-21 marzo 2019 vs. 1-21 marzo 2020.",
      width = 12
    )),

    fluidRow(box(width = 12,
      p(HTML("
        Esaminando i comuni selezionati dall'Istat, si può osservare che
        tutte le regioni presentano, in tali comuni, un aumento di entità variabile
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

    fluidRow(box(width = 12, Title = "Tabella 2: Variazione percentuale per classi di età e regione. Periodo 1-21 marzo 2019 vs. 1-21 marzo 2020.",
      DT::DTOutput(ns("tab_1_age"))
    )),

    fluidRow(box(plotlyOutput(ns("fig_2_sex")),
      title = "Figura 2: Variazione percentuale per classi di sesso e regione. Periodo 1-21 marzo 2019 vs. 1-21 marzo 2020.",
      footer = "f: femminile; m: maschile; mf: globale",
      width = 12,
    )),

    fluidRow(box(width = 12, Title = "Tabella 3: Variazione percentuale per classi di sesso e regione. Periodo 1-21 marzo 2019 vs. 1-21 marzo 2020.",
      DT::DTOutput(ns("tab_2_sex"))
    )),


    fluidRow(box(width = 12,
      h2(HTML("
        Estendendo la valutazione agli anni precedenti, partendo dal
        2015, esistono variazioni tra i diversi anni e di quale entità,
        sempre considerando sesso, classe di età e regione di residenza?
      ")),

      p(HTML("
        La base dati messa a disposizione consente di valutare
        l’andamento della mortalità negli anni precedenti, a partire
        dal 2015. Sono stati usati i dati presentati nella tabella
        https://www.istat.it/it/files//2020/03/dati-comunali-settimanali-ANPR-1.zip
        Le analisi saranno approfondite in futuro valutando più
        appropriatamente l’andamento della mortalità nel periodo
        2015-2020 anche con metodi di modellizzazione, comunque
        l’ispezione dei dati con metodi descrittivi consente di
        apprezzare come limitata la variabilità nel numero di decessi
        nel periodo 2015 - 2019, sia complessivamente sia nelle
        disaggregazioni per sesso e classe di età.
      ")),

      p(HTML("
        Il numero di decessi nei comuni relativi all'indagine Istat
        sono stati sommati assieme al fine di ottenere un dato regionale.
        I grafici che seguono (figura 3) presentano il numero totale di
        morti nel periodo 2015-2020 per regione. Le regioni sono state
        aggregate in due gruppi per migliorare la leggibilità dei
        grafici, corrispondenti alla classificazione Istat con le
        regioni del Nord e con le regioni del Centro-Sud-Isole.
      "))
    )),


    fluidRow(box(plotlyOutput(ns("fig_3_year_all")),
      title = "Figura 3: Numero di decessi per regione nel periodo 1-21 marzo dal 2015 al 2020.",
      width = 12,
    )),

    fluidRow(box(width = 12,
      p(HTML("
        I grafici seguenti (figura 4) presentano il confronto della
        mortalità negli anni dal 2015 al 2020, per regione e classe di
        età. Il numero di decessi nei comuni relativi all'indagine Istat
        sono stati sommati assieme al fine di ottenere un dato regionale.
        Le classi di età sono state definite sulla base dei
        raggruppamenti con cui sono presentati i dati originali,
        e precisamente: fino a 64 anni (ottenuto sommando le due classi
        presenti nella tabella originale 0-14 e 15-64), da 65 a 74, 75
        e oltre.
      ")),

      p(HTML("
        Si ricorda che i grafici presentano i numeri assoluti e quindi
        le differenze tra le regioni riflettono in primo luogo la
        dimensione dei comuni coinvolti dall'indagine Istat per regione.
      "))
    )),

    fluidRow(box(
      plotlyOutput(ns("fig_4_year_age")),
      title = "Figura 4: Numero di decessi per regione e classi di età nel periodo 1-21 marzo dal 2015 al 2020.",
      width = 12,
    )),






    fluidRow(box(width = 12,
      h2(HTML("
        In quale settimana di rilevazione (considerando il periodo dal
        1 gennaio 2020 al 21 marzo 2020) è osservabile una variazione
        della mortalità complessiva?
      ")),

      p(HTML("
        Per contribuire alla risposta a questo quesito sono stati usati
        i dati relativi ai 1084 comuni riportati nella tabella
        https://www.istat.it/it/files//2020/03/dati-comunali-settimanali-ANPR-1.zip,
        limitatamente ai dati per il periodo 1 gennaio – 21 marzo 2020.
        Nella tabella tali dati sono presentati suddivisi a ritroso in
        periodi di 7 giorni. Così facendo il primo periodo va da 1 al
        10 gennaio e quindi non è confrontabile con i successivi perché
        comprende 11 giorni di osservazione. In questa fase si è deciso
        pertanto di escluderlo e di partire dal secondo periodo, che ha
        inizio il 12 gennaio 2020. I grafici seguenti (figura 5 e figura 6)
        presentano l’andamento per classe di età, sesso e regione.
        Le classi ed i raggruppamenti regionali adottati per rendere più
        agevole l’esame dei risultati sono gli stessi adottati per le
        analisi precedenti. I grafici riportano sull’asse orizzontale
        la data di inizio dei diversi periodi settimanali.
      ")),

      p(HTML("
        Si osserva da queste analisi che vi è un aumento della mortalità
        generale a partire dalla settimana del 1 marzo in particolare
        nelle regioni maggiormente interessate dall’epidemia, ed in
        particolare in Lombardia. Si ricorda che è stato incluso in
        queste analisi il campione di comuni selezionato dall'Istat.
      "))

    )),

    fluidRow(box(
      plotlyOutput(ns("fig_5_week_sex")),
      title = "Figura 5: Numero di decessi settimanali per regione e sesso dall'12 al 21 marzo 2020.",
      width = 12,
    )),

    fluidRow(box(
      plotlyOutput(ns("fig_6_week_age")),
      title = "Figura 6: Numero di decessi settimanali per regione e classi di età dall'12 al 21 marzo 2020.",
      width = 12,
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

  ## 1-2: variazione percentuale 2019-2020 --------------------------

  ### by age (fig 1)
  gg_fig_1_age <- mort_data_reg_age %>%
    ggmort("Classe di età") +
    ggtitle("Mortalità totale per classe di età",
      subtitle = "Confronto 1-21 marzo 2019 vs 2020"
    )

  ### by age (fig 2)
  gg_fig_2_sex <- mort_data_reg_sex %>%
    ggmort("Sesso") +
    ggtitle("Mortalità totale per sesso",
      subtitle = "Confronto 1-21 marzo 2019 vs 2020"
    )




  ## 3: mortalità prime tre settimane di marzo 2015-2020 ------------

  data_year_marzo <- mort_data_comuni %>%
    dplyr::filter(
      .data$settimana %in%
        c("01/03-07/03", "08/03-14/03", "15/03-21/03")
    )


  ### all (fig 3)
  data_year_marzo_all <- data_year_marzo %>%
    dplyr::group_by(.data$regione, .data$area, .data$year) %>%
    dplyr::summarise(decessi = sum(.data$n_death))

  gg_fig_3_year_all <- data_year_marzo_all %>%
    ggplot(aes(
      x = .data$year,
      y = .data$decessi,
      colour = .data$regione
    )) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_wrap(~ .data$area, scales = "free_y") +
    labs(y = "Numero decessi 1-20 marzo") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.background = element_blank()
    )


  ### by age (fig 4)
  data_year_marzo_age <- data_year_marzo  %>%
    dplyr::group_by(
      .data$area, .data$regione, .data$year, .data$classe_di_eta
    ) %>%
    dplyr::summarise(decessi = sum(.data$n_death))

  gg_fig_4_year_age <- data_year_marzo_age %>%
    ggplot(aes(
      x = .data$year,
      y = .data$decessi,
      colour = .data$regione
    )) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(.data$area ~ .data$classe_di_eta, scales = "free_y") +
    labs(y = "Numero decessi 1-20 marzo") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.background = element_blank()
    )




  ## 4: prime settimane 2020 ----------------------------------------
  data_inizio_2020 <- mort_data_comuni %>%
    dplyr::filter(
      (.data$settimana != "01/01-11/01") & (.data$year == 2020)
    ) %>%
    dplyr::mutate(
      settimana = substr(.data$settimana, start = 1, stop = 5) %>%
        as.Date(format = "%d/%m")
    )


  data_week_sex <- data_inizio_2020 %>%
    dplyr::group_by(
      .data$regione,
      .data$area,
      .data$settimana,
      .data$sex
    ) %>%
    dplyr::summarise(decessi = sum(.data$n_death))

  data_week_age <- data_inizio_2020 %>%
    dplyr::filter(.data$sex == "totale") %>%
    dplyr::group_by(
      .data$regione,
      .data$area,
      .data$settimana,
      .data$classe_di_eta,
    ) %>%
    dplyr::summarise(decessi = sum(.data$n_death))



  ### by sex (fig 5)
  gg_fig_5_week_sex <- data_week_sex %>%
    ggplot(aes(
      x = .data$settimana,
      y = .data$decessi,
      colour = .data$regione
    )) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(.data$area ~ .data$sex, scales = "free_y") +
    labs(y = "Numero decessi 1-20 marzo") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.background = element_blank()
    )


  ### bay age (fig 6)
  gg_fig_6_week_age <- data_week_age %>%
    ggplot(aes(
      x = .data$settimana,
      y = .data$decessi,
      colour = .data$regione
    )) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(.data$area ~ .data$classe_di_eta, scales = "free_y") +
    labs(y = "Numero decessi 1-20 marzo") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.background = element_blank()
    )



# Output (reactive) objects ---------------------------------------

  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$tab_0_residenti <- DT::renderDT({
      residenti_anpr_1084
    })

    output$fig_1_age <- renderPlotly({
      clean_ggplotly(gg_fig_1_age)
    })

    output$tab_1_age <- DT::renderDT({
      mort_data_reg_age
    })

    output$fig_2_sex <- renderPlotly({
      clean_ggplotly(gg_fig_2_sex)
    })

    output$tab_2_sex <- DT::renderDT({
      mort_data_reg_sex
    })

    output$fig_3_year_all <- renderPlotly({
      clean_ggplotly(gg_fig_3_year_all)
    })

    output$fig_4_year_age <- renderPlotly({
      clean_ggplotly(gg_fig_4_year_age)
    })

    output$fig_5_week_sex <- renderPlotly({
      clean_ggplotly(gg_fig_5_week_sex)
    })

    output$fig_6_week_age <- renderPlotly({
      clean_ggplotly(gg_fig_6_week_age)
    })

  })
}

## To be copied in the UI
# mod_focus_20200404_magnani_ui("magnani_1")

## To be copied in the server
# mod_focus_20200404_magnani_server("magnani_1")

