#' focus_20200406_mort_veneto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_focus_20200406_mort_veneto_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(box(
      width = 12,

      p(HTML("
        L'Istituto Nazionale di Statistica (Istat) ha messo a
        disposizione su proprio sito web
        (https://www.istat.it/it/archivio/240401) i risultati della
        rilevazione della mortalit\u00E0  effettuata in 1084 comuni italiani,
        con dati aggiornati al 21 marzo 2020.<sup>1</sup> I comuni veneti
        inclusi in questo archivio sono 122.
      ")),

      p(HTML("
        Come comunicato sul sito dell'Istat,
        sono stati considerati i comuni che \"presentano almeno dieci
        decessi da gennaio al 28 marzo 2020 (perch\u00E9  meno esposti a eccessive
        variazioni  nei  dati  giornalieri)  e che hanno  fatto  registrare
        un  aumento  dei morti pari o superiore al 20 percento nei primi
        21 o 28 giorni di marzo 2020, rispetto al dato medio dello
        stesso periodo degli anni 2015-2019\".
        Il criterio di selezione introdotto da ISTAT porta ad una sovrastima
        dell'aumento di mortalit\u00E0  per cui i dati che presentiamo devono essere
        intesi come i valori massimi prevedibili.
      ")),
    )),
    fluidRow(box(
      width = 12,
      p(HTML("
        La mortalit\u00E0  complessiva costituisce un indicatore estremamente
        rilevante perch\u00E9  \u00E8 poco suscettibile ad errori o difformit\u00E0 di
        valutazione e tiene conto sia della mortalit\u00E0  direttamente
        indotta da una patologia sia della mortalit\u00E0  causata in modo
        indiretto, ad esempio per le possibili difficolt\u00E0  nell'accesso
        ai servizi ospedalieri per persone affette da altre patologie.
      ")),

      p(HTML("
        La mortalit\u00E0  complessiva inoltre non risente di quesiti
        diagnostici o di difficolt\u00E0  nella codifica delle cause di morte
        e quindi \u00E8  una utile base su cui andr\u00E0 costruita la valutazione
        pi\u00F9  dettagliata degli effetti della epidemia di COVID-19 in
        atto.
      ")),

      p(HTML("
        I dati sono forniti da Istat in diverse tabelle, accessibili e
        scaricabili dal sito istituzionale, che consentono una immediata
        lettura e che possono anche essere utilizzate per la
        preparazione di ulteriori analisi. Abbiamo utilizzato questa
        seconda opportunit\u00E0  per preparare alcune analisi descrittive,
        che sono presentate essenzialmente sotto forma di grafici, per
        illustrare l'andamento della mortalit\u00E0 complessiva per area
        geografica, sesso, classe di et\u00E0  e periodo.
      ")),

      p(HTML("
        Si tratta di analisi preliminari, finalizzate alla condivisione
        di informazioni in un momento di emergenza, che saranno
        migliorate ed approfondite nel prossimo periodo. In particolare
        l'obiettivo attuale \u00E8 limitato alla presentazione ragionata dei
        valori assoluti e dei coefficienti di variazione percentuali.
        Saranno condotte analisi supplementari per giungere ad una
        migliore modellizzazione dei trend e per arricchire gli indici
        dei relativi intervalli di confidenza.
      ")),

      p(HTML("
        Le analisi sono state condotte per rispondere ai seguenti
        quesiti:
        <ul>
          <li> Di quale entit\u00E0  \u00E8  la variazione di mortalit\u00E0 osservata
               confrontando il periodo tra il 1 ed il 21 marzo 2019 con
               il periodo tra il 1 ed il 21 marzo 2020?
          <li> La variazione osservata come \u00E8  distribuita in relazione
               al sesso, alla classe di et\u00E0  ed alla provincia di
               residenza?
          <li> Estendendo la valutazione agli anni precedenti, partendo
               dal 2015, esistono variazioni tra i diversi anni e di
               quale entit\u00E0, sempre considerando, classe di et\u00E0 e
               provincia di residenza?
          <li> A partire da quale settimana di rilevazione (considerando
               il periodo dal 1 gennaio 2020 al 21 marzo 2020) \u00E8
               osservabile una variazione della mortalit\u00E0  complessiva?
        </ul>
      ")),

      p(HTML("
        In relazione all'aggregazione dei dati nelle tabelle fornite da
        Istat e alla numerosit\u00E0  dei dati osservati alcune variabili
        sono state raggruppate in categorie pi\u00F9  ampie, come indicato
        nella presentazione dei risultati delle diverse analisi.
      "))
    )),





    fluidRow(box(
      width = 12,
      h2(HTML("
        Di quale entit\u00E0  \u00E8  la variazione di mortalit\u00E0 osservata
        confrontando il periodo tra il 1 e il 21 marzo 2019 con il
        periodo tra il 1 e il 21 marzo 2020? La variazione osservata
        come \u00E8  distribuita in relazione al sesso, alla classe di et\u00E0 e
        alla provincia di residenza?
      ")),

      p(HTML("
        L'indicatore della variazione percentuale tra il numero di
        morti osservate nel periodo dal 1 al 21 marzo 2019 e nel
        corrispondente periodo del 2020 \u00E8  stato calcolato a partire dai
        dati aggregati per provincia, sesso e classe di et\u00E0. In questo caso
        l'aggregazione per provincia comprende soltanto i comuni di cui Istat
        ha messo a disposizione i dati.Sono state
        usate le categorizzazioni presenti nella tabella fornita
        dall'Istat
        (https://www.istat.it/it/files//2020/03/Tavola-sintetica-decessi.xlsx).
        Le classi di et\u00E0  considerate sono: 65-74 anni, 75-84 anni,
        85 anni e oltre, gi\u00E0  presenti nei dati.
      ")),

      p(HTML("
        L'indice di variazione percentuale \u00E8 calcolato come:
      ")),

      p(HTML("
        variazione<sub>%</sub> =
          100 * (
            numero decessi<sub>2020</sub> --
            numero decessi<sub>2019</sub>
          ) /
          numero decessi<sub>2019</sub>
      ")),

      p(HTML("
        L'indice \u00E8 presente nella tabella dei dati originali calcolato a
        livello comunale. In queste analisi \u00E8  stato calcolato con
        aggregazione provinciale, per ridurre la variabilit\u00E0  conseguente
        alle fluttuazioni casuali che sono particolarmente marcate nel
        caso di comuni di piccole dimensioni, che costituiscono una
        parte rilevante della base dati. Sono riportati i numeri
        assoluti di decessi, per ciascuna provincia e le variazioni
        percentuali dal 2019 al 2020, nel periodo
        considerato.<sup>2</sup>
      "))
    )),


    fluidRow(box(
      width = 12,
      p(HTML("
        L'analisi \u00E8 stata condotta anche separatamente per classe di
        et\u00E0, e per sesso, i risultati sono presentati nei grafici
        seguenti (figura 1 e figura 2):
      "))
    )),

    fluidRow(box(plotlyOutput(ns("fig_1_age")),
      title = "Figura 1: Variazione percentuale per classi di et\u00E0  e provincia. Periodo 1-21 marzo 2019 vs. 1-21 marzo 2020.",
      width = 12
    )),

    fluidRow(box(
      width = 12,
      p(HTML("
        Nella lettura del valore di variazione
        percentuale occorre tenere conto della numerosit\u00E0  delle morti,
        che \u00E8  molto diversa tra le province a causa della diversa
        numerosit\u00E0  del campione. In alcune province una variazione
        appare importante ma \u00E8  causata da piccoli numeri di morti in
        pi\u00F9  o in meno (Tabella 1).
      "))
    )),

    fluidRow(box(
      width = 12, Title = "Tabella 1: Variazione percentuale per classi di et\u00E0  e provincia. Periodo 1-21 marzo 2019 vs. 1-21 marzo 2020.",
      DT::DTOutput(ns("tab_1_age"))
    )),

    fluidRow(box(plotlyOutput(ns("fig_2_sex")),
      title = "Figura 2: Variazione percentuale per sesso e provincia. Periodo 1-21 marzo 2019 vs. 1-21 marzo 2020.",
      footer = "f: femminile; m: maschile; mf: globale",
      width = 12,
    )),

    fluidRow(box(
      width = 12, Title = "Tabella 2: Variazione percentuale per sesso e provincia. Periodo 1-21 marzo 2019 vs. 1-21 marzo 2020.",
      DT::DTOutput(ns("tab_2_sex"))
    )),


    fluidRow(box(
      width = 12,
      h2(HTML("
        Estendendo la valutazione agli anni precedenti, partendo dal
        2015, esistono variazioni tra i diversi anni e di quale entit\u00E0,
        sempre considerando, classe di et\u00E0  e provincia di residenza?
      ")),

      p(HTML("
        La base dati messa a disposizione consente di valutare
        l'andamento della mortalit\u00E0 negli anni precedenti, a partire
        dal 2015. Sono stati usati i dati presentati nella tabella
        https://www.istat.it/it/files//2020/03/dati-comunali-settimanali-ANPR-1.zip
        Le analisi saranno approfondite in futuro valutando pi\u00F9
        appropriatamente l'andamento della mortalit\u00E0 nel periodo
        2015-2020 anche con metodi di modellizzazione.
      ")),

      p(HTML("
        I decessi nei comuni relativi all'indagine Istat
        sono stati sommati assieme al fine di ottenere un dato provinciale.
        Il grafico che segue (figura 3) presenta il numero totale di
        morti nel periodo 2015-2020 per provincia.
      "))
    )),


    fluidRow(box(plotlyOutput(ns("fig_3_year_all")),
      title = "Figura 3: Numero di decessi per provincia nel periodo 1-21 marzo dal 2015 al 2020.",
      width = 12,
    )),

    fluidRow(box(
      width = 12,
      p(HTML("
        I grafici seguenti (figura 4) presentano il confronto della
        mortalit\u00E0 negli anni dal 2015 al 2020, per provincia e classe di
        et\u00E0. Il numero di decessi nei comuni relativi all'indagine Istat
        sono stati sommati assieme al fine di ottenere un dato provinciale.
        Le classi di et\u00E0 sono state definite sulla base dei
        raggruppamenti con cui sono presentati i dati originali,
        e precisamente: fino a 64 anni (ottenuto sommando le due classi
        presenti nella tabella originale 0-14 e 15-64), da 65 a 74, 75
        e oltre.
      ")),

      p(HTML("
        Si ricorda che i grafici presentano i numeri assoluti e quindi
        le differenze tra le province riflettono in primo luogo la
        dimensione dei comuni coinvolti dall'indagine Istat per provincia.
      "))
    )),

    fluidRow(box(
      plotlyOutput(ns("fig_4_year_age")),
      title = "Figura 4: Numero di decessi per provincia e classi di et\u00E0 nel periodo 1-21 marzo dal 2015 al 2020.",
      width = 12,
    )),






    fluidRow(box(
      width = 12,
      h2(HTML("
        In quale settimana di rilevazione (considerando il periodo dal
        1 gennaio 2020 al 21 marzo 2020) \u00E8  osservabile una variazione
        della mortalit\u00E0 complessiva?
      ")),

      p(HTML("
        Per contribuire alla risposta a questo quesito sono stati usati
        i dati relativi ai 122 comuni veneti riportati nella tabella
        https://www.istat.it/it/files//2020/03/dati-comunali-settimanali-ANPR-1.zip,
        limitatamente ai dati per il periodo 1 gennaio -- 21 marzo 2020.
        Nella tabella tali dati sono presentati suddivisi a ritroso in
        periodi di 7 giorni. Cos\u00EC  facendo il primo periodo va da 1 al
        10 gennaio e quindi non \u00E8  confrontabile con i successivi perch\u00E9
        comprende 11 giorni di osservazione. In questa fase si \u00E8  deciso
        pertanto di escluderlo e di partire dal secondo periodo, che ha
        inizio il 12 gennaio 2020. I grafici seguenti (figura 5)
        presentano l'andamento per classe di et\u00E0 e provincia.
        I grafici riportano sull'asse orizzontale
        la data di inizio dei diversi periodi settimanali.
      ")),
    )),

    fluidRow(box(
      plotlyOutput(ns("fig_6_week_age")),
      title = "Figura 5: Numero di decessi settimanali per provincia e classi di et\u00E0 dall'12 al 21 marzo 2020.",
      width = 12,
    )),



    fluidRow(box(
      width = 12, title = "Notes",
      p(HTML("
        <sup>1</sup> Per maggiori informazioni sulla rilevazione si
        rimanda alla metodologia descritta da Istat.
        <br>
        <sup>2</sup> Un indice del 100% indica che la mortalit\u00E0 \u00E8
        raddoppiata tra i due periodi a confronto.
      "))
    ))
  )
}



















#' focus_20200406_mort_veneto Server Function
#'
#' @noRd
mod_focus_20200406_mort_veneto_server <- function(id) {

  # Data preparation ------------------------------------------------


  ## 1-2: variazione percentuale 2019-2020 --------------------------

  ### by age (fig 1)
  gg_fig_1_age <- mort_data_veneto_age %>%
    ggmort("Classe di et\u00E0", x = "provincia") +
    ggtitle("Mortalit\u00E0 totale per classe di et\u00E0",
      subtitle = "Confronto 1-21 marzo 2019 vs 2020"
    )

  ### by age (fig 2)
  gg_fig_2_sex <- mort_data_veneto_sex %>%
    ggmort("Sesso", x = "provincia") +
    ggtitle("Mortalit\u00E0 totale per sesso",
      subtitle = "Confronto 1-21 marzo 2019 vs 2020"
    )




  ## 3: mortalit\u00E0 prime tre settimane di marzo 2015-2020 ------------

  data_year_marzo_veneto <- mort_data_comuni %>%
    dplyr::filter(
      .data$settimana %in%
        c("01/03-07/03", "08/03-14/03", "15/03-21/03"),
      .data$regione == "Veneto"
    )


  ### all (fig 3)
  data_year_marzo_veneto_all <- data_year_marzo_veneto %>%
    dplyr::group_by(.data$provincia, .data$year) %>%
    dplyr::summarise(decessi = sum(.data$n_death))

  gg_fig_3_year_all <- data_year_marzo_veneto_all %>%
    ggplot(aes(
      x = .data$year,
      y = .data$decessi,
      colour = .data$provincia
    )) +
    geom_point() +
    geom_smooth(se = FALSE) +
    labs(y = "Numero decessi 1-20 marzo") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.background = element_blank()
    )


  ### by age (fig 4)
  data_year_marzo_veneto_age <- data_year_marzo_veneto %>%
    dplyr::group_by(
      .data$provincia, .data$year, .data$classe_di_eta
    ) %>%
    dplyr::summarise(decessi = sum(.data$n_death))

  gg_fig_4_year_age <- data_year_marzo_veneto_age %>%
    ggplot(aes(
      x = .data$year,
      y = .data$decessi,
      colour = .data$provincia
    )) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_wrap(.data$classe_di_eta ~ ., scales = "free_y") +
    labs(y = "Numero decessi 1-20 marzo") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.background = element_blank()
    )




  ## 4: prime settimane 2020 ----------------------------------------
  data_inizio_2020_veneto <- mort_data_comuni %>%
    dplyr::filter(
      .data$settimana != "01/01-11/01",
      .data$year == 2020,
      .data$regione == "Veneto"
    ) %>%
    dplyr::mutate(
      settimana = substr(.data$settimana, start = 1, stop = 5) %>%
        as.Date(format = "%d/%m")
    )


  data_week_veneto <- data_inizio_2020_veneto %>%
    dplyr::filter(.data$sex == "totale") %>%
    dplyr::group_by(
      .data$provincia,
      .data$settimana,
      .data$classe_di_eta
    ) %>%
    dplyr::summarise(decessi = sum(.data$n_death))



  ### bay age (fig 6)
  gg_fig_6_week_age <- data_week_veneto %>%
    ggplot(aes(
      x = .data$settimana,
      y = .data$decessi,
      colour = .data$provincia
    )) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_wrap(.data$classe_di_eta ~ ., scales = "free_y") +
    labs(y = "Numero decessi 1-20 marzo") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.background = element_blank()
    )


  # Output (reactive) objects ---------------------------------------

  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    output$fig_1_age <- renderPlotly({
      clean_ggplotly(gg_fig_1_age)
    })

    output$tab_1_age <- DT::renderDT({
      mort_data_veneto_age
    })

    output$fig_2_sex <- renderPlotly({
      clean_ggplotly(gg_fig_2_sex)
    })

    output$tab_2_sex <- DT::renderDT({
      mort_data_veneto_sex
    })

    output$fig_3_year_all <- renderPlotly({
      clean_ggplotly(gg_fig_3_year_all)
    })

    output$fig_4_year_age <- renderPlotly({
      clean_ggplotly(gg_fig_4_year_age)
    })

    output$fig_6_week_age <- renderPlotly({
      clean_ggplotly(gg_fig_6_week_age)
    })
  })
}

## To be copied in the UI
#> mod_focus_20200406_mort_veneto_ui("magnani_1")

## To be copied in the server
#> mod_focus_20200406_mort_veneto_server("magnani_1")
