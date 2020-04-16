#' focus_20200404_magnani UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
eng_mod_focus_20200404_magnani_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(box(
      width = 12,
      p(HTML("
        Prof. <strong>Corrado Magnani</strong>
        <br>
        Department of Translational Medicine -- Universit\u00E0 del Piemonte Orientale
      ")),
    )),

    fluidRow(box(
      width = 12,

      p(HTML("
      The National Institute of Statistics (Istat) made available on its
      website (https://www.istat.it/it/archivio/240401) the mortality data
      of 1084 Italian municipalities, with data updated to the 21st of
      March 2020. 1.<sup>1</sup>
      ")),

      p(HTML("
        As it is possible to see on the Istat website,
        the municipalities that take part in this
        analysis are the ones that counted at least 10 deaths in the period
        1 January 2020 to 28 March 2020 and that registered a rise in mortality
        of at least 20 % in the first 21 or 28 days of March 2020.
        The selection criteria introduced by ISTAT causes an
        overestimate of mortality. For this reason the numbers we present
        here must be seen as the highest forseeable values.
      ")),

      p(HTML("
        The data collection includes 6.177.016 men and 6.496.805 women,
        distributed according to residence. Representation is different
        in the different regions (Table 1).
      ")),
    )),

    fluidRow(box(
      width = 12,
      DT::DTOutput(ns("tab_0_residenti")),
      title = "Table 1: Residents in the 1084 Italian municipalities as of the 1st of January 2019."
    )),

    fluidRow(box(
      width = 12,
      p(HTML("
        Overall mortality is a strong indicator as it has low
        susceptibility to errors or discrepancies in assessments
        and it accounts for both the mortality caused directly by
        a specific pathology and the mortality caused in an indirect
        way, as for example by difficulties for people who suffer
        from different pathologies in accessing the hospital services.
      ")),

      p(HTML("
        Moreover, the overall mortality is not affected by diagnostic
        questions or difficulties in coding cause of death and is
        therefore a useful foundation on which we can build an
        accurate estimate of the effects of the COVID-19 epidemics.
      ")),

      p(HTML("
        Data is made available by Istat in different tables, which
        can be accessed and downloaded from the official website.
        The tables allow an immediate reading and can also be used
        for further analysis. We hence used the data in order to
        make some descriptive analysis, that are essentially presented
        in the form of graphs, in order to illustrate the trend in
        overall mortality by geographic area, sex, age and time period.
      ")),

      p(HTML("
        These are some preliminary analyses that aim at sharing
        information during times of emergencies, that will be improved
        and explored further in the coming weeks. In particular the
        current goal is only to give a reasoned presentation of the
        absolute values and the change percentages. Further analyses
        will be conducted in order to reach a better modelling of
        the trend and to improve the indices of the confidence intervals.
      ")),

      p(HTML("
        The analyses want to answer to the following questions:
        <ul>
          <li> What is the entity of the observed mortality change
          if we compare the period from the 1st to the 21st of March
          2019 to the period from the 1st to the 21st of March 2020?
          <li> How the mortality change distributed by sex, age and
          region of residency?
          <li> If we also consider previous years, starting from 2015,
          can we observe relevant change throughout the different years?
          And again, what is the distribution by sex, age and region
          of residency?
          <li> Starting from which week of the year is it possible
          to observe change of the overall mortality?
        </ul>
      ")),

      p(HTML("
        Note on aggregation and numerosity of data: some
        variables were grouped into wider categories, as
        indicated in the analyses results.
      "))
    )),





    fluidRow(box(
      width = 12,
      h2(HTML("
        How much did the overall mortality change from last year
        (1-21 March 2019 vs 1-21 March 2020)? How is the mortality
        change distributed by sex, age, and region of residency?
      ")),

      p(HTML("
        The percentage change in mortality (1-21 March 2019 vs 1-21
        March 2020) was estimated by region, sex and age aggregated
        data. Remember data only includes the municipalities provided
        by Istat. Data was categorized as in the table provided by Istat
        (https://www.istat.it/it/files//2020/03/Tavola-sintetica-decessi.xlsx).
        Age categories are: 65-74, 75-84, over 85.
      ")),

      p(HTML("
        The percentage change is defined as:
      ")),

      p(HTML("
        change<sub>%</sub> =
          100 * (
            deaths<sub>2020</sub> --
            deaths<sub>2019</sub>
          ) /
          deaths<sub>2019</sub>
      ")),

      p(HTML("
        This index appears in the original table, computed for
        each municipality.  In order to reduce statistical variability
        given by random fluctuations, which is rather high in those
        municipalities with a low number of inhabitants, we computed
        the percentage change  on a regional level. Total deaths,
        number of municipalities included in the measurement for
        each region and percentage change (from 2019 to 2020) are
        shown in the table.<sup>2</sup>
      ")),

      p(HTML("
        The analysis was also conducted separately by age class and
        sex, and results are presented in the following graphs
        (Figure 1 and Figure 2)
      "))
    )),

    fluidRow(box(plotlyOutput(ns("fig_1_age")),
      title = "Figure 1: Change percentage by age and region. 1-21 March 2019 vs. 1-21 March 2020.",
      width = 12
    )),

    fluidRow(box(
      width = 12,
      p(HTML("
        All regions show increased mortality for the two
        oldest age classes. Moreover, the majority of regions
        also present an increased mortality between 65 and 74
        years. For a correct reading of the percentage change
        it is necessary to remember that number of total deaths
        is very different from one region to another as the
        sample size can change quite a lot. In some regions
        the mortality change seems quite important, but it is
        actually given by a small difference in terms of number
        of deaths.
      "))
    )),

    fluidRow(box(
      width = 12, Title = "Table 2: Change percentage by class and regions. 1-21 March 2019 vs. 1-21 March 2020.",
      DT::DTOutput(ns("tab_1_age"))
    )),

    fluidRow(box(plotlyOutput(ns("fig_2_sex")),
      title = "Figure 2: Change percentage by sex and regions. Periodo 1-21 March 2019 vs. 1-21 March 2020.",
      footer = "f: female; m: male; mf: total",
      width = 12,
    )),

    fluidRow(box(
      width = 12, Title = "Table 3: Change percentage by sex and regions. 1-21 March 2019 vs. 1-21 March 2020.",
      DT::DTOutput(ns("tab_2_sex"))
    )),


    fluidRow(box(
      width = 12,
      h2(HTML("
        Considering the data on mortality starting from 2015,
        what is the entity of the change registered throughout
        the years by sex, age and region of residency?
      ")),

      p(HTML("
        The data provided by Istat allows to analyse the mortality
        trend starting from 2015. Data can be found at
        https://www.istat.it/it/files//2020/03/dati-comunali-settimanali-ANPR-1.zip.
        Further analyses will be conducted in the coming weeks
        to better explore mortality between 2015 and 2020.
        Anyhow, a descriptive analysis is still useful to notice
        the low variability of the number of deaths between 2015 and 2019.
      ")),

      p(HTML("
        Deaths in all municipalities in the Istat database
        belonging to the same region were summed together
        in order to obtain the number of deaths by province.
        The graphs here below (Figure 3) show the number of total
        deaths by region from 2015 to 2020. Regions were aggregated
        in two groups in order to enhance the readability of the
        graphs. The two groups are based on an Istat classification
        that splits regions in \"North Regions\" and \"South Central
        Regions and Islands\".
      "))
    )),


    fluidRow(box(plotlyOutput(ns("fig_3_year_all")),
      title = "Figure 3: Number of deaths by regions in the period 1-21 March from 2015 to 2020.",
      width = 12,
    )),

    fluidRow(box(
      width = 12,
      p(HTML("
        The graphs here below (Figure 4) show how mortality
        changed from 2015 to 2020 by region and age. Age
        classes were defined as follows: under 64 (putting
        together the classes 0-14 and 15-64 of the original
        table), 65-74, over 75.
      ")),

      p(HTML("
        It is necessary to keep in mind that the graphs
        only show absolute numbers, hence differences
        between regions are mainly due to different sample sizes.
      "))
    )),

    fluidRow(box(
      plotlyOutput(ns("fig_4_year_age")),
      title = "Figure 4: Number of deaths by regions and age in the period 1-21 March from 2015 to 2020.",
      width = 12,
    )),






    fluidRow(box(
      width = 12,
      h2(HTML("
        In which week of the year is it possible to notice
        change in the overall mortality?
      ")),

      p(HTML("
        Data presented in the table at
        https://www.istat.it/it/files//2020/03/dati-comunali-settimanali-ANPR-1.zip
        for the period of time that goes from the 1st of January
        to the 21st of March can be helpful to answer this question.
        The data in the table is divided into time slots of 7 days,
        except for the period from the 1st to the 10th of January;
        this period was therefore excluded from the analysis. The
        following graphs (Figure 5 and 6) present the trend by age,
        sex and region. As in the previous analyses, age is divided
        into classes (same as before) and regions are divided into
        two groups (same as before).
      ")),

      p(HTML("
        In this study it is possible to notice increased mortality
        starting from the week of the 1st of March, especially in those
        regions that are most affected by the epidemics (especially the
        Lombardy Region). Remind that the municipalities included in the
        analysis are the ones made available by Istat.
      "))
    )),

    fluidRow(box(
      plotlyOutput(ns("fig_5_week_sex")),
      title = "Figure 5: Number of weekly deaths by sex and region from the 12th to the 21st of March 2020.",
      width = 12,
    )),

    fluidRow(box(
      plotlyOutput(ns("fig_6_week_age")),
      title = "Figure 6: Number of weekly deaths by region and age from the 12th to the 21st of March 2020.",
      width = 12,
    )),



    fluidRow(box(
      width = 12, title = "Notes",
      p(HTML("
        <sup>1</sup> For further information on data collection
        see the Istat methodology.
        <br>
        <sup>2</sup> 2.If the index is equal to 100% it means
        the mortality has doubled.
      "))
    ))
  )
}



















#' focus_20200404_magnani Server Function
#'
#' @noRd
eng_mod_focus_20200404_magnani_server <- function(id) {

  # Data preparation ------------------------------------------------

  ## 1-2: variazione percentuale 2019-2020 --------------------------

  ### by age (fig 1)
  gg_fig_1_age <- mort_data_reg_age %>%
    ggmort("Age class") +
    ggtitle("Overall mortality by age class",
      subtitle = "1-21 March 2019 vs 2020"
    )

  ### by age (fig 2)
  gg_fig_2_sex <- mort_data_reg_sex %>%
    ggmort("Sex") +
    ggtitle("Overall mortality by sex",
      subtitle = "1-21 March 2019 vs 2020"
    )




  ## 3: mortalit\u00E0  prime tre settimane di marzo 2015-2020 ------------

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
  data_year_marzo_age <- data_year_marzo %>%
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
    labs(y = "Number of deaths 1-20 marzo") +
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
    labs(y = "Number of death 1-20 marzo") +
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
    labs(y = "Number of death 1-20 marzo") +
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
