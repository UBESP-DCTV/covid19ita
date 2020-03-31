#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
eng_app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    dashboardPage(title = " COVID-19 - Italy ",
      # Helpers for dashboard's header and sidebar
      eng_dashboard_header(), eng_dashboard_sidebar(), eng_dashboard_body(),
      # Main dashboard skin colour
      skin = "red"
    )
  )
}




#' Header's dashboard
#'
#' Helper function defining the header of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardHeader
#' @noRd
eng_dashboard_header <- function() {dashboardHeader(

  title = "COVID-19 - Italy",

  dropdownMenu(type = "messages",
messageItem(
  from = "Highlights",
  message = "2020-03-31 Comparative Ven-Pie",
  icon = icon("search")
  ),
messageItem(
      from = "Data",
      message = glue::glue("Data updated ({last_data_update})"),
      icon = icon("database")
    ),
    messageItem(
      from = "Lingua",
      message = "Clicca qui per il sito in italiano",
      icon = icon("flag"),
      href = "https://r-ubesp.dctv.unipd.it/shiny/covid19ita/"
    )
  )
)}




#' Sidebars dashboard
#'
#' Helper function defining the sidebar of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
eng_dashboard_sidebar <- function() {dashboardSidebar(sidebarMenu(
  id = "sidebar",

  eng_mod_help_plot_ui("help"),

  menuItem("Home", tabName = "home", icon = icon("home")),

  menuItem("Highlights", icon = icon("bullseye"),
           menuSubItem("2020-03-31 Comparative", tabName = "20200331Comp",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-28 Veneto", tabName = "20200328Tot_hosp",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-25 Veneto", tabName = "20200325Hosp",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-24 Methodology", tabName = "20200323Picco",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-21 Alessandria", tabName = "20200321Alessandria",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-21 Vercelli", tabName = "20200321Vercelli",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-20 Novara", tabName = "20200320Novara",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-19 Veneto", tabName = "20200319Veneto",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-18 Piemonte", tabName = "20200318Piemonte",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-18 FVG", tabName = "20200318Fvg",
                       icon = icon("flag")
           ),
           menuSubItem("2020-03-14 Veneto", tabName = "20200314Veneto",
                       icon = icon("flag")
           )
  ),

  menuItem("Epidemic", icon = icon("chart-line"),
    menuSubItem("National", tabName = "national", icon = icon("flag")),
    menuSubItem("Regional", tabName = "regional", icon = icon("map")),
    menuSubItem("Provincial", tabName = "provincial", icon = icon("location-arrow"))
  ),

  menuItem("Principal indices", tabName = "impact", icon = icon("compass")),

  menuItem("Issues", icon = icon("exclamation-triangle"),
    href = "https://github.com/UBESP-DCTV/covid19ita/issues/"
  ),

  menuItem("Info and sources", tabName = "data_tab", icon = icon("database")),

  eng_mod_info_sidebar_ui("summary_today")

))}




#' Body dashboard
#'
#' Helper function defining the main body of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardBody box
#' @noRd
eng_dashboard_body <- function() {dashboardBody(
  tags$head(includeScript(app_sys('app/www/google-analytics.js'))),
  tabItems(
  tabItem(tabName = "home",
    fluidPage(title = HTML("Project <strong>covid19ita</strong>"),

      p(HTML('<strong>WARNING: Translation in progress.</strong>')),
      p(''),
      p('Note: the website is optimized for computer viewing; in case of mobile or tablet viewing landscape mode is recommended.'),

      # box(width = 12, solidHeader = TRUE,
      #   mod_img_header_ui("logo_testa")
      # ),

      box(width = 12,
        p(
          HTML("The project <strong>covid19ita</strong> was developed by the"),
          a(href = 'https://ubesp.jimdofree.com/', target = '_blank', 'Unit of Biostatistics, Epidemiology, and Public Health'),
          " of the", a(href = 'https://www.dctv.unipd.it/', target = '_blank', 'Department of Cardiac, Thoracic and Vascular Sciences and Public Health'),
          " at ", a(href = 'https://www.unipd.it/', target = '_blank', 'Università degli Studi di Padova'), ",",
          " in partnership with the ", a(href = 'https://www.dscb.unito.it/do/gruppi.pl/Tree', target = '_blank', 'Department of Clinical and Biological Sciences'),
          " at ", a(href = 'https://www.unito.it/', target = '_blank', 'Università degli Studi di Torino'), ",",
          " and the ", a(href = 'https://www.dimet.uniupo.it/', target = '_blank', 'Department of Translational Medicine'),
          " at ", a(href = 'https://www.uniupo.it/', target = '_blank', 'Università del Piemonte Orientale'), "."
        )
      ),

      h2("Work group"),
      box(width = 12, title = HTML("<strong>Project coordinator</strong>"),
        p(
          HTML("Prof. <strong>Dario Gregori</strong>, PhD, Head of the
            Unit of Biostatistics, Epidemiology, and Public Health of the Department
            of Cardiac, Thoracic and Vascular Sciences and Public Health --
            Università degli Studi di Padova."
          ),
          a(href = "https://linkedin.com/in/dario-gregori-2720039/", target = "_blank", "LinkedIn")
        )
      ),

      box(width = 12, title = HTML("<strong>App and R package development</strong>"),
        p(HTML("<strong>Corrado Lanera</strong>, PhD, Unit of
          Biostatistics, Epidemiology, and Public Health of the Department
          of Cardiac, Thoracic and Vascular Sciences and Public Health --
          Università degli Studi di Padova. Head of the Laboratory of Artificial Intelligence for Medical Sciences"),
          a(href = "https://linkedin.com/in/corradolanera/", target = "_blank", "LinkedIn")
        )
      ),


      box(width = 12, title = HTML("<strong>Epidemiological modeling</strong>"),
        p(
          HTML("Prof. <strong>Paola Berchialla</strong>, PhD, Department of Clinical
            and Biological Sciences -- Università degli Studi di Torino"
          ),
          a(href = "https://linkedin.com/in/paola-berchialla-36b44410/", target = "_blank", "LinkedIn")
        ),

        p(
          HTML(
            'Prof. <strong>Dolores Catelan</strong>, Ph.D., Department of
            Statistics, Computer Sciences, Applications "G. Parenti" (DISIA),
            Università degli Studi di Firenze'
          ),
          a(
            href = "https://www.linkedin.com/in/dolores-catelan-43998b23/",
            target = "_blank", "LinkedIn"
          )
        )
      ),

      box(width = 12, title = HTML("<strong>Predictive models</strong>"),
        p(HTML("<strong>Danila Azzolina</strong>, PhD, Department of Translational Medicine --
          Università del Piemonte Orientale"),
          a(href = "https://linkedin.com/in/danila-azzolina-862465166/", target = "_blank", "LinkedIn")
        ),

        p(HTML("<strong>Ilaria Prosepe</strong>, MSc, Unit of
           Biostatistics, Epidemiology, and Public Health of the Department
           of Cardiac, Thoracic and Vascular Sciences and Public Health --
           Università degli Studi di Padova."),
          a(href = "https://linkedin.com/in/ilaria-prosepe-1b52371a4/", target = "_blank", "LinkedIn")
        )
      ),


      box(width = 12, title = HTML("<strong>Modeling in Environmental Epidemiology and Pollution</strong>"),
          p(
            HTML(
              'Prof. <strong>Annibale Biggeri</strong>, MD, MSPH, MSB,
              Department of Statistics, Computer Sciences, Applications
              "G. Parenti" (DISIA), Università degli Studi di Firenze'
            ),
            a(href = "https://linkedin.com/in/danila-azzolina-862465166/", target = "_blank", "LinkedIn")
          ),
          p(
            HTML(
              'Prof. <strong>Cristina Canova</strong>, Ph.D., Unit of
           Biostatistics, Epidemiology and Public Health of the Department
           of Cardiac, Thoracic and Vascular Sciences and Public Health --
           Università degli studi di Padova.'
            ),
            a(href = "https://www.linkedin.com/in/cristina-canova-05448861/", target = "_blank", "LinkedIn")
          ),
          p(HTML("<strong>Elisa Gallo</strong>, MS., Unit of
           Biostatistics, Epidemiology and Public Health of the Department
           of Cardiac, Thoracic and Vascular Sciences and Public Health --
           Università degli studi di Padova."),
            a(href = "https://www.linkedin.com/in/elisa-gallo-9b3933152/", target = "_blank", "LinkedIn")
          ),
          p(HTML("<strong>Francesco Garzotto</strong>, MSc, Unit of
           Biostatistics, Epidemiology and Public Health of the Department
           of Cardiac, Thoracic and Vascular Sciences and Public Health --
           Università degli studi di Padova."),
            a(href = "https://www.linkedin.com/in/francesco-garzotto-19907826/", target = "_blank", "LinkedIn")
          )
      ),


      box(width = 12, title = HTML("<strong>Risk communication</strong>"),
        p(
          HTML("<strong>Giulia Lorenzoni</strong>, PhD, Unit of
            Biostatistics, Epidemiology, and Public Health of the Department
            of Cardiac, Thoracic and Vascular Sciences and Public Health --
            Università degli Studi di Padova. Head of the Laboratory of Clinical Epidemiology and Digital Health"
          ),
          a(href = "https://linkedin.com/in/giulia-lorenzoni-b382a6180/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML(
            "<strong>Nicolas Destro</strong>, MA, Unit of
            Biostatistics, Epidemiology, and Public Health of the Department
            of Cardiac, Thoracic and Vascular Sciences and Public Health --
            Università degli Studi di Padova."
          ),
          a(href = "https://www.linkedin.com/in/nicolas-destro-b2a67212b/", target = "_blank", "LinkedIn")
        )
      ),

            h2("Navigation instructions"),
      box(width = 12, title = HTML("<strong>Website structure</strong>"),
        HTML(
          "<ol>
            <li><strong>Home</strong>: This page.</li>
            <li><strong>Highlights</strong>: Main considerations for the Veneto region.</li>
            <li><strong>Epidemic</strong>: Dynamic and interactive time series.</li>
            <li><strong>Principal indices</strong>: Regional and national models and predictions.</li>
            <li><strong>Issues</strong>: Link to report issues with the website.</li>
            <li><strong>Info and sources</strong>: Sources description, Use License and software used for the app development.</li>
            <li><strong>Latest metrics</strong>: Choose a region (or 'Italy', default) to visualize the most relevant metrics based on the latest data. Data is updated daily, usually at 6pm (local time), by the Italian Civil Protection.</li>
          </ol>"
        )
      ),

      box(width = 12, title = HTML("<strong>How to use the dynamic graphs</strong>"),
          p(HTML("The majority of graphs on this website are dynamic. The user can choose:")),
          p(""),
          p(HTML("1. <strong>the information he/she wants to visualize</strong>: while looking at one graph the user can <strong>visualize further details</strong> by clicking or hovering the mouse cursor over the different points/curves; <strong>zoom</strong> on some areas of interest by clicking on the semi-transparent buttons +/- at the top right of the graph (or by selecting the area with the mouse cursor). If multiple information is reported on the same graph (e.g. multiple regions o measures), it is possible to <strong>exclude part of the information</strong> by clicking on the right legend items or <strong>visualize only one piece of information</strong> by double-clicking on it. It is moreover possible to <strong>save each graph</strong> independently by clicking on the semi-transparent camera button. By clicking on the small-house button the <strong> original version</strong> of the graph is restored.")),
          p(HTML("2. <strong>which and how much information is to be processed and shown</strong>: Whenever cells appear above the graph, the user can decide to<strong> add or remove regions, provinces or metrics</strong> (from those available when the pane is selected). ")),
      ),

    )

  ),
  tabItem(tabName = "20200331Comp",
          h2("Comparative analysis between the Piemonte Region and Veneto Region of the epidemiological data relative to Covid-19 infection."),
          eng_mod_focus_20200331_ui("ven_pie")
  ),
  tabItem(tabName = "20200328Tot_hosp",
          h2("Possible effect on hospitalizations of the health policies implemented by the Veneto region"),
          eng_mod_focus_20200328_hosp_ui("tot")
  ),
  tabItem(tabName = "20200325Hosp",
          h2("Possible effect on hospitalizations of the health policies implemented by the Veneto region"),
          eng_mod_focus_20200325_hosp_ui("hosp")
  ),
  tabItem(tabName = "20200323Picco",
          h2("Impact of statistical uncertainty on COVID-19 predictions"),
          eng_mod_focus_20200323_picco_ui("picco")
  ),
  tabItem(tabName = "20200321Alessandria",
          h2("Expected number of total cases in Alessandria"),
          eng_mod_focus_20200320_novara_ui("da_alessandria")
  ),
  tabItem(tabName = "20200321Vercelli",
          h2("Expected number of total cases in Vercelli"),
          eng_mod_focus_20200320_novara_ui("da_vercelli")
  ),

  tabItem(tabName = "20200320Novara",
          h2("Expected number of total cases in Novara"),
          eng_mod_focus_20200320_novara_ui("da_novara")
  ),

  tabItem(tabName = "20200319Veneto",
          h2("Possible effect on ICU admissions of the health policies implemented by the Veneto region"),
          eng_mod_focus_20200318_veneto_intensive_ui("21")
  ),

  tabItem(tabName = "20200318Piemonte",
          h2("Possible effect of the health policies implemented by the Piemonte region"),
          eng_mod_focus_20200318_piemonte_ui("20200318_piemonte")
  ),
  tabItem(tabName = "20200318Fvg",
          h2("Possible effect of the health policies implemented by the Friuli Venezia Giulia region"),
          eng_mod_focus_20200318_friuli_ui("20200318_fvg")
  ),
  tabItem(tabName = "20200314Veneto",
    h1("Possible effect of the health policies implemented by the Veneto region"),
    eng_mod_focus_20200314_ui("dapb")
  ),

  tabItem(tabName = "national",
    h2("National events"),
    box(width = 12, title = "Instructions",
        p("Visualize/hide one or more measures on the graph by clicking on the items in the legend. Double click to only visualize the selected item."),
        p("Click on the autoscale button (the third one) to maximize the size of the graph."),
    ),
    eng_mod_ts_ita_ui("ts_nat_cum", title = "Time series trend - cumulative events"),
    eng_mod_ts_ita_ui("ts_nat_inc", title = "Time series trend - daily new events")
  ),


  tabItem(tabName = "regional",
    h2("Regional events"),
    box(width = 12, title = "Istructions",
        p("Add/remove computations for one or more region/measure by adding/removing it from the box."),
        p("NOTE: The number of tests per day can be visualized on the graph (by selecting the correct item in the legend) but it is hidden by default as it is off the scale (compared to the other measures)."),
        p(""),
        p("Visualize/hide one or more regions/measures on the graph by clicking on the items in the legend. Double click to only visualize the selected item."),
        p("Click on the autoscale button (the third one) to maximize the size of the graph."),
      ),

    h3("Time series by region"),
    fluidRow(
      box(title = "Cumulative events", width = 12,
        eng_mod_ts_reg_ui("ts_reg_cum_mes")
      )
    ),

    fluidRow(
      box(title = "Daily new events", width = 12,
        eng_mod_ts_reg_ui("ts_reg_inc_mes")
      )
    ),

    h3("Regional time series by event"),
    fluidRow(
      box(title = "Cumulative events", width = 12,
        eng_mod_ts_reg_ui("ts_reg_cum_reg")
      )
    ),

    fluidRow(
      box(title = "Daily new events", width = 12,
        eng_mod_ts_reg_ui("ts_reg_inc_reg")
      )
    )

  ),


  tabItem(tabName = "provincial",
    h2("Provincial events"),
    p("Add/remove computations for one or more region/measure by adding/removing it from the box."),
    p("Visualize/hide one or more regions on the graph by clicking on the items in the legend. Double click to only visualize the selected item."),
    p("Click on the autoscale button (the third one) to maximize the size of the graph."),

    h3("Time series"),
    fluidRow(
      box(title = "Cumulative events", width = 12,
        eng_mod_ts_prv_ui("ts_prv_cum")
      )
    ),

    fluidRow(
      box(title = "New events", width = 12,
        eng_mod_ts_prv_ui("ts_prv_inc")
      )
    )
  ),

  tabItem(tabName = "data_tab",
    h2("Data Info"),

    p("This app takes the needed data from the offical records that track the italian COVID-19 outbreak, on a national, regional and provincial level"),

    p(
      'Data are processed and made available by the ',
      a(href = "http://www.protezionecivile.it/web/guest/home", target = "_blank", "Presidenza del Consiglio dei Ministri - Dipartimento di Protezione Civile"),
      ' (Italian Civil Protection Department) and licensed under ', a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
      'as provided by the ', a(href = "", target = "_blank", "Ministero della Salute"), '(Ministry of Health).'
    ),

    p('Data is usually updated daily at 6pm.'),

    p(
      "Further information about data attribution and availability can be found at the",
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = "_blank", "web page of covid19ita."),
      "."
    ),

    h2("Software"),
    p(HTML("The app <strong>covid19ita</strong> was developed using R ver. 3.6.3 as an expansion package. The source code of the package and the app is freely available online on Github at "),
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = '_blank', 'https://github.com/UBESP-DCTV/covid19ita/'), "."
    ),

    p("For the app development the following expansion packages were used: {shiny} ver. 1.4.0, {shinydashboard} v.0.7.1 and {golem} ver. 0.2.1."),

    p("The analyses were performed using functions of the following packages: {stats} ver. 3.6.3, and {gam} ver. 1.16.1"),

    p("Our graphs were produced with the following packages: {ggplot2} ver. 3.3.0 and {plotly} ver. 4.9.2."),

    h3('Note for R users'),
    p(
      'In addition to this app (that can be locally excecuted, after installing the package {covid19ita}, by running the code `run_app()`), the R package {covid19ita}, available on ',
      a(href = 'https://github.com/UBESP-DCTV/covid19ita/', target = '_blank', 'GitHub'),
      ' and licenced under ',
      a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
      ', makes raw data available for direct use.'
    )

  ),


  tabItem(tabName = "impact",
    h1("Principal indices"),
    eng_mod_ind_ita_ui("20200315")
  )

))}




#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
eng_golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = ' {covid19ita} ',
      meta = list(
        title = "{covid19ita} App",
        description = "Monitoring platform analysis of the COVID-19 infection in Italy.",
        url = "https://r-ubesp.dctv.unipd.it/shiny/covid19ita/",
        image = app_sys('app/www/ubep_covid.png'),
        image_alt = "Emergenza COVID-19 ITA",
        twitter_creator = "@CorradoLanera",
        twitter_card_type = "summary",
        twitter_site = "@ubesppadova",
        og_locale = "it_IT",
        og_author = c(
          "Corrado L.", "Dario G.", "Paola B.", "Danila A.", "Giulia L.",
          "Ilaria P.", "Nicolas D."
        ),
        og_site_name = "covid19ita"
      )
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
