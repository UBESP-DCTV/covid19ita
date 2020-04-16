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

    dashboardPage(
      title = " COVID-19 - Italy ",
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
eng_dashboard_header <- function() {
  dashboardHeader(
    title = "COVID-19 - Italy",

    dropdownMenu(
      type = "messages",
      messageItem(
        from = "Highlights",
        message = "Impact of testing on hospitalizations",
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
  )
}

#' Sidebars dashboard
#'
#' Helper function defining the sidebar of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
eng_dashboard_sidebar <- function() {
  dashboardSidebar(sidebarMenu(
    id = "sidebar",

    eng_mod_help_plot_ui("help"),

    menuItem("Home", tabName = "home", icon = icon("home")),

    menuItem("Highlights",
      icon = icon("bullseye"),

      menuSubItem(
        text = "2020-04-15 Effects of testing", tabName = "20200415TampHosp",
        icon = icon("flag")
      ),

      menuSubItem("2020-04-06 Veneto Mortality",
        tabName = "20200406MortVeneto",
        icon = icon("flag")
      ),
      menuSubItem("2020-04-05 Mortality",
        tabName = "20200405Mort",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-31 Comparative",
        tabName = "20200331Comp",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-28 Veneto",
        tabName = "20200328Tot_hosp",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-25 Veneto",
        tabName = "20200325Hosp",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-24 Methodology",
        tabName = "20200323Picco",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-21 Alessandria",
        tabName = "20200321Alessandria",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-21 Vercelli",
        tabName = "20200321Vercelli",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-20 Novara",
        tabName = "20200320Novara",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-19 Veneto",
        tabName = "20200319Veneto",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-18 Piemonte",
        tabName = "20200318Piemonte",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-18 FVG",
        tabName = "20200318Fvg",
        icon = icon("flag")
      ),
      menuSubItem("2020-03-14 Veneto",
        tabName = "20200314Veneto",
        icon = icon("flag")
      )
    ),

    menuItem("Epidemic",
      icon = icon("chart-line"),
      menuSubItem("National", tabName = "national", icon = icon("flag")),
      menuSubItem("Regional", tabName = "regional", icon = icon("map")),
      menuSubItem("Provincial", tabName = "provincial", icon = icon("location-arrow"))
    ),

    menuItem("Principal indices", tabName = "impact", icon = icon("compass")),

    menuItem("Maps", tabName = "geo_spatialTot", icon = icon("map-marked-alt")),

    menuItem("Issues",
      icon = icon("exclamation-triangle"),
      href = "https://github.com/UBESP-DCTV/covid19ita/issues/"
    ),

    menuItem("Info and sources", tabName = "data_tab", icon = icon("database")),

    eng_mod_info_sidebar_ui("summary_today")
  ))
}





#' Body dashboard
#'
#' Helper function defining the main body of the dashboard
#'
#' @import shiny
#' @importFrom shinydashboard dashboardBody box
#' @noRd
eng_dashboard_body <- function() {
  dashboardBody(
    tags$head(includeScript(app_sys("app/www/google-analytics.js"))),
    tabItems(
      eng_dashboard_home_body(),

      tabItem(
        tabName = "20200415TampHosp",
        h2("Impact of testing on hospitalizations"),
        eng_mod_focus_20200415_tamponi_ui("tamp_hosp")
      ),

      tabItem(
        tabName = "20200406MortVeneto",

        h2("Preliminary analysis of the overall mortality in 122 municipalities of the Veneto Region from the 1st of March to the 21 of March 2020."),
        eng_mod_focus_20200406_mort_veneto_ui("mort_veneto")
      ),
      tabItem(
        tabName = "20200405Mort",
        h2("Preliminary analysis of the overall mortality in 1084 italian municipalities from the 1st of March to the 21 of March 2020."),
        eng_mod_focus_20200404_magnani_ui("mortality")
      ),
      tabItem(
        tabName = "20200331Comp",
        h2("Comparative analysis between the Piemonte Region and Veneto Region of the epidemiological data relative to Covid-19 infection."),
        eng_mod_focus_20200331_ui("ven_pie")
      ),
      tabItem(
        tabName = "20200328Tot_hosp",
        h2("Possible effect on hospitalizations of the health policies implemented by the Veneto region"),
        eng_mod_focus_20200328_hosp_ui("tot")
      ),
      tabItem(
        tabName = "20200325Hosp",
        h2("Possible effect on hospitalizations of the health policies implemented by the Veneto region"),
        eng_mod_focus_20200325_hosp_ui("hosp")
      ),
      tabItem(
        tabName = "20200323Picco",
        h2("Impact of statistical uncertainty on COVID-19 predictions"),
        eng_mod_focus_20200323_picco_ui("picco")
      ),
      tabItem(
        tabName = "20200321Alessandria",
        h2("Expected number of total cases in Alessandria"),
        eng_mod_focus_20200320_novara_ui("da_alessandria")
      ),
      tabItem(
        tabName = "20200321Vercelli",
        h2("Expected number of total cases in Vercelli"),
        eng_mod_focus_20200320_novara_ui("da_vercelli")
      ),

      tabItem(
        tabName = "20200320Novara",
        h2("Expected number of total cases in Novara"),
        eng_mod_focus_20200320_novara_ui("da_novara")
      ),

      tabItem(
        tabName = "20200319Veneto",
        h2("Possible effect on ICU admissions of the health policies implemented by the Veneto region"),
        eng_mod_focus_20200318_veneto_intensive_ui("21")
      ),

      tabItem(
        tabName = "20200318Piemonte",
        h2("Possible effect of the health policies implemented by the Piemonte region"),
        eng_mod_focus_20200318_piemonte_ui("20200318_piemonte")
      ),
      tabItem(
        tabName = "20200318Fvg",
        h2("Possible effect of the health policies implemented by the Friuli Venezia Giulia region"),
        eng_mod_focus_20200318_friuli_ui("20200318_fvg")
      ),
      tabItem(
        tabName = "20200314Veneto",
        h1("Possible effect of the health policies implemented by the Veneto region"),
        eng_mod_focus_20200314_ui("dapb")
      ),

      tabItem(
        tabName = "national",
        h2("National events"),
        box(
          width = 12, title = "Instructions",
          p("Visualize/hide one or more measures on the graph by clicking on the items in the legend. Double click to only visualize the selected item."),
          p("Click on the autoscale button (the third one) to maximize the size of the graph."),
        ),
        eng_mod_ts_ita_ui("ts_nat_cum", title = "Time series trend - cumulative events"),
        eng_mod_ts_ita_ui("ts_nat_inc", title = "Time series trend - daily new events")
      ),


      tabItem(
        tabName = "regional",
        h2("Regional events"),
        box(
          width = 12, title = "Istructions",
          p("Add/remove computations for one or more region/measure by adding/removing it from the box."),
          p("NOTE: The number of tests per day can be visualized on the graph (by selecting the correct item in the legend) but it is hidden by default as it is off the scale (compared to the other measures)."),
          p(""),
          p("Visualize/hide one or more regions/measures on the graph by clicking on the items in the legend. Double click to only visualize the selected item."),
          p("Click on the autoscale button (the third one) to maximize the size of the graph."),
        ),

        h3("Time series by region"),
        fluidRow(
          box(
            title = "Cumulative events", width = 12,
            eng_mod_ts_reg_ui("ts_reg_cum_mes")
          )
        ),
        fluidRow(
          box(
            title = "Daily new events", width = 12,
            eng_mod_ts_reg_ui("ts_reg_inc_mes")
          )
        ),

        h3("Regional time series by event"),
        fluidRow(
          box(
            title = "Cumulative events", width = 12,
            eng_mod_ts_reg_ui("ts_reg_cum_reg")
          )
        ),

        fluidRow(
          box(
            title = "Daily new events", width = 12,
            eng_mod_ts_reg_ui("ts_reg_inc_reg")
          )
        )
      ),


      tabItem(
        tabName = "provincial",
        h2("Provincial events"),
        p("Add/remove computations for one or more region/measure by adding/removing it from the box."),
        p("Visualize/hide one or more regions on the graph by clicking on the items in the legend. Double click to only visualize the selected item."),
        p("Click on the autoscale button (the third one) to maximize the size of the graph."),

        h3("Time series"),
        fluidRow(
          box(
            title = "Cumulative events", width = 12,
            eng_mod_ts_prv_ui("ts_prv_cum")
          )
        ),

        fluidRow(
          box(
            title = "New events", width = 12,
            eng_mod_ts_prv_ui("ts_prv_inc")
          )
        )
      ),


      tabItem(
        tabName = "data_tab",
        h2("Data Info"),
        p("This app takes the needed data from the offical records that track the italian COVID-19 outbreak, on a national, regional and provincial level"),

        p(
          "Data are processed and made available by the ",
          a(href = "http://www.protezionecivile.it/web/guest/home", target = "_blank", "Presidenza del Consiglio dei Ministri - Dipartimento di Protezione Civile"),
          " (Italian Civil Protection Department) and licensed under ", a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
          "as provided by the ", a(href = "", target = "_blank", "Ministero della Salute"), "(Ministry of Health)."
        ),

        p("Data is usually updated daily at 6pm."),

        p(
          "Further information about data attribution and availability can be found at the",
          a(href = "https://github.com/UBESP-DCTV/covid19ita/", target = "_blank", "web page of covid19ita."),
          "."
        ),

        h2("Software"),
        p(
          HTML("The app <strong>covid19ita</strong> was developed using R ver. 3.6.3 as an expansion package. The source code of the package and the app is freely available online on Github at "),
          a(href = "https://github.com/UBESP-DCTV/covid19ita/", target = "_blank", "https://github.com/UBESP-DCTV/covid19ita/"), "."
        ),

        p("For the app development the following expansion packages were used: {shiny} ver. 1.4.0, {shinydashboard} v.0.7.1 and {golem} ver. 0.2.1."),

        p("The analyses were performed using functions of the following packages: {stats} ver. 3.6.3, and {gam} ver. 1.16.1"),

        p("Our graphs were produced with the following packages: {ggplot2} ver. 3.3.0 and {plotly} ver. 4.9.2."),

        h3("Note for R users"),
        p(
          "In addition to this app (that can be locally excecuted, after installing the package {covid19ita}, by running the code `run_app()`), the R package {covid19ita}, available on ",
          a(href = "https://github.com/UBESP-DCTV/covid19ita/", target = "_blank", "GitHub"),
          " and licenced under ",
          a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
          ", makes raw data available for direct use."
        )
      ),


      tabItem(
        tabName = "impact",
        h1("Principal indices"),
        eng_mod_ind_ita_ui("20200315")
      ),

      tabItem(
        tabName = "geo_spatialTot",
        # h1("Mappe 1"),
        eng_mod_maps_ui("geo_1")
      )
    )
  )
}





#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
eng_golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = " {covid19ita} ",
      meta = list(
        title = "{covid19ita} App",
        description = "Monitoring platform analysis of the COVID-19 infection in Italy.",
        url = "https://r-ubesp.dctv.unipd.it/shiny/covid19ita/",
        image = app_sys("app/www/ubep_covid.png"),
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
