#' Dashboard's HOME (sidebar)
#'
#' Helper function defining the sidebar of the dashboard's HOME page
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
eng_dashboard_home_sidebar <- function() {
  menuItem("Home", tabName = "home", icon = icon("home"))
}




#' Dashboard's HOME (body)
#'
#' Helper function defining the body of the dashboard's HOME page
#'
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @noRd
eng_dashboard_home_body <- function() {
  tabItem(
    tabName = "home",
    fluidPage(
      title = HTML("Project <strong>covid19ita</strong>"),

      p("Note: the website is optimized for computer viewing; in case of mobile or tablet viewing landscape mode is recommended."),

      # box(width = 12, solidHeader = TRUE,
      #   mod_img_header_ui("logo_testa")
      # ),

      box(
        width = 12,
        p(
          HTML("The project <strong>covid19ita</strong> was developed by the"),
          a(href = "https://ubesp.jimdofree.com/", target = "_blank", "Unit of Biostatistics, Epidemiology, and Public Health"),
          " of the", a(href = "https://www.dctv.unipd.it/", target = "_blank", "Department of Cardiac, Thoracic and Vascular Sciences and Public Health"),
          " at ", a(href = "https://www.unipd.it/", target = "_blank", "Universit\u00E0  degli Studi di Padova"), ",",
          " in partnership with the ", a(href = "https://www.dscb.unito.it/do/gruppi.pl/Tree", target = "_blank", "Department of Clinical and Biological Sciences"),
          " at ", a(href = "https://www.unito.it/", target = "_blank", "Universit\u00E0  degli Studi di Torino"), ",",
          " and the ", a(href = "https://www.dimet.uniupo.it/", target = "_blank", "Department of Translational Medicine"),
          " at ", a(href = "https://www.uniupo.it/", target = "_blank", "Universit\u00E0  del Piemonte Orientale"), "."
        )
      ),

      h2("Work group"),
      box(
        width = 12, title = HTML("<strong>Project coordinator</strong>"),
        p(
          HTML("Prof. <strong>Dario Gregori</strong>, PhD, Head of the
            Unit of Biostatistics, Epidemiology, and Public Health of the Department
            of Cardiac, Thoracic and Vascular Sciences and Public Health --
            Universit\u00E0  degli Studi di Padova."),
          a(href = "https://linkedin.com/in/dario-gregori-2720039/", target = "_blank", "LinkedIn")
        )
      ),

      box(
        width = 12, title = HTML("<strong>App and R package development</strong>"),
        p(
          HTML("<strong>Corrado Lanera</strong>, PhD, Unit of
          Biostatistics, Epidemiology, and Public Health of the Department
          of Cardiac, Thoracic and Vascular Sciences and Public Health --
          Universit\u00E0  degli Studi di Padova. Head of the Laboratory of Artificial Intelligence for Medical Sciences"),
          a(href = "https://linkedin.com/in/corradolanera/", target = "_blank", "LinkedIn")
        )
      ),


      box(
        width = 12, title = HTML("<strong>Epidemiological modeling</strong>"),
        p(
          HTML("Prof. <strong>Paola Berchialla</strong>, PhD, Department of Clinical
            and Biological Sciences -- Universit\u00E0  degli Studi di Torino"),
          a(href = "https://linkedin.com/in/paola-berchialla-36b44410/", target = "_blank", "LinkedIn")
        ),

        p(
          HTML(
            'Prof. <strong>Dolores Catelan</strong>, PhD, Department of
            Statistics, Computer Sciences, Applications "G. Parenti" (DISIA),
            Universit\u00E0  degli Studi di Firenze'
          ),
          a(
            href = "https://www.linkedin.com/in/dolores-catelan-43998b23/",
            target = "_blank", "LinkedIn"
          )
        )
      ),

      box(
        width = 12, title = HTML("<strong>Predictive models</strong>"),
        p(
          HTML("<strong>Danila Azzolina</strong>, PhD, Department of Translational Medicine --
          Universit\u00E0  del Piemonte Orientale"),
          a(href = "https://linkedin.com/in/danila-azzolina-862465166/", target = "_blank", "LinkedIn")
        ),

        p(
          HTML("<strong>Ilaria Prosepe</strong>, MSc, Unit of
           Biostatistics, Epidemiology, and Public Health of the Department
           of Cardiac, Thoracic and Vascular Sciences and Public Health --
           Universit\u00E0  degli Studi di Padova."),
          a(href = "https://linkedin.com/in/ilaria-prosepe-1b52371a4/", target = "_blank", "LinkedIn")
        )
      ),


      box(
        width = 12, title = HTML("<strong>Modeling in Environmental Epidemiology and Pollution</strong>"),
        p(
          HTML(
            'Prof. <strong>Annibale Biggeri</strong>, MD, MSPH, MSB,
              Department of Statistics, Computer Sciences, Applications
              "G. Parenti" (DISIA), Universit\u00E0  degli Studi di Firenze'
          ),
          a(href = "https://linkedin.com/in/danila-azzolina-862465166/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML(
            "Prof. <strong>Cristina Canova</strong>, PhD, Unit of
           Biostatistics, Epidemiology and Public Health of the Department
           of Cardiac, Thoracic and Vascular Sciences and Public Health --
           Universit\u00E0  degli studi di Padova."
          ),
          a(href = "https://www.linkedin.com/in/cristina-canova-05448861/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML("<strong>Elisa Gallo</strong>, MSc, Unit of
           Biostatistics, Epidemiology and Public Health of the Department
           of Cardiac, Thoracic and Vascular Sciences and Public Health --
           Universit\u00E0  degli studi di Padova."),
          a(href = "https://www.linkedin.com/in/elisa-gallo-9b3933152/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML("<strong>Francesco Garzotto</strong>, MSc, Unit of
           Biostatistics, Epidemiology and Public Health of the Department
           of Cardiac, Thoracic and Vascular Sciences and Public Health --
           Universit\u00E0  degli studi di Padova."),
          a(href = "https://www.linkedin.com/in/francesco-garzotto-19907826/", target = "_blank", "LinkedIn")
        )
      ),

      box(
        width = 12, title = HTML("<strong>Geospatial Analysis e Cartographic Representation</strong>"),
        p(
          HTML("Prof. <strong>Francesco Pirotti</strong>, PhD,
            Department of Territory and Agro-Forestry Systems (TESAF)
            CIRGEO - Interdepartmental Centre of Geomatics Research."),
          a(href = "https://www.linkedin.com/in/fpirotti", target = "_blank", "LinkedIn")
        )
      ),

      box(
        width = 12, title = HTML("<strong>Analysis of mortality</strong>"),
        p(
          HTML("Prof. <strong>Corrado Magnani</strong>, PhD,
               Department of Translational Medicine --
          Universit\u00E0  del Piemonte Orientale, Novara."),
          a(href = "https://www.linkedin.com/in/corrado-magnani-173a50108/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML("<strong>Daniela Ferrante</strong>, PhD,
               Department of Translational Medicine --
          Universit\u00E0  del Piemonte Orientale, Novara."),
          a(href = "https://www.linkedin.com/in/daniela-ferrante-05507494/", target = "_blank", "LinkedIn")
        )
      ),

      box(
        width = 12, title = HTML("<strong>Risk communication</strong>"),
        p(
          HTML("<strong>Giulia Lorenzoni</strong>, PhD, Unit of
            Biostatistics, Epidemiology, and Public Health of the Department
            of Cardiac, Thoracic and Vascular Sciences and Public Health --
            Universit\u00E0  degli Studi di Padova. Head of the Laboratory of Clinical Epidemiology and Digital Health"),
          a(href = "https://linkedin.com/in/giulia-lorenzoni-b382a6180/", target = "_blank", "LinkedIn")
        ),
        p(
          HTML(
            "<strong>Nicolas Destro</strong>, MA, Unit of
            Biostatistics, Epidemiology, and Public Health of the Department
            of Cardiac, Thoracic and Vascular Sciences and Public Health --
            Universit\u00E0  degli Studi di Padova."
          ),
          a(href = "https://www.linkedin.com/in/nicolas-destro-b2a67212b/", target = "_blank", "LinkedIn")
        )
      ),


      h2("Navigation instructions"),
      box(
        width = 12, title = HTML("<strong>Website structure</strong>"),
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

      box(
        width = 12, title = HTML("<strong>How to use the dynamic graphs</strong>"),
        p(HTML("The majority of graphs on this website are dynamic. The user can choose:")),
        p(""),
        p(HTML("1. <strong>the information he/she wants to visualize</strong>: while looking at one graph the user can <strong>visualize further details</strong> by clicking or hovering the mouse cursor over the different points/curves; <strong>zoom</strong> on some areas of interest by clicking on the semi-transparent buttons +/- at the top right of the graph (or by selecting the area with the mouse cursor). If multiple information is reported on the same graph (e.g. multiple regions o measures), it is possible to <strong>exclude part of the information</strong> by clicking on the right legend items or <strong>visualize only one piece of information</strong> by double-clicking on it. It is moreover possible to <strong>save each graph</strong> independently by clicking on the semi-transparent camera button. By clicking on the small-house button the <strong> original version</strong> of the graph is restored.")),
        p(HTML("2. <strong>which and how much information is to be processed and shown</strong>: Whenever cells appear above the graph, the user can decide to<strong> add or remove regions, provinces or metrics</strong> (from those available when the pane is selected). ")),
      ),
    )
  )
}
