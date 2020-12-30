#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {








  ############
  ## LOGIN #############################################################
  ############
  # login <- FALSE
  USER <- reactiveValues(login = TRUE)
  usr_pos <- which(super_secret()$username == "")
  updateTabItems(session, "sidebar", "home")


  observeEvent(input$login, {
    if (!USER$login && !is.null(input$login) && input$login > 0) {
      Username <- input$userName
      Password <- input$passwd


      usr_pos <<- which(super_secret()$username == Username)

      if (length(usr_pos) == 1) {
        pasmatch  <- super_secret()[["password"]][usr_pos]
        pasverify <- sodium::password_verify(pasmatch, Password)

        if (pasverify) {
          USER$login <- TRUE
          if (Username != "") {
            showNotification(
              HTML("
                Setup zone riservate</br>serve un po' di tempo in pi\u00F9...
              "),
              duration = 9, type = "warning"
            )
          }
          updateTabItems(session, "sidebar", "home")
          showNotification(
            HTML("
                Grazie per la pazienza!
              "),
            duration = 18, type = "warning"
          )
        } else {
          nomatch()
        }
      } else {
        nomatch()
      }
    } else {
      updateTabItems(session, "sidebar", "home")
    }
  })





  #############
  ## LOGOUT ############################################################
  #############
  observeEvent(input$logout, {
    USER$login <- FALSE
  })







  #############
  ## SERVER ############################################################
  #############
  observe({
    req(USER$login)

    ## Header info
    mod_img_header_server("logo_testa", "Covid19.png")
    mod_img_header_server("logo_coda_torino", "Torino.png")
    mod_img_header_server("logo_coda_novara", "Novara.png")
    mod_img_header_server("logo_coda", "Covid19.png")
    mod_info_sidebar_server("summary_today")

    ## Impact
    mod_ind_ita_server("20200315")

    ## plottply help
    mod_help_plot_server("help")

    ## ICUs VE

    if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "tip-v")) {
      mod_icuve_sitrep_server("icuve_sitrep")
      mod_icuve_ts_server("icuve_ts")
      mod_icuve_static_server("icuve_static")
    }

    if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "tip-v", "agenas")) {
      mod_tsicuve_server("partial_ts_icuve")
      mod_reg_tsicuve_server("regional_partial_tsicuve")
      mod_reg_nocritica_server("regional_partial_nocritica")
    }

    ## National
    mod_ts_ita_server("ts_nat_cum", "cum")
    mod_ts_ita_server("ts_nat_inc", "inc")

    # ## Regional
    mod_ts_reg_server("ts_reg_cum_mes", "cum", color_var = "measure")
    mod_ts_reg_server("ts_reg_inc_mes", "inc", color_var = "measure")
    mod_ts_reg_server("ts_reg_cum_reg", "cum", color_var = "region")
    mod_ts_reg_server("ts_reg_inc_reg", "inc", color_var = "region")

    ## Provincial
    mod_ts_prv_server("ts_prv_cum", "cum")
    mod_ts_prv_server("ts_prv_inc", "inc")


    ## In evidenza
    mod_0314_server("dapb")
    mod_0318_friuli_server("20200318_fvg")
    mod_0318_piemonte_server("20200318_piemonte")
    mod_0318_intensive_server("21")
    mod_0320_novara_server("da_novara")
    mod_0320_novara_server("da_vercelli",
                           loc = "Vercelli",
                           pop = 174904
    )
    mod_0320_novara_server("da_alessandria",
                           loc = "Alessandria",
                           pop = 428826
    )
    mod_0323_picco_server("picco")
    mod_0325_hosp_server("hosp")
    mod_0328_hosp_server("tot")
    mod_0331_server("ven_pie")
    mod_0404_magnani_server("mortality")
    mod_0406_mort_veneto_server("mort_veneto")
    mod_0415_tamponi_server("tamp_hosp")

    ## Geo-spatial
    mod_maps_server("geo_1")

  })













  #################
  ## UI SIDEBAR ########################################################
  #################

  output$sidebarpanel <- renderUI({        # called into app_server.R
    if (USER$login) {
      sidebarMenu(
        id = "sidebar",

        mod_help_plot_ui("help"),




        dashboard_home_sidebar(),




        menuItem("Andamento epidemia",
                 icon = icon("chart-line"),
                 menuSubItem("Nazionale", tabName = "national",
                             icon = icon("flag")),
                 menuSubItem("Regionale", tabName = "regional",
                             icon = icon("map")),
                 menuSubItem("Provinciale", tabName = "provincial",
                             icon = icon("location-arrow"))
        ),




        if (super_secret()[["permission"]][usr_pos] %in%
            c("ubep", "tip-v", "agenas")) {

          menuItem("Terapie intensive",
                   icon = icon("procedures"),
                   menuSubItem("Serie TI (reg)",
                               tabName = "regional-partial-ts-icuve",
                               icon = icon("map")
                   ),
                   menuSubItem("Serie area non critica (reg)",
                               tabName = "regional-partial-ts-nocritica",
                               icon = icon("map")
                   ),
                   if (super_secret()[["permission"]][usr_pos] !=
                       "agenas") {
                     menuSubItem("Veneto ICUs situation report",
                                 tabName = "regional-icuve-sitrep",
                                 icon = icon("map")
                     )},

                   if (super_secret()[["permission"]][usr_pos] !=
                       "agenas") {
                     menuSubItem("Veneto ICUs timeseries",
                                 tabName = "regional-icuve-ts",
                                 icon = icon("map")
                     )},

                   if (super_secret()[["permission"]][usr_pos] !=
                       "agenas") {
                     menuSubItem("Veneto ICUs overview",
                                 tabName = "regional-icuve-static",
                                 icon = icon("map")
                     )}
          )
        },




        menuItem("Analisi tematiche",
                 icon = icon("bullseye"),

                 if (super_secret()[["permission"]][usr_pos] %in%
                     c("ubep", "tip-v", "agenas")) {

                   menuSubItem("Veneto partial timeseries",
                               tabName = "partial-ts-icuve",
                               icon = icon("map")
                   )
                 }
        ),




        menuItem("Indici principali", tabName = "impact",
                 icon = icon("compass")),




        menuItem("Mappe", tabName = "geo_spatialTot",
                 icon = icon("map-marked-alt")),








        menuItem("Archivio analisi",
                 icon = icon("bullseye"),

                 if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "tip-v", "agenas")) {

                   menuSubItem("Veneto partial timeseries",
                               tabName = "partial-ts-icuve",
                               icon = icon("map")
                   )
                 },


                 menuSubItem(
                   text = "2020-04-15 Impatto tamponi",
                   tabName = "20200415TampHosp",
                   icon = icon("flag")
                 ),

                 menuSubItem("2020-04-06 Mortalit\u00E0  Veneto",
                             tabName = "20200406MortVeneto",
                             icon = icon("flag")
                 ),
                 menuSubItem("2020-04-05 Mortalit\u00E0",
                             tabName = "20200405Mort",
                             icon = icon("flag")
                 ),
                 menuSubItem("2020-03-31 Comparativa",
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
                 menuSubItem("2020-03-24 Metodologia",
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








        menuItem("Segnalazioni",
                 icon = icon("exclamation-triangle"),
                 href = "https://github.com/UBESP-DCTV/covid19ita/issues/"
        ),




        menuItem("Fonti e informazioni", tabName = "data_tab",
                 icon = icon("database")),




        mod_info_sidebar_ui("summary_today")
      )
    }
  })








  ##############
  ## UI BODY ###########################################################
  ##############
  output$body <- renderUI({            # called in app_server.R
      if (USER$login) {
        tabItems(
          dashboard_home_body(),

          #
          # # Focus ====================================================
          # ## Reserved ------------------------------------------------
          #
          tabItem(
            tabName = "partial-ts-icuve",
            h2("Progressione delle proiezioni e andamento dell'errore di stima per le terapie intensive venete dall'inizio della pandemia."),
            if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "agenas", "tip-v")) {
              mod_tsicuve_ui("partial_ts_icuve")
            }
          ),

          #
          # # Public --------------------------------------------------
          #
          tabItem(
            tabName = "20200415TampHosp",
            h2("Impatto dei tamponi sulle ospedalizzazioni"),
            mod_0415_tamponi_ui("tamp_hosp")
          ),
          tabItem(
            tabName = "20200406MortVeneto",
            h2("Analisi preliminare della mortalit\u00E0  generale in 122 comuni veneti durante il periodo da 1 a 21 Marzo 2020."),
            mod_0406_mort_veneto_ui("mort_veneto")
          ),
          tabItem(
            tabName = "20200405Mort",
            h2("Analisi preliminare della mortalit\u00E0  generale in 1084 comuni italiani durante il periodo da 1 a 21 Marzo 2020."),
            mod_0404_magnani_ui("mortality")
          ),
          tabItem(
            tabName = "20200331Comp",
            h2("Analisi comparativa tra Regione Piemonte e Regione Veneto dei dati epidemiologici relativi all'infezione da Covid-19."),
            mod_0331_ui("ven_pie")
          ),
          tabItem(
            tabName = "20200328Tot_hosp",
            h2("Possibile effetto sulle ospedalizzazioni delle politiche sanitarie in Veneto"),
            mod_0328_hosp_ui("tot")
          ),
          tabItem(
            tabName = "20200325Hosp",
            h2("Possibile effetto sulle ospedalizzazioni delle politiche sanitarie in Veneto"),
            mod_0325_hosp_ui("hosp")
          ),
          tabItem(
            tabName = "20200323Picco",
            h2("Impatto dell'incertezza statistica sulle previsioni dell'andamento del COVID-19"),
            mod_0323_picco_ui("picco")
          ),
          tabItem(
            tabName = "20200321Alessandria",
            h2("Stime previsive sul totale casi ad Alessandria"),
            mod_0320_novara_ui("da_alessandria")
          ),
          tabItem(
            tabName = "20200321Vercelli",
            h2("Stime previsive sul totale casi a Vercelli"),
            mod_0320_novara_ui("da_vercelli")
          ),
          tabItem(
            tabName = "20200320Novara",
            h2("Stime previsive sul totale casi a Novara"),
            mod_0320_novara_ui("da_novara")
          ),
          tabItem(
            tabName = "20200319Veneto",
            h2("Possibile impatto delle politiche sanitarie sull'occupazione dei posti letto nelle terapie intensive in Veneto"),
            mod_0318_intensive_ui("21")
          ),
          tabItem(
            tabName = "20200318Piemonte",
            h2("Possibile effetto delle politiche sanitarie in Piemonte"),
            mod_0318_piemonte_ui("20200318_piemonte")
          ),
          tabItem(
            tabName = "20200318Fvg",
            h2("Possibile effetto delle politiche sanitarie in Friuli Venezia Giulia"),
            mod_0318_friuli_ui("20200318_fvg")
          ),
          tabItem(
            tabName = "20200314Veneto",
            h2("Possibile effetto delle politiche sanitarie in Veneto"),
            mod_0314_ui("dapb")
          ),

          #
          # # ICU ======================================================
          #
          tabItem(
            tabName = "regional-icuve-sitrep",
            h2("Report situazione corrente nelle terapie intensive venete a livello regionale."),
            if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "tip-v")) {
              mod_icuve_sitrep_ui("icuve_sitrep")
            }
          ),
          tabItem(
            tabName = "regional-icuve-ts",
            h2("Andamenti e proiezioni sui posti letto nelle terapie intensive venete a livello regionale."),
            if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "tip-v")) {
              mod_icuve_ts_ui("icuve_ts")
            }
          ),
          tabItem(
            tabName = "regional-icuve-static",
            h2("Andamenti delle terapie intensive venete dall'inizio della pandemia."),
            if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "tip-v")) {
              mod_icuve_static_ui("icuve_static")
            }
          ),
          tabItem(
            tabName = "regional-partial-ts-icuve",
            h2("Andamenti delle terapie intensive regionali dall'inizio della pandemia."),
            if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "tip-v", "agenas")) {
              mod_reg_tsicuve_ui("regional_partial_tsicuve")
            }
          ),
          tabItem(
            tabName = "regional-partial-ts-nocritica",
            h2("Andamenti delle aree non critiche regionali dall'inizio della pandemia."),
            if (super_secret()[["permission"]][usr_pos] %in% c("ubep", "tip-v", "agenas")) {
              mod_reg_nocritica_ui("regional_partial_nocritica")
            }
          ),

          #
          # # DPC data =================================================
          # ## National ------------------------------------------------
          #
          tabItem(
            tabName = "national",
            h2("Eventi nazionali"),
            box(
              width = 12, title = "Istruzioni",
              p("\u00C8  possibile attivare/disattivare la visualizzazione di una o pi\u00F9  misure dal grafico facendo click sulle corrispondenti voci in legenda. Doppio-click per attivare/disattivare la visualizzazione esclusiva della voce selezionata."),
              p("Fare click sul pulsante autoscale (terzo) per espandere il grafico alla massima grandezza interamente visionabile."),
            ),
            mod_ts_ita_ui("ts_nat_cum", title = "Serie storiche degli eventi cumulati"),
            mod_ts_ita_ui("ts_nat_inc", title = "Serie storiche dei nuovi eventi giornalieri")
          ),


          #
          # ## Regional ------------------------------------------------
          #
          tabItem(
            tabName = "regional",
            h2("Eventi regionali"),
            box(
              width = 12, title = "Istruzioni",
              p("\u00C8  possibile aggiungere o rimuovere la computazione dei grafici per una o pi\u00F9  regione/misura selezionandola o deselezionandola dal corrispondente box."),
              p("NOTA: la misurazione dei tamponi effettuati \u00E8  selezionabile ma disattivata di default in quanto fuori scala rispetto alle altre misure."),
              p(""),
              p("\u00C8  possibile attivare/disattivare la visualizzazione di una o pi\u00F9  regioni/misure dal grafico facendo click sulle corrispondenti voci in legenda. Doppio-click per attivare/disattivare la visualizzazione esclusiva della voce selezionata."),
              p("Fare click sul pulsante autoscale (terzo) per espandere il grafico alla massima grandezza interamente visionabile."),
            ),


            h3("Serie storiche regionali per misura"),
            fluidRow(
              box(
                title = "Eventi cumulati", width = 12,
                mod_ts_reg_ui("ts_reg_cum_reg")
              )
            ),

            fluidRow(
              box(
                title = "Nuovi eventi giornalieri", width = 12,
                mod_ts_reg_ui("ts_reg_inc_reg")
              )
            ),


            h3("Serie storiche per regione"),
            fluidRow(
              box(
                title = "Eventi cumulati", width = 12,
                mod_ts_reg_ui("ts_reg_cum_mes")
              )
            ),

            fluidRow(
              box(
                title = "Nuovi eventi giornalieri", width = 12,
                mod_ts_reg_ui("ts_reg_inc_mes")
              )
            )
          ),


          #
          # ## Provincial ----------------------------------------------
          #
          tabItem(
            tabName = "provincial",
            h2("Eventi provinciali"),
            p("\u00C8  possibile aggiungere o rimuovere la computazione dei grafici per una o pi\u00F9  regione selezionandola o deselezionandola dal corrispondente box."),
            p("\u00C8  possibile attivare/disattivare la visualizzazione di una o pi\u00F9  regioni dal grafico facendo click sulle corrispondenti voci in legenda. Doppio-click per attivare/disattivare la visualizzazione esclusiva della voce selezionata."),
            p("Fare click sul pulsante autoscale (terzo) per espandere il grafico alla massima grandezza interamente visionabile."),

            h3("Serie storiche"),
            fluidRow(
              box(
                title = "Eventi cumulati", width = 12,
                mod_ts_prv_ui("ts_prv_cum")
              )
            ),

            fluidRow(
              box(
                title = "Nuovi eventi", width = 12,
                mod_ts_prv_ui("ts_prv_inc")
              )
            )
          ),


          #
          # # General info =============================================
          #
          tabItem(
            tabName = "data_tab",
            h2("Informazioni sui dati"),

            p("L'applicazione utilizza i dati ufficiali riguardanti la situazione COVID-19 italiana, a livello nazionale, regionale e provinciale."),

            p(
              "Tali dati sono inizialmente gestiti, processati e messi a disposizione dalla ",
              a(href = "http://www.protezionecivile.it/web/guest/home", target = "_blank", "Presidenza del Consiglio dei Ministri - Dipartimento di Protezione Civile"),
              " con licenza ", a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
              "cos\u00EC  come forniti dal ", a(href = "", target = "_blank", "Ministero della Salute.")
            ),

            p("Di norma, i dati sono aggiornati alle ore 18:00 di ogni giorno."),

            p(
              "Per ulteriori informazioni sulla loro attribuzione, disponibilita, e uso, si consiglia di visitare la",
              a(href = "https://github.com/UBESP-DCTV/covid19ita/", target = "_blank", "pagina del progetto covid19ita"),
              "."
            ),

            h2("Software"),
            p(
              HTML("L'applicazione <strong>covid19ita</strong> \u00E8  stata sviluppata in R ver. 3.6.3 come un suo pacchetto di espansione. Il codice sorgente del pacchetto e dell'applicazione\u00E8 ? liberamente disponibile disponibile su GitHub all'indirizzo "),
              a(href = "https://github.com/UBESP-DCTV/covid19ita/", target = "_blank", "https://github.com/UBESP-DCTV/covid19ita/"), "."
            ),

            p("Per il suo sviluppo sono stati utilizzati i pacchetti di espansione {shiny} ver. 1.4.0, {shinydashboard} v.0.7.1 e {golem} ver. 0.2.1."),

            p("Le analisi sono state eseguite sfruttando le funzioni dei pacchetti {stats} ver. 3.6.3, {gam} ver. 1.16.1, e {mgcv} ver. 1.8-31"),

            p("I grafici sono stati prodotti grazie ai pacchetti {ggplot2} ver. 3.3.0 e {plotly} ver. 4.9.2."),

            h3("Nota per gli utilizzatori di R"),
            p(
              "Oltre a questa stessa applicazione in s\u00E9  (che pu\u00F2  essere eseguita localmente installando il pacchetto {covid19ita} ed eseguendo l'istruzione `run_app()`), ll pacchetto R {covid19ita}, disponibile su ",
              a(href = "https://github.com/UBESP-DCTV/covid19ita/", target = "_blank", "GitHub"),
              " e sviluppato anch'esso sotto licenza ",
              a(href = "https://creativecommons.org/licenses/by/4.0/deed.en", target = "_blank", "CC-BY-4.0"),
              ", mette a disposizione tali dati, senza alcun processamento ulteriore rispetto a quelli originali, per un loro diretto utilizzo in R."
            )
          ),


          #
          # # Impact ===================================================
          #
          tabItem(
            tabName = "impact",
            h1("Indici principali"),
            mod_ind_ita_ui("20200315")
          ),

          #
          # # GEO-Spatial ==============================================
          #
          tabItem(
            tabName = "geo_spatialTot",
            # h1("Mappe 1"),
            mod_maps_ui("geo_1")
          )
        )
      } else login_page()
    })

}
