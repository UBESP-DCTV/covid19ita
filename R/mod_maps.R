#' maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_maps_ui <- function(id){
  ns <- NS(id) # non cancellare

  linear<-function(x){ x }
  log10Per<-function(x){ log10(x+1) }
  logPer<-function(x){ log(x+1) }

  inv.funct_ <- list(
    linear=function(x){ x } ,
    log10Per=function(x){ round(10^(x)-1,0) } ,
    logPer=function(x){ round(exp(x)-1, 0) },
    sqrt=function(x){ x^2 }
  )

  paletteList.t<-list(

    Person=c("#cccccc",   "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb",
             "#41b6c4", "#1d91c0", "#225ea8",  "#6e016b", "#990000", "#d7301f", "#FF0000" ),
    Spectral=c("#cccccc",   rev( rainbow(20)[1:12])),
    YellowOrangeRed= RColorBrewer::brewer.pal(9,"YlOrRd"),
    RedYellowBlue= RColorBrewer::brewer.pal(11,"RdYlBu"),
    BlueYellowRed=rev(RColorBrewer::brewer.pal(11,"RdYlBu")),
    RedWhiteGrey= rev(RColorBrewer::brewer.pal(11,"RdGy"))
  )
  paletteList<- (names(paletteList.t))

  functionList<- list( "Linear"="linear",
                       "Log10"="log10Per",
                       #"Ln"= "logPer",
                       "Sqrt"= "sqrt"  )

  functionList.lut <- names(functionList)
  names(functionList.lut) <- functionList

  tooltips<-list(scale.fixed="<b style='font-weight:bold; color:red;'>
           Fixed Scale</b><br>If checked, this box will keep the scale constant to the range of the min and max of all values accross the time-span of analysis of whatever observation is plotted.
           If unchecked, the scale will have min and max values of the current day, and therefore change every time a different day is chosen.
           The former allows comparing accross time, but has lower color contrast as range will be larger.
           The latter allows more color contrast over small variations. ",
                 calculus=" <b style='color:red;'>Total Cases</b>: total count of COVID-19 cases at selected day.<br><br>
    <b style='color:red;'>Daily Increase:</b> Difference of cases with previous day.<br><br>
    <b style='color:red;'>Normalized daily Increase (not implemented)</b>: difference between chosen day's number of COVID-19 cases (T1) and previous day (T0), over the sum, i.e.
    (T0 - T1)/(T0 + T1) - ranges from -1 to 1, 0 means flat increase - this scale highlights initial cases and tends to 0 when new cases are a
               small percentage of total cases.",
                 scale.residents="Will normalize per residents in the area and scaled x1000 (i.e. number every 1000 residents).",
                 scale.funct_="Scales can be linear or transformed to lognormal, log base 10 or square root to improve visualization of asymmetric (skewed) frequency distributions."
  )
  ## Da qui in poi inserire il contenuto lato UI del modulo, in
  ## particolare la definizione degli input (ricordarsi di inserire i
  ## relativi id all'interno di una chiamata a `ns(<input_id>)`)
  fluidPage(
    ## sfruttare i `box()` per quanto possibile, ed eventuali
    ## `fludRow()`

    tags$head(
      tags$style(type="text/css",
                 sprintf("
      .datepicker { z-index:999999  !important; }
      .selectize-dropdown{   z-index:99999999 !important;  }
      #%s-mymap {
        height:calc(100vh - 200px) !important;
        min-height:600px;
        background: #333;
        border: 1px solid black;
        border-radius: 8px;
      }
      #%s-date1 input.form-control  {
        color: red !important;
        font-size: larger;
        margin: 0;
        background: darkgray;
        padding: 15px;
      }", id, id))
    ),
    fluidRow(
      column(3, style="font-weight:bold;",
             dateInput(ns("date1"), NULL, value = max(dpc_covid19_ita_regioni$data), format = 'DD dd MM yyyy') ),
      column(3, div(  selectInput(ns("calculus"), NULL,
                                  choices = list(
                                    "Total cases"="totale_casi",
                                    "Daily cases"="delta",
                                    "Total cases / 10 000 residents"="totale_casi.normPop",
                                    "Daily cases / 10 000 residents"="delta.normPop" ) )
      )
      ),

      column(2,
             div(style="display: inline-block; margin-top: 12px; width:50px; ",
                 `data-toggle` ="tooltip" , `data-html` ="true" ,   `data-placement`="bottom" , title=tooltips$scale.fixed,
                shinyWidgets::prettyCheckbox(
                   inputId = ns("scale.fixed"), label = "FixScale", icon = icon("check"),
                   animation = "pulse", plain = TRUE, outline = TRUE ) )
      ),
      column(2, title="Scales",  `data-toggle` ="tooltip" ,  `data-placement`="bottom" ,
             selectInput(ns("scale.funct_"), NULL,  choices = functionList ) ),
      column(2, title="Color Palette",   `data-toggle` ="tooltip" ,  `data-placement`="bottom" ,
             selectInput(ns("palette"), NULL,  choices = paletteList, selectize = T ) )
    ),
    fluidRow(
      box(  leaflet::leafletOutput(ns("mymap")) ,
        title = "Andamento dei casi nel tempo per regione",
        footer = "CIRGEO Centro Interdipartimentale di Ricerca di Geomatica",
        width = 12
      )
    )
  )
}

#' maps Server Function
#'
#' @noRd
mod_maps_server <- function(id) {

  ## Zona dedicata alle computazioni preliminari, non reattive
  ## qui se possibile metterli su un "global.R" dato che sono identici per tutti
  basic.layerlist.list<-list(    baseGroups = list( osm.bn="Map Night",osm.light ="Map Light",
                                                    osm="None" ),
                                 overlayGroups = list( Casi_COVID19="COVID-19",
                                                       Casi_COVID19labels="Labels")
  )

  basic.layerlist<-list(
    baseGroups = unname(unlist(basic.layerlist.list$baseGroups )),
    overlayGroups = unname(unlist(basic.layerlist.list$overlayGroups ))
  )

  myLeaflet<-leaflet::leaflet(width="100%", height = 600) %>%

    htmlwidgets::onRender("function(el, x) {
           mapElement=this;
           //this.on('layeradd', onLayerAddFunction);
           }") %>%
    leaflet::flyTo( 11, 43, 6)  %>%
    leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                      attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
                      options =  leaflet::tileOptions(zIndex = 1, preferCanvas=TRUE, maxZoom = 19, subdomains = "abcd" ), group=basic.layerlist.list$baseGroups$osm.bn)  %>%
    leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                      attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
                      options =  leaflet::tileOptions(zIndex = 2, preferCanvas=TRUE, maxZoom = 19, subdomains = "abcd" ), group=basic.layerlist.list$baseGroups$osm.light)  %>%


    #  hideGroup( as.character(basic.layerlist.list$overlayGroups) ) %>%
    leaflet::showGroup( c(basic.layerlist.list$overlayGroups$Casi_COVID19 ) )   %>%
    # addMouseCoordinates() %>%
    leaflet::addLayersControl(baseGroups    =  basic.layerlist$baseGroups,
                              overlayGroups = basic.layerlist$overlayGroups,
                              options =  leaflet::layersControlOptions(collapsed = F) )


  data_to_use <- dpc_covid19_ita_province %>%
    dplyr::group_by(codice_provincia) %>%
    dplyr::mutate(  delta     =c(0, diff(totale_casi)) ) %>%
    dplyr::select(
      .data$data, .data$denominazione_provincia , .data$totale_casi, .data$delta, .data$lat, .data$long
    )

  data_to_use <- merge(data_to_use, province_population2019)


  data_to_use$totale_casi.normPop<-data_to_use$totale_casi/data_to_use$Residenti * 10000
  data_to_use$delta.normPop<-data_to_use$delta/data_to_use$Residenti * 10000

  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    ## zona dedicata alle computazioni reattive del modulo, in
    ## particolare la definizione degli output (ricordarsi che tali nomi
    ## NON vanno inseriti (a differenza della controparte in input)
    ## all'interno della chiamata a `ns()`)
    output$foo <- renderText("Bar.")

    output$mymap <- leaflet::renderLeaflet(  myLeaflet )

    observe(  {
      req(input[["date1"]])


      dt.filtered<- data_to_use %>%
        dplyr::filter( as.Date(.data$data) == input[["date1"]])


      if( nrow(dt.filtered)<1 )
      {
        showNotification("No data found for selected day.",  type ="warning")
        return(NULL)
      }
      dt.label<- unname(unlist(dt.filtered[,input$calculus]))
      if( !is.element(input$calculus,c("delta", "totale_casi") ) )  label<-sprintf("%.2f",dt.label)
      else label <- sprintf("%d",   dt.label)

      label[label=="NA"]<-""

      leaflet::leafletProxy("mymap") %>%

        # leaflet::addWMSTiles(baseUrl = sprintf("https://geolab02.vs-ix.net/cgi-bin/mapserv?uselessCache=%s&map=/archivio/R/shared/covid19carto/data/mapserver/mapfile.map",
        #                               input$giorno,
        #                               #input$scale.fixed, input$scale.funct_, input$calculus,
        #                               input$palette  ) ,
        #             options = leaflet::WMSTileOptions(zIndex = 4, format = "image/png", transparent = T ,
        #                                      layerId = sprintf("%s_%s", basic.layerlist.list$overlayGroups$Casi_COVID19, input$giorno)  ),
        #             layers="Casi_COVID19",
        #             layerId = sprintf("%s_%s", basic.layerlist.list$overlayGroups$Casi_COVID19, input$giorno),
        #             group=basic.layerlist.list$overlayGroups$Casi_COVID19,
        #             attribution = "Provincie ISTAT @CIRGEO" )  %>%

        leaflet::addLabelOnlyMarkers(data = dt.filtered, lng = ~long, lat=~lat,
                            layerId = sprintf("%s%s", basic.layerlist.list$overlayGroups$Casi_COVID19labels,
                                              dt.filtered$codice_provincia    ),
                            group =  basic.layerlist.list$overlayGroups$Casi_COVID19labels,

                            label = label ,
                             labelOptions = leaflet::labelOptions(zIndex = 10,
                                                                 noHide = TRUE,
                                                                 # direction = "bottom",
                                                                 textOnly = T,
                                                                 style= list(
                                                                   "font-size" = "11px",
                                                                   "font-weight" = "bold",
                                                                   "color"="white",
                                                                   "text-shadow"="0px 0px 3px black"
                                                                 ),
                                                                 #offset = c(0, -10),
                                                                 opacity = 1
                            )
        )


      })

  })

}

## To be copied in the UI
# mod_maps_ui("maps_1")

## To be copied in the server
# mod_maps_server("maps_1")

