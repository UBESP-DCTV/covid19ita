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

  ## Da qui in poi inserire il contenuto lato UI del modulo, in
  ## particolare la definizione degli input (ricordarsi di inserire i
  ## relativi id all'interno di una chiamata a `ns(<input_id>)`)
  fluidPage(
    ## sfruttare i `box()` per quanto possibile, ed eventuali
    ## `fludRow()`
    fluidRow(
      box(textOutput(ns("foo")),
        title = "Testo generato dal server",
        width = 3 # integer, max 12(/12) = full width of the page body
      ),
      box(
        sliderInput(ns("n_min"),
          label = "Numero minimo di casi da considerare",
          min = 0L,
          max = max(dpc_covid19_ita_regioni[["totale_casi"]],
            na.rm = TRUE
          ),
          value = 0L,
          step = 1L
        ),
        width = 9
      )
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
           this.on('layeradd', onLayerAddFunction);
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

  data_to_use <- dpc_covid19_ita_regioni %>%
    dplyr::select(
      .data$data, .data$totale_casi, .data$denominazione_regione
    )

  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    ## zona dedicata alle computazioni reattive del modulo, in
    ## particolare la definizione degli output (ricordarsi che tali nomi
    ## NON vanno inseriti (a differenza della controparte in input)
    ## all'interno della chiamata a `ns()`)
    output$foo <- renderText("Bar.")

    current_data <- reactive({
      req(input[["n_min"]])

      data_to_use %>%
        dplyr::filter(.data$totale_casi >= input[["n_min"]])

    })

    output$mymap <- leaflet::renderLeaflet({
      #req(input[["n_min"]])
      myLeaflet

    })
  })

}

## To be copied in the UI
# mod_maps_ui("maps_1")

## To be copied in the server
# mod_maps_server("maps_1")

