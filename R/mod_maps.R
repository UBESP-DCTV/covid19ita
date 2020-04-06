#' maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'
mod_maps_ui <- function(id){
  ns <- NS(id) # non cancellare

  loader<-HTML(
    '<svg style="width: 10%;height: 10%; min-width: 50px;min-height: 50px; margin-top: 80px;" width="38" height="38" viewBox="0 0 38 38" xmlns="http://www.w3.org/2000/svg">
    <defs>
        <linearGradient x1="8.042%" y1="0%" x2="65.682%" y2="23.865%" id="a">
            <stop stop-color="#fff" stop-opacity="0" offset="0%"/>
            <stop stop-color="#fff" stop-opacity=".631" offset="63.146%"/>
            <stop stop-color="#fff" offset="100%"/>
        </linearGradient>
    </defs>
    <g fill="none" fill-rule="evenodd">
        <g transform="translate(1 1)">
            <path d="M36 18c0-9.94-8.06-18-18-18" id="Oval-2" stroke="url(#a)" stroke-width="2">
                <animateTransform attributeName="transform" type="rotate" from="0 18 18" to="360 18 18" dur="0.9s" repeatCount="indefinite"/>
            </path>
            <circle fill="#fff" cx="36" cy="18" r="1">
                <animateTransform attributeName="transform" type="rotate" from="0 18 18" to="360 18 18" dur="0.9s" repeatCount="indefinite"/>
            </circle>
        </g>
    </g>
</svg>'
  )
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
    Spectral=c("#cccccc",   rev( grDevices::rainbow(20)[1:12])),
    YellowOrangeRed= RColorBrewer::brewer.pal(9,"YlOrRd"),
    RedYellowBlue= RColorBrewer::brewer.pal(11,"RdYlBu"),
    BlueYellowRed=rev(RColorBrewer::brewer.pal(11,"RdYlBu")),
    RedWhiteGrey= rev(RColorBrewer::brewer.pal(11,"RdGy"))
  )
  paletteList<- (names(paletteList.t))

  paletteList.img<-c()
  for( pal in paletteList.t){
    png(tf1 <- tempfile(fileext = ".png"), width = 160, height=20)
    op <- par(mar = rep(0, 4))
    image(1:length(pal), 1, as.matrix(1:length(pal)),
          col = pal,
          xlab = "", ylab = "", xaxt = "n", yaxt = "n",   bty = "n"  )
    dev.off()

    txt <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
    paletteList.img<-c(paletteList.img,  sprintf('<img src="data:image/png;base64,%s">', txt))

    # cat(html, file = tf2 <- tempfile(fileext = ".html"))
    # browseURL(tf2)
  }
  paletteList<- (names(paletteList.t))

  functionList<- list( "Linear"="linear", "Log10"="log10Per" )
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
                 scale.residents="Will normalize per residents in the area and scale x1000 (i.e. number every 1000 residents).",
                 scale.funct_="Scales can be linear or transformed to lognormal to improve visualization of asymmetric (skewed) frequency distributions."
  )
  ## Da qui in poi inserire il contenuto lato UI del modulo, in
  ## particolare la definizione degli input (ricordarsi di inserire i
  ## relativi id all'interno di una chiamata a `ns(<input_id>)`)
  fluidPage(
    ## sfruttare i `box()` per quanto possibile, ed eventuali
    ## `fludRow()`
    shinyjs::useShinyjs(),
    tags$head(
    #  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/rangeslider.js/2.3.3/rangeslider.min.js"),
      tags$style(type="text/css",
                 sprintf("
      .datepicker { z-index:999999999999  !important; }
      .selectize-dropdown{   z-index:9999999999 !important;  }
      .dropdown-menu{   z-index:9999999999 !important;  }
      #shiny-notification-panel{ width:100%% !important; height:200px;}
      #%s-mymap {
        height:calc(100vh - 200px) !important;
        min-height:500px;
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

      #   column(1,  `data-toggle` ="tooltip" , `data-html` ="true" ,   `data-placement`="bottom" , title=tooltips$scale.fixed,
      #           checkboxInput("scale.fixed", "F.S.", value = F )),
      column(2,
             div(style="display: inline-block; margin-top: 12px; width:50px; ",
                 `data-toggle` ="tooltip" , `data-html` ="true" ,   `data-placement`="bottom" , title=tooltips$scale.fixed,
                 checkboxInput( ns("scale.fixed"), label = "FixScale", value=T ) )
      ),
      column(2, title="Scales",  `data-toggle` ="tooltip" ,  `data-placement`="bottom" ,
             selectInput(ns("scale.funct_"), NULL,  choices = functionList ) ),
      column(2, title="Color Palette",   `data-toggle` ="tooltip" ,  `data-placement`="bottom" ,
             shinyWidgets::pickerInput(ns("palette"), NULL,
                                       choices = paletteList,
                                       choicesOpt = list(content = paletteList.img
                                       ) ) )
    ),
    fluidRow(
      box(  div( id=ns("loader"), alt="",
                 style="width:100%; position:absolute; text-align: center; z-index:9999999999;",
                 loader,
                 HTML("<font style='display: block; color:white;text-shadow:0px 0px 3px black;'>Loading Geodata...</font>" )),
            HTML(sprintf("<input style='width:100%%;'  type='range' id='%s' />",   ns("dateRange") ) ),
            leaflet::leafletOutput(ns("mymap")) ,
        title = "Distribuzione geografica del numero di casi per provincia",
        footer = HTML("<div style='width:100% ; text-align:center; font-size:smaller;'>by F. Pirotti,  Dip.to TESAF /
                      <a href='https://www.cirgeo.unipd.it' target=_blank>
                      CIRGEO Centro Interdipartimentale di Ricerca di Geomatica</a></div>"),
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

  base.url<-"https://geolab02.vs-ix.net"
  remote.path<-"/var/www/html/covid19carto/"
  remote.base.url<-sprintf("%s/covid19carto/" , base.url)
  remote.mapfile.creator<-sprintf("%s/creamapfile.php" , remote.base.url)
  ### un mapfile per server WMS identificato da 5 variabili:
  ### 1-data 2-scale.fixed 3-scale.funct_, 4-calculus (e.g. totale, giornaliero etcc) 5-colorscale
  remote.mapfile.template<-"mapfile%s_%s_%s_%s_%s.map"

  temp.mapfile<-NULL

  httpheader <- c(Accept="application/json; charset=UTF-8",
                  "Content-Type"="application/json")


  creaMap<-function( cm=list("*"="666666"), mapfile=NULL){
    ### inizio con funzioni per verificare che il server CIRGEO/VSIX sia online e che ci siano i files
    #full.remote.path.mapfile<-sprintf("%s/%s" , remote.base.url, filename)
    # if( RCurl::url.exists( full.remote.path.mapfile ) ){
    #   ### esiste quindi
    #   return(T)
    # }
    if( !RCurl::url.exists( remote.mapfile.creator ) ){
      return(list("message"="Non esiste nel server remoto il file per creare i MAPFILES!"))
    }

    if(is.null(mapfile)) {
      body=list(layername=basic.layerlist.list$overlayGroups$Casi_COVID19,
                colormap=as.list(cm))
    } else {
      body=list(layername=basic.layerlist.list$overlayGroups$Casi_COVID19,
                mapfile=mapfile ,
                colormap=as.list(cm))
    }

    r<-httr::POST(remote.mapfile.creator,
         body = body, encode = "json")

    status<-httr::http_status(r)
    if(r$status_code!=200) {
      print(status$message )
      return(status)
    }

    output<-httr::content(r, "text")
    print(output)

    output<-httr::content(r)
    if(!is.list(output)){
      output<-httr::content(r, "text")
      return(list("message"=paste0("Cannot parse PHP file from remote host, contact developer.<br>", output, sep="")))
    }
    # output.st<- RJSONIO::isValidJSON(output)
    # if(!output.st) {
    #   print(output)
    #   return(list("message"="Cannot parse as JSON PHP file from remote host, contact developer."))
    # }

    print(output)
    return(output$message)

  }

  basic.layerlist.list<-list(    baseGroups = list( osm.bn="Map Night",osm.light ="Map Light",
                                                    osm="None" ),
                                 overlayGroups = list( Casi_COVID19="COVID-19",
                                                       Casi_COVID19labels="Labels")
  )

  basic.layerlist<-list(
    baseGroups = unname(unlist(basic.layerlist.list$baseGroups )),
    overlayGroups = unname(unlist(basic.layerlist.list$overlayGroups ))
  )

  ## FIX che Napoli ha sigla "NA" sbaglio importazione
  naples<-which(dpc_covid19_ita_province$denominazione_provincia=="Napoli")
  dpc_covid19_ita_province[naples, "sigla_provincia"]<-"NA"
  data_to_use <- dpc_covid19_ita_province %>%
    dplyr::group_by(sigla_provincia) %>%
    dplyr::mutate(  delta     =c(0, diff(totale_casi)) ) %>%
    dplyr::select(
      .data$data,  .data$totale_casi, .data$delta, .data$lat, .data$long
    )

  data_to_use <- merge(data_to_use, province_population2019)

  data_to_use$totale_casi.normPop<-data_to_use$totale_casi/data_to_use$Residenti * 10000
  data_to_use$delta.normPop<-data_to_use$delta/data_to_use$Residenti * 10000


  callModule(id = id, function(input, output, session) {
    ns <- session$ns

    #outputOptions(output,  "mymap", suspendWhenHidden = FALSE)
    ## zona dedicata alle computazioni reattive del modulo, in
    ## particolare la definizione degli output (ricordarsi che tali nomi
    ## NON vanno inseriti (a differenza della controparte in input)
    ## all'interno della chiamata a `ns()`)

    output$mymap <- leaflet::renderLeaflet(
      leaflet::leaflet(width="100%", height = 600   ) %>%
        htmlwidgets::onRender(sprintf("function(el, x) {
           %s_mapElement=this;
           %s_layerobjects='';


            var loadHandler = function (event) {
        //        const keys = Object.keys(%s_layerobjects);
        //        for (var  key of keys) {
        //          if(key !== event.sourceTarget.options.layerId){
        //              var ii = %s_mapElement.currentLayersControl.removeLayer( event.sourceTarget );
        //              console.log(ii);
        //              %s_mapElement.removeLayer(%s_layerobjects[key]);
        //              delete %s_layerobjects[key];
        //              //console.log(%s_layerobjects);
        //          }
        //        }

                event.sourceTarget.off('load');

                $('#%s-loader').hide();
            };


            function %s_onLayerAddFunction(e){
              console.log(e.layer);
              if( typeof(e.layer.options.layers)!=='undefined' && e.layer.options.layers=='%s'  ) {
                  %s_layerobjects=e.layer;
                  e.layer.on('load', loadHandler);
                  console.log(e.layer);
              }
            }

           Shiny.onInputChange('%s-leaflet_rendered', true);
           this.on('layeradd', %s_onLayerAddFunction);

           $(\".leaflet-control-layers-overlays > label:nth-child(2) > div:nth-child(1)\").append(\"&nbsp;&nbsp;(<input   title='black label' id='%s_labelBlack' type='checkbox'   >B/W)\");

           $(\".leaflet-control-layers-overlays > label:nth-child(2) > div:nth-child(1)\").append(\"<input style='width: 100px;' title='change size of label' id='%s_labelsizeSlider' type='range' value='11' step='1' min='3' max='30'  >\");

           $(\".leaflet-control-layers-overlays > label:nth-child(1) > div:nth-child(1)\").append(\"<input style='width: 100px;' title='change transparency to layer' id='%s_opacitySlider' type='range' value='60' step='1'  >\");

           $('#%s_labelBlack').on('change', function(x){
              vv=this.checked;
              Shiny.onInputChange('%s-currentLabelBlack', vv);
              if(vv){
                 $('.leaflet-tooltip').css({ 'color' : 'black'  });
                 $('.leaflet-tooltip').css({ 'text-shadow' : '0px 0px 3px white'  });
              } else {
                 $('.leaflet-tooltip').css({ 'color' : 'white'  });
                 $('.leaflet-tooltip').css({ 'text-shadow' : '0px 0px 3px black'  });
              }
            });

           $('#%s_labelsizeSlider').on('input', function(x){
              vv=$(this).val();
              Shiny.onInputChange('%s-currentLabelSize', vv);
              $('.leaflet-tooltip').css({ 'font-size' : vv+'px'  });
            });

           $('#%s_opacitySlider').on('input', function(x){
              var oo = %s_layerobjects;
              vv=$(this).val();
              oo.setOpacity(vv/100);
              Shiny.onInputChange('%s-currentWMSopacity', vv);
              // oo.options.opacity=vv/100 ;
              // $(oo._container).css({ 'opacity' : vv/100 });
            });

            $('#%s-dateRange').on('input', function(e){
             //Shiny.onInputChange('%s-dateRangeChanged', e.target.value);
            });


           }", id, id, id, id, id, id,   id, id, id,
               id, basic.layerlist.list$overlayGroups$Casi_COVID19 ,
               id, id, id, id, id, id, id, id, id, id, id, id, id, id, id)) %>%
        leaflet::setView( 11, 43, 6)  %>%
        leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                          attribution = '&copy;<a href="https://carto.com/attributions">OSM CARTO</a>',
                          options =  leaflet::tileOptions(zIndex = 1, preferCanvas=TRUE, maxZoom = 19, subdomains = "abcd" ), group=basic.layerlist.list$baseGroups$osm.bn)  %>%
        leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                          attribution = '&copy;<a href="https://carto.com/attributions">OSM CARTO</a>',
                          options =  leaflet::tileOptions(zIndex = 2, preferCanvas=TRUE, maxZoom = 19, subdomains = "abcd" ), group=basic.layerlist.list$baseGroups$osm.light)  %>%

        leaflet::hideGroup( as.character(basic.layerlist.list$overlayGroups) ) %>%
        leaflet::showGroup( c(basic.layerlist.list$overlayGroups$Casi_COVID19 ) )   %>%
        # addMouseCoordinates() %>%
        leaflet::addLayersControl(baseGroups    =  basic.layerlist$baseGroups,
                                  overlayGroups = basic.layerlist$overlayGroups,
                                  options =  leaflet::layersControlOptions(collapsed = F) )



      )

    #outputOptions(output,  "mymap", suspendWhenHidden = FALSE)



    observeEvent(input$dateRangeChanged, {

    })



    observe({

      #req(input$leaflet_rendered,input$calculus)
      req(input$leaflet_rendered, input$calculus,
          input$date1, input$scale.fixed,
          input$palette )

      ## trovo labels per ultima data


      print(functionList)
      dt.filtered<- current_data()

      print("paletteList.t")
      # mapfilename<-sprintf(remote.mapfile.template,
      #                      input$date1, input$scale.fixed,
      #                      input$scale.funct_, input$calculus,  input$palette)

      cm<-current_palettFunction()
      sigla2hex<-setNames( cm(dt.filtered[[input$calculus]]) ,
                dt.filtered[["sigla_provincia"]]  )
      retMessage<-creaMap(sigla2hex , temp.mapfile  )

      if( is.list(retMessage) ){
        showNotification( HTML(retMessage$message),   duration = NULL, type ="error")
        shinyjs::runjs(sprintf("$('#%s-loader').hide();", id))
        return(NULL)
      }

      print(retMessage)

      dt.label<-  dt.filtered[[input$calculus]]

      if( !is.element(input$calculus,c("delta", "totale_casi") ) )  label<-sprintf("%.2f",dt.label)
      else label <- sprintf("%d",   dt.label)

      label[label=="NA"]<-""

      op<-isolate(input$currentWMSopacity )
      labsize<-isolate(input$currentLabelSize )
      labelBlackv<-isolate(input$currentLabelBlack )

      labelBlack<-'white'
      labelBlack2<-'black'
      if(!is.null(labelBlackv) && labelBlackv) {
        labelBlack<-'black'
        labelBlack2<-'white'
        }

      if(is.null(op)) op<-0.6
      else op<-as.integer(isolate(input$currentWMSopacity ))/100

      if(is.null(labsize)) labsize<-11
      else  labsize<-as.integer(isolate(input$currentLabelSize ))



      if(is.null(temp.mapfile)){
        pp<-readRDS("../data-raw/province_polygons2019.rds")
        leaflet::leafletProxy("mymap") %>%
            # leaflet::addWMSTiles(baseUrl = sprintf("%s/cgi-bin/mapserv?map=%s",
            #                                    base.url, retMessage   ) ,
            #                  options = leaflet::WMSTileOptions(zIndex = 4, format = "image/png",
            #                                                    transparent = T , opacity=op,
            #                                                    ## below a unique layerid depending on mapfilename.
            #                                                    layerId =   basic.layerlist.list$overlayGroups$Casi_COVID19
            #                                                    ),
            #                  layers=basic.layerlist.list$overlayGroups$Casi_COVID19,
            #                  group=basic.layerlist.list$overlayGroups$Casi_COVID19,
            #                  attribution = "&copy <a title='COVID-19 Maps' href='mailto:francesco.pirotti@unipd.it;'>F. Pirotti</a><a href='www.cirgeo.unipd.it' target='_blank'>@CIRGEO</a>" )  %>%

          leaflet::addPolygons(data = pp, weight=2,
                               color=cm(pp[[input$calculus]]),
                               options=leaflet::pathOptions(interactive=F),
                               opacity = 0.5, fillOpacity = 0.2,
                               className=sprintf("cl%s_%s__%s" ,
                                                 id,
                                                 basic.layerlist.list$overlayGroups$Casi_COVID19,
                                                 pp$SIGLA),
                               layers=sprintf("%s_%s__%s" ,
                                                 id,
                                                 basic.layerlist.list$overlayGroups$Casi_COVID19,
                                                 pp$SIGLA),
                               group=basic.layerlist.list$overlayGroups$Casi_COVID19,
                               attribution = "&copy <a title='COVID-19 Maps' href='mailto:francesco.pirotti@unipd.it;'>F. Pirotti</a><a href='www.cirgeo.unipd.it' target='_blank'>@CIRGEO</a>"
                               )

                  temp.mapfile<<-retMessage

      } else {
        ## do cachebusting to reload tiles
        shinyjs::runjs( sprintf("
              console.log(%s_layerobjects);
              %s_layerobjects.setParams({fake: Date.now()});
              console.log(%s_layerobjects);
                     ", id, id, id) )

                  }

      # leaflet::leafletProxy("mymap") %>%
      #   leaflet::addLabelOnlyMarkers(data = dt.filtered, lng = ~long, lat=~lat,
      #                layerId = sprintf("%s%s", basic.layerlist.list$overlayGroups$Casi_COVID19labels,
      #                                  dt.filtered$sigla_provincia    ),
      #                group =  basic.layerlist.list$overlayGroups$Casi_COVID19labels,
      #                label = label ,
      #                labelOptions = leaflet::labelOptions(zIndex = 10,  noHide = TRUE,
      #                                                     # direction = "bottom",
      #                                                     textOnly = T,
      #                                                     style= list(
      #                                                       "font-size" = sprintf("%dpx",labsize),
      #                                                       "font-weight" = "bold",
      #                                                       "color"=labelBlack,
      #                                                       "text-shadow"=sprintf("0px 0px 3px %s", labelBlack2)
      #                                                     ),
      #                                                     #offset = c(0, -10),
      #                                                     opacity = 1 ) )



    })


    observe(  {
      req(input$date1, NULL,  input$leaflet_rendered, input$calculus )

      dt.filtered<- data_to_use %>%
        dplyr::filter( as.Date(.data$data) == input[["date1"]])

      if( nrow(dt.filtered)<1 )
      {
        showNotification("No data found for selected day.",   duration = 15, type ="error")
        return(NULL)
      }
      dt.label<- unname(unlist(dt.filtered[,input$calculus]))
      if( !is.element(input$calculus,c("delta", "totale_casi") ) )  label<-sprintf("%.2f",dt.label)
      else label <- sprintf("%d",   dt.label)

      label[label=="NA"]<-""



      leaflet::leafletProxy("mymap") %>%

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



    ### reactive functions

    current_data <- reactive({
      req(input$date1, input$calculus)
      data_to_use %>%
        dplyr::filter( as.Date(.data$data) == input$date1) %>%
        dplyr::select(
          .data$sigla_provincia,
          .data[[ input$calculus ]],
          .data$lat, .data$long
        )
    })

    current_palettFunction <- reactive({
      dt<-current_data()
      req(dt,  input[["palette"]], input[["calculus"]])
      leaflet::colorNumeric(
        palette =   paletteList.t[[  input[["palette"]] ]] ,
        domain =  dt[[ input[["calculus"]] ]]
      )
    })

  })

}

## To be copied in the UI
# mod_maps_ui("maps_1")

## To be copied in the server
# mod_maps_server("maps_1")

