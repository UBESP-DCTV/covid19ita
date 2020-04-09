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

  functionList<- list( "Linear"="linear", "Log10"="log10Per" )
  functionList.lut <- names(functionList)
  names(functionList.lut) <- functionList

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

  date.list<-as.Date(unique(dpc_covid19_ita_province$data))
  data.ultima<-max(date.list)
   data.prima<-min(date.list)

  tooltips<-list(scale.fixed="<b style='font-weight:bold; color:red;'>
           Fixed Scale</b><br>If checked, the scale will not change, but keep to the full range
           of all values of chosen variable accross the full time-span.
           If unchecked, the scale will have min and max values of the current day, and therefore change every time a different day is chosen.
           The former allows comparing accross time, but has lower color contrast as range will be larger.
           The latter allows more color contrast over small variations. ",
                 variableName=" <b style='color:red;'>Total Cases</b>: total count of COVID-19 cases at selected day.<br><br>
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
      includeScript(app_sys('app/www/wNumb.min.js')),
      includeScript(app_sys('app/www/nouislider.min.js')),
      includeCSS(app_sys('app/www/nouislider.min.css')),
        tags$script(sprintf("
  $(document).ready(function(){

  function timestamp(str) {
      return new Date(str).getTime();
  }
  var dateSlider = document.getElementById('%s-dateRangeSlider');

  noUiSlider.create(dateSlider, {
      range: {
          min: 1,
          max: %d
      },
      step:1,
      start: [%d],
      pips: {
        mode: 'steps',
        density: 2
    }
  });

  dateSlider.noUiSlider.on('update', function(e){
    Shiny.onInputChange('%s-dateRangeChanged', e[0]);
  });

  dateSlider.noUiSlider.on('change', function(e){
    Shiny.onInputChange('%s-dateRangeChangeFinished',e[0] );
  });
});
                    ", id, length(date.list),
                            length(date.list),
                          #  length(date.list),
                            id, id ) ),
        tags$style(type="text/css",
                 sprintf("
      .noUi-pips-horizontal{    top:-2px !important; height:0px !important; color:#0000 !important; }
      .noUi-value-sub{ color:#0000 !important; }
      .noUi-connects {background: #0006; }
      .datepicker { z-index:999999999999  !important; }
      .selectize-dropdown{   z-index:9999999999 !important;  }
      .dropdown-menu{   z-index:9999999999 !important;  }
      #shiny-notification-panel{ border:black; position:fixed;  bottom:10px;  width:100%%;}
      .shiny-notification-warning{  opacity: 0.97; color:black; border: 1px solid black;
      text-shadow: 0px 0px 15px black; }
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
      }",  id, id))
    ),
    fluidRow(
      box(  div( id=ns("loader"), alt="",
                 style="width:100%; position:fixed; text-align: center; z-index:9999999999;",
                 loader,
                 div( style='display: block; color:white; font-weight:bold;
                      text-shadow:0px 0px 8px black;',
                      "Loading Geodata...", div(id=ns("loader-text")) ) ),

            fluidRow(
              column(4, style="font-weight:bold;",
                     dateInput(ns("date1"), NULL, value = data.ultima, min = data.prima,
                               max = data.ultima, format = 'DD dd MM yyyy') ),
              column(3, div(  selectInput(ns("variableName"), NULL,
                                          choices = list(
                                            "Total cases / 10 000 residents"="totale_casi.normPop",
                                            "Total cases"="totale_casi",
                                            "Daily cases / 10 000 residents"="delta.normPop",
                                            "Daily cases"="delta" ) ),

                   )
              ),
              column(1,
                     div(style=" ",
                         checkboxInput( ns("scale.fixed"), label = "FixScale", value=T ) )
              ),
              column(2, title="Scales",
                     selectInput(ns("scale.funct_"), NULL,  choices = functionList ) ),
              column(2, title="Color Palette",
                     shinyWidgets::pickerInput(ns("palette"), NULL,
                                               choices = names(paletteList.t),
                                               choicesOpt = list(content = paletteList.img) ) )
            ),
            tags$div(     id= ns("dateRangeSlider")  ),
            leaflet::leafletOutput(ns("mymap")) ,
        title = HTML("Distribuzione geografica del numero di casi per provincia" ),
        footer = HTML(""),
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
  #### SNIPPET PER CREARE REGIONI
  # region_polygons2019_poly<-maptools::unionSpatialPolygons(province_polygons2019, province_polygons2019$codice_regione)
  # df<-data.frame(codice_regione=names(region_polygons2019_poly))
  # rownames(df)<-df$codice_regione
  # region_polygons2019 <- sp::SpatialPolygonsDataFrame(region_polygons2019_poly,df)



  dir.funct_ <- list(
    linear=function(x){ x+1-1 },
    log10Per=function(x){ log10(x+1) },
    logPer=function(x){ log(x+1) },
    sqrt=function(x){ sqrt(x) }
  )
  inv.funct_ <- list(
    linear=function(x){ x } ,
    log10Per=function(x){ round(10^(x)-1,0) } ,
    logPer=function(x){ round(exp(x)-1, 0) },
    sqrt=function(x){ x^2 }
  )

######## DUPICATA DA MODIFICARE IN GLOBALS SOMEWHAT
  paletteList.t<-list(

    Person=c("#cccccc",   "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb",
             "#41b6c4", "#1d91c0", "#225ea8",  "#6e016b", "#990000", "#d7301f", "#FF0000" ),
    Spectral=c("#cccccc",   rev( grDevices::rainbow(20)[1:12])),
    YellowOrangeRed= RColorBrewer::brewer.pal(9,"YlOrRd"),
    RedYellowBlue= RColorBrewer::brewer.pal(11,"RdYlBu"),
    BlueYellowRed=rev(RColorBrewer::brewer.pal(11,"RdYlBu")),
    RedWhiteGrey= rev(RColorBrewer::brewer.pal(11,"RdGy"))
  )

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
    ) %>%
    dplyr::arrange(sigla_provincia)

  data_to_use <- merge(data_to_use, province_population2019)

  data_to_use$totale_casi.normPop<-data_to_use$totale_casi/data_to_use$Residenti * 10000
  data_to_use$delta.normPop<-data_to_use$delta/data_to_use$Residenti * 10000


  dates.list<-sort(unique(as.Date(dpc_covid19_ita_province$data)))
  data.prima<- min(dates.list)
  data.ultima<- max(dates.list)
  ## initial values for polygon loading
  dt.filtered.init <- data_to_use %>%
    dplyr::filter( as.Date(.data$data) ==  data.ultima)  %>%
    dplyr::select(
      .data$sigla_provincia,
      .data$denominazione_provincia,
      .data$totale_casi.normPop,
      .data$lat, .data$long
    )%>%
    dplyr::arrange(sigla_provincia)

  cm.init<-leaflet::colorNumeric(
    palette =   paletteList.t$Person ,
    domain =  dt.filtered.init$totale_casi.normPop
  )


  callModule(id = id, function(input, output, session) {
    ns <- session$ns
    leaflet_rendered_all<-F
    isLabelFixed<-F

    #outputOptions(output,  "mymap", suspendWhenHidden = FALSE)
    ## zona dedicata alle computazioni reattive del modulo, in
    ## particolare la definizione degli output (ricordarsi che tali nomi
    ## NON vanno inseriti (a differenza della controparte in input)
    ## all'interno della chiamata a `ns()`) , options=leaflet::leafletOptions(preferCanvas = T)


    #### DRAW MAP ----------
    output$mymap <- leaflet::renderLeaflet(
      leaflet::leaflet(width="100%", height = 600  ) %>%
        htmlwidgets::onRender(sprintf("function(el, x) {
           %s_covidGroupname='%s';
           %s_mapElement=this;
           var count=0;
            function %s_onLayerAddFunction(e){
                 if(typeof(e.layer.options) !=='undefined' && e.layer.options.group== %s_covidGroupname ){
                    count++;
                    if(count>105) {
                      count=0;
                      $('#%s-loader').hide();
                      Shiny.onInputChange('%s-leaflet_rendered_all', true);
                    }
                 }
            }

           Shiny.onInputChange('%s-leaflet_rendered', true);
           this.on('layeradd', %s_onLayerAddFunction);
           this.on('baselayerchange', function(e){
              Shiny.onInputChange('%s-baselayerChanged', e.name );
            });
           this.on('overlayadd', function(e){
              Shiny.onInputChange('%s-layerAdded', {name:e.name, ran:Math.random() } );
            });
            this.on('overlayremove', function(e){
              Shiny.onInputChange('%s-layerRemoved', {name:e.name, ran:Math.random() } );
            });

           $(\".leaflet-control-layers-overlays > label:nth-child(1) > div:nth-child(1)\").append(\"<input style='width: 100px;' title='change transparency to layer' id='%s_opacitySlider' type='range' value='50' step='1'  >\");

           $('#%s_opacitySlider').on('input', function(x){
              var oo = %s_mapElement;
              var pp = oo.layerManager.getLayerGroup(%s_covidGroupname);
              vv=$(this).val();
              pp.setStyle({'fillOpacity':vv/100, 'opacity':vv/100});
              Shiny.onInputChange('%s-currentWMSopacity', vv);
            });



           }",  id, basic.layerlist.list$overlayGroups$Casi_COVID19 ,
                id, id, id, id, id,   id, id, id, id,
                id, id, id, id, id, id, id, id, id, id,
                id,  id, id, id, id, id, id, id)) %>%

        leaflet::setView( 11, 43, 6)  %>%
        leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                          attribution = '&copy;<a href="https://carto.com/attributions">OSM CARTO</a>',
                          options =  leaflet::tileOptions(zIndex = 1, preferCanvas=TRUE, maxZoom = 19, subdomains = "abcd" ), group=basic.layerlist.list$baseGroups$osm.bn)  %>%
        leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                          attribution = '&copy;<a href="https://carto.com/attributions">OSM CARTO</a>',
                          options =  leaflet::tileOptions(zIndex = 2, preferCanvas=TRUE, maxZoom = 19, subdomains = "abcd" ), group=basic.layerlist.list$baseGroups$osm.light)  %>%
        leaflet::hideGroup( as.character(basic.layerlist.list$overlayGroups) ) %>%
        leaflet::showGroup( c(basic.layerlist.list$overlayGroups$Casi_COVID19 ) )   %>%
        leaflet::addLayersControl(baseGroups    =  basic.layerlist$baseGroups,
                                  overlayGroups = basic.layerlist$overlayGroups,
                                  options =  leaflet::layersControlOptions(collapsed = F) )



      )

    ### CHANGE DATE ----
    observeEvent(input$dateRangeChanged, {
      updateDateInput(session, "date1", value= dates.list[[ as.integer(input[["dateRangeChanged"]]) ]] )
    })

    ###  FIX LABEL  ----
    observeEvent(input$layerAdded, {
      req(input$layerAdded==basic.layerlist.list$overlayGroups$Casi_COVID19labels)

      js<-sprintf("
              var layers = %s_mapElement.layerManager.getLayerGroup('%s').getLayers();

              for(var i=0; i < layers.length; i++) {
                 var tt=layers[i].getTooltip();
                 tt.options.sticky=false;
                 tt.options.permanent=true;
                 tt.options.textOnly=true;
                 var tt2=L.tooltip(tt.options);
                 tt2.options.direction='center';
                 tt2.options.style.color=layers[i].options.color;
                 if(tt2.options.style.color=='black'){
                  tt2.options.style['text-shadow']='0px 0px 7px white';
                 } else {
                  tt2.options.style['text-shadow']='0px 0px 7px black';
                 }
                 layers[i].unbindTooltip();
                 layers[i].bindTooltip(tt2);
                 layers[i].setTooltipContent('');

                 layers[i].openTooltip();
                 layers[i].off();
              }
                 Shiny.onInputChange('%s-isLabelFixed', true);
                     ",    id, basic.layerlist.list$overlayGroups$Casi_COVID19,   id
      )
      shinyjs::runjs( js )

    })


    ###  REMOVE FIXED LABEL  ----
    observeEvent(input$layerRemoved, {
      req(input$layerAdded==basic.layerlist.list$overlayGroups$Casi_COVID19labels)

      js<-sprintf("
              var layers = %s_mapElement.layerManager.getLayerGroup('%s').getLayers();

              for(var i=0; i < layers.length; i++) {
                 var tt=layers[i].getTooltip();
                 tt.options.sticky=true;
                 tt.options.permanent=false;
                 tt.options.textOnly=false;
                 var tt2=L.tooltip(tt.options);
                 tt2.options.style.color='black';
                 layers[i].unbindTooltip();
                 layers[i].bindTooltip(tt2);
                // layers[i].setTooltipContent(tt.getContent());
                 layers[i].closeTooltip();
              }

                 Shiny.onInputChange('%s-isLabelFixed', false);
                     ",    id, basic.layerlist.list$overlayGroups$Casi_COVID19,   id
      )
      shinyjs::runjs( js )

    })
    ###  CHANGE OUTLINE of polygons ----
    observeEvent(input$baselayerChanged, {
      print(input$baselayerChanged)
      col<-"white"
      if(input$baselayerChanged == basic.layerlist.list$baseGroups$osm.light ){
        col<-"black"
      }

      if(!is.null(input$isLabelFixed) && input$isLabelFixed){
        coltt<-col
      } else {
        coltt<-"black"
      }

      js<-sprintf("
              var group = %s_mapElement.layerManager.getLayerGroup('%s'); //.getLayers();
              group.setStyle({  'color': '%s' });
              layers=group.getLayers();
              for(var i=0; i < layers.length; i++) {
                 var tt=layers[i].getTooltip();
                 tt.options.color='%s';

              }
                     ",    id, basic.layerlist.list$overlayGroups$Casi_COVID19, col, coltt
      )
      shinyjs::runjs( js )

    })



    #### DRAW POLYGONS FIRST RENDER ----------
    observeEvent(input$leaflet_rendered, {
      cm<-current_palettFunction()
      fillColors<-cm.init(dt.filtered.init[["totale_casi.normPop"]])



      showNotification(HTML(sprintf("
      Drag the <b>slider above the map panel</b> to dynamically change day and color
      over the date range (currently from %s to %s). This will simulate a <b style='color:#0700e8;'>timelapse</b> of spatial distribution of COVID-19 positive cases over Italian provinces.<br>
      You can <b>switch color palette</b>  and convert <b>from linear to logarithmic scales</b>
      to improve visualization when frequency distribution of values is strongly asymmetric/skewed.
      <br><b>FixedScale option</b>: if selected the scale will not change when changing date,
      but keep to the full range calculated from all values of the chosen variable accross
      the full time-span. If unchecked, the scale will change every time a different day is chosen according to the day's values.
      <hr style='margin:3px; color:#666;'>
      <div style='width:100%% ; font-size:10px; text-align:center; '><a href='mailto:francesco.pirotti@unipd.it;'> Francesco Pirotti PhD - </a>,
                           <a href='https://www.cirgeo.unipd.it' target=_blank>TESAF</a> /
                      <a href='https://www.cirgeo.unipd.it' target=_blank>
                      CIRGEO</a></div>


                            ",  format(data.prima , "%A %e %B %yyyy"),
                                    format(data.ultima, "%A %e %B %yyyy") ) ) ,
                       duration = NULL, type ="warning")



      for(i in 1:length(province_polygons2019)){
        leaflet::leafletProxy("mymap") %>%
          leaflet::addPolygons(data = province_polygons2019[i,], weight=1,
                               color="#FFFFFF",
                               fillColor=fillColors[[i]],
                               highlightOptions = leaflet::highlightOptions(stroke = T,
                                                                            weight = 3 ),
                               options=leaflet::pathOptions(interactive=T,
                                                            sigla= province_polygons2019$SIGLA[[i]] ),
                               opacity = 1, fillOpacity = 0.5,
                               label ='' ,
                               labelOptions = leaflet::labelOptions(#zIndex = 100,
                                                                    #noHide = TRUE,
                                                                    #textOnly = T,
                                                                    #permanent = T,
                                                                    style= list(
                                                                      "font-size" = "12px",
                                                                      "font-weight" = "bold",
                                                                      "color"="black",
                                                                      "text-shadow"="0px 0px 7px black"
                                                                    ),
                                                                    opacity = 1
                               ),


                               layerId=sprintf("%s_%s__%s" ,
                                               id,
                                               basic.layerlist.list$overlayGroups$Casi_COVID19,
                                               province_polygons2019$SIGLA[[i]]),
                               group=basic.layerlist.list$overlayGroups$Casi_COVID19 )
      }

      drawResponsive$resume()
    })



    #### DRAW RESPONSIVE ----------
    drawResponsive<-observe(suspended=T, {

      req(input$leaflet_rendered,input$leaflet_rendered_all,
          input$variableName,
          input$date1,
          input$palette,
          (input[["scale.fixed"]]||(input[["scale.fixed"]]==F)) )

      ## trovo labels per ultima data

      #print("triggered")

      dt.filtered<- current_data()
      if( nrow(dt.filtered)<1 )
      {
        showNotification("No data found for selected day.",   duration = 15, type ="error")
        return(NULL)
      }

      fn<- dir.funct_[[ input[["scale.funct_"]] ]]

      cm<-current_palettFunction()

     # print(input$isLabelFixed)
      if(!is.null(input$isLabelFixed) && input$isLabelFixed){

        templ<- ifelse( !is.element(input[["variableName"]],c("delta", "totale_casi") ),
                        "%.2f" ,  "%.0f"  )

        label <- sprintf(templ,
                         dt.filtered[[  input[["variableName"]] ]] )

        label[label=="NA"]<-""

      } else {

        templ<- ifelse( !is.element(input[["variableName"]],c("delta", "totale_casi") ),
                        "%s (%s)<br>%.2f" ,  "%s (%s)<br>%.0f"  )

        label <- sprintf(templ,
                         dt.filtered[["denominazione_provincia"]],
                         dt.filtered[["sigla_provincia"]],
                         dt.filtered[[  input[["variableName"]] ]] )

        label[label=="NA"]<-""

      }


      jsonString<-sprintf("\"%s\":{ col:\"%s\", lab:\"%s\"}",
                          dt.filtered[["sigla_provincia"]],
                          cm( fn(dt.filtered[[input$variableName]])  ) ,
                          label )

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

      js<-sprintf("
              var valueMap = {%s};
              var layers = %s_mapElement.layerManager.getLayerGroup('%s').getLayers();

             //console.log(layers[0]);
             // var labels = %s_mapElement.layerManager.getLayerGroup('%s').getLayers();
              if(layers.length!= Object.keys(valueMap).length   ){
                 alert(layers.length+' OPS problem');
              } else {
                for(var i=0; i < layers.length; i++) {
                   layers[i].setStyle({  'fillColor': valueMap[ layers[i].options.sigla ].col });
                   layers[i].setTooltipContent(  valueMap[ layers[i].options.sigla ].lab  );
                   layers[i].redraw();
                }
              }
                     ",   paste(collapse=",",jsonString),
              id, basic.layerlist.list$overlayGroups$Casi_COVID19,
              id ,  basic.layerlist.list$overlayGroups$Casi_COVID19labels, op
      )
      shinyjs::runjs( js )
    })


    #### get data ----
    current_data <- reactive({
      req(input$date1, input$variableName)
      data_to_use %>%
        dplyr::filter( as.Date(.data$data) == input$date1) %>%
        dplyr::select(
          .data$denominazione_provincia,
          .data$sigla_provincia,
          .data[[ input[["variableName"]] ]],
          .data$lat, .data$long
        ) %>%
        dplyr::arrange(sigla_provincia)

    })


    #### palette and legend ----
    current_palettFunction <- reactive({
      dt<-current_data()
      req(dt,  input[["palette"]], input[["variableName"]], input[["scale.funct_"]])

      fn<- dir.funct_[[ input[["scale.funct_"]] ]]

      if(!input[["scale.fixed"]]) domain<-fn(dt[[ input[["variableName"]] ]])
      else                        domain<-fn(data_to_use[[  input[["variableName"]] ]])

      if(   is.null(domain)){
        print('problema')
        return(NULL)
      }

      pal<-leaflet::colorNumeric(
        palette =   paletteList.t[[  input[["palette"]] ]] ,
        domain =  domain
      )

      leaflet::leafletProxy("mymap") %>%
        leaflet::addLegend( "bottomright", pal = pal, values = domain,
                            layerId="Legend_Casi_COVID19",
                            labFormat = leaflet::labelFormat(prefix = "", big.mark = " ",
                                                    transform = inv.funct_[[input$scale.funct_]] ),
                            opacity = 1 )

      pal


    })

  })

}

## To be copied in the UI
# mod_maps_ui("maps_1")

## To be copied in the server
# mod_maps_server("maps_1")

