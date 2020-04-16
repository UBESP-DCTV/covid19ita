#' maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom grDevices dev.off png
#' @importFrom graphics image par
#'
#'
mod_maps_ui <- function(id) {
  ns <- NS(id) # non cancellare

  loader <- HTML(
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

  function_list <- list("Linear" = "linear", "Log10" = "log10Per")
  function_list_lut <- names(function_list)
  names(function_list_lut) <- function_list

  palette_list <- (names(palette_list_t))

  palette_list_img <- NA_character_
  for (pal in palette_list_t) {
    png(tf1 <- tempfile(fileext = ".png"), width = 160, height = 20)
    op <- par(mar = rep(0, 4))
    image(seq_along(pal), 1, as.matrix(seq_along(pal)),
      col = pal,
      xlab = "", ylab = "", xaxt = "n", yaxt = "n",   bty = "n"
    )
    dev.off()

    txt <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
    palette_list_img <- c(palette_list_img, sprintf('<img src="data:image/png;base64,%s">', txt))

    # cat(html, file = tf2 <- tempfile(fileext = ".html"))
    # browseURL(tf2)
  }

  dates_list <- as.Date(unique(dpc_covid19_ita_province$data))
  data_ultima <- max(dates_list)
  data_prima <- min(dates_list)

  tooltips <- list(
    scale.fixed = "<b style='font-weight:bold; color:red;'>
           Fixed Scale</b><br>If checked, the scale will not change, but keep to the full range
           of all values of chosen variable accross the full time-span.
           If unchecked, the scale will have min and max values of the current day, and therefore change every time a different day is chosen.
           The former allows comparing accross time, but has lower color contrast as range will be larger.
           The latter allows more color contrast over small variations. ",
    variableName = " <b style='color:red;'>Total Cases</b>: total count of COVID-19 cases at selected day.<br><br>
    <b style='color:red;'>Daily Increase:</b> Difference of cases with previous day.<br><br>
    <b style='color:red;'>Normalized daily Increase (not implemented)</b>: difference between chosen day's number of COVID-19 cases (T1) and previous day (T0), over the sum, i.e.
    (T0 - T1)/(T0 + T1) - ranges from -1 to 1, 0 means flat increase - this scale highlights initial cases and tends to 0 when new cases are a
               small percentage of total cases.",
    scale.residents = "Will normalize per residents in the area and scale x1000 (i.e. number every 1000 residents).",
    scale.funct_ = "Scales can be linear or transformed to lognormal to improve visualization of asymmetric (skewed) frequency distributions."
  )

  help_tag <- tags$i(
    class = "fa fa-info-circle", style = "cursor:pointer;font-size: 30px; margin-top: 3px;margin-left: 4px;",
    onclick = sprintf(
      'Shiny.onInputChange("%s", Math.random())  ',
      ns("getHelp")
    )
  )
  icon_tag <- tags$i(
    class = "fa fa-play-circle-o", title = "", style = "cursor:pointer;font-size: 30px; margin-left: 4px; margin-top: 3px;",
    onclick = sprintf(
      '$(this).toggleClass("fa-play-circle-o fa-pause-circle-o");
                       Shiny.onInputChange("%s", {rand:Math.random(), state:$(this).attr("class")=="fa fa-pause-circle-o"});  ',
      ns("isPlaying")
    )
  )
  htmltools::htmlDependencies(icon_tag) <- htmltools::htmlDependency("font-awesome",
    "5.3.1", "www/shared/fontawesome",
    package = "shiny",
    stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
  )
  ## Da qui in poi inserire il contenuto lato UI del modulo, in
  ## particolare la definizione degli input (ricordarsi di inserire i
  ## relativi id all'interno di una chiamata a `ns(<input_id>)`)
  fluidPage(
    ## sfruttare i `box()` per quanto possibile, ed eventuali
    ## `fludRow()`
    shinyjs::useShinyjs(),

    tags$head(
      includeScript(app_sys("app/www/nouislider.min.js")),
      includeCSS(app_sys("app/www/nouislider.min.css")),
      tags$script(sprintf(
        "
  $(document).ready(function() {

  function timestamp(str) {
      return new Date(str).getTime();
  }
  var dateSlider = document.getElementById('%s-dateRangeSlider');
  maxNdates= %d;
  noUiSlider.create(dateSlider, {
      range: {
          min: 1,
          max: maxNdates
      },
      step:1,
      start: [maxNdates],
      pips: {
        mode: 'steps',
        density: 2
    }
  });

  dateSlider.noUiSlider.on('update', function(e) {
    Shiny.onInputChange('%s-dateRangeChanged', e[0]);
  });

});
                    ", id, length(dates_list),
        id, id
      )),
      tags$style(
        type = "text/css",
        sprintf("
      .noUi-pips-horizontal{    top:-2px !important; height:0px !important; color:#0000 !important; }
      .noUi-value-sub{ color:#0000 !important; }
      .noUi-connects {background: #0006; }
      .datepicker { z-index:999999999999  !important; }
      .selectize-dropdown{   z-index:9999999999 !important;  }
      .dropdown-menu{   z-index:9999999999 !important;  }
      #shiny-notification-%s-initialNotification{ border:black; right: 10px; position:fixed;  bottom:10px; width: 50vw !important; min-width:400px  !important;   opacity: 0.97; color:black; border: 1px solid black;
      text-shadow: 0px 0px 15px black; }
      #%s-mymap {
        height:calc(100vh - 200px) !important;
        min-height:500px;
        background: #333;
        border: 1px solid black;
        border-radius: 8px;
      }
      #%s-date1   {
        color: red !important;
        width:180px;
        font-size: larger;
        margin: 0;
        background: darkgray;
      }", id, id, id, id)
      )
    ),
    fluidRow(
      box(div(
        id = ns("loader"), alt = "",
        style = "left: 10px; right: 10px; position:fixed; text-align: center; z-index:9999999999;",
        loader,
        div(
          style = "display: block; color:white; font-weight:bold;
                      text-shadow:0px 0px 8px black;",
          "Loading Geodata...", div(id = ns("loader-text"))
        )
      ),

      fluidRow(
        column(3,
          style = "font-weight:bold; color:#2d5900a1; width:300px !important;",
          div(style = "float:left;", dateInput(ns("date1"), NULL,
            value = data_ultima, min = data_prima,
            max = data_ultima, format = "DD dd MM yyyy"
          )),
          icon_tag, help_tag
        ),
        column(3, style = " width: 250px;", selectInput(ns("variableName"), NULL,
          choices = list(
            "Total cases / 10 000 residents" = "totale_casi_norm_pop",
            "Total cases" = "totale_casi",
            "Daily cases / 10 000 residents" = "delta_norm_pop",
            "Daily cases" = "delta"
          )
        )),
        column(3,
          title = "Scales", style = "width: 190px;",
          div(
            style = "float:left; margin-right:5px;",
            selectInput(ns("scale.funct_"), NULL,
              width = 100,
              choices = function_list
            )
          ),
          checkboxInput(ns("scale.fixed"), label = "Fixed", value = TRUE)
        ),
        column(2,
          title = "Color Palette", style = "min-width: 225px;width: 232px;",
          shinyWidgets::pickerInput(ns("palette"), NULL,
            choices = names(palette_list_t),
            choicesOpt = list(content = palette_list_img)
          )
        )
      ),
      tags$div(id = ns("dateRangeSlider")),

      leaflet::leafletOutput(ns("mymap")),
      title = HTML("Distribuzione geografica del numero di casi per provincia"),
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
    linear = function(x) {
      x + 1 - 1
    },
    log10Per = function(x) {
      log10(x + 1)
    },
    logPer = function(x) {
      log(x + 1)
    },
    sqrt = function(x) {
      sqrt(x)
    }
  )
  inv_funct_ <- list(
    linear = function(x) {
      x
    },
    log10Per = function(x) {
      round(10^x - 1, 0)
    },
    logPer = function(x) {
      round(exp(x) - 1, 0)
    },
    sqrt = function(x) {
      x^2
    }
  )


  basic_layerlist_list <- list(
    baseGroups = list(
      osm.bn = "Map Night", osm.light = "Map Light",
      osm = "None"
    ),
    overlayGroups = list(
      Casi_COVID19 = "COVID-19",
      Casi_COVID19labels = "Labels"
    )
  )

  basic_layerlist <- list(
    baseGroups = unname(unlist(basic_layerlist_list$baseGroups)),
    overlayGroups = unname(unlist(basic_layerlist_list$overlayGroups))
  )

  ## FIX che Napoli ha sigla "NA" sbaglio importazione
  naples <- which(dpc_covid19_ita_province$denominazione_provincia == "Napoli")
  dpc_covid19_ita_province[naples, "sigla_provincia"] <- "NA"
  data_to_use <- dpc_covid19_ita_province %>%
    dplyr::group_by(.data$sigla_provincia) %>%
    dplyr::mutate(delta = c(0, diff(.data$totale_casi))) %>%
    dplyr::select(
      .data$data, .data$totale_casi, .data$delta, .data$lat, .data$long
    ) %>%
    dplyr::arrange(.data$sigla_provincia)

  data_to_use <- merge(data_to_use, province_population2019)

  data_to_use$totale_casi_norm_pop <- data_to_use$totale_casi / data_to_use$Residenti * 10000
  data_to_use$delta_norm_pop <- data_to_use$delta / data_to_use$Residenti * 10000


  dates_list <- sort(unique(as.Date(dpc_covid19_ita_province$data)))
  data_prima <- min(dates_list)
  data_ultima <- max(dates_list)
  ## initial values for polygon loading
  dt_filtered_init <- data_to_use %>%
    dplyr::filter(as.Date(.data$data) == data_ultima) %>%
    dplyr::select(
      .data$sigla_provincia,
      .data$denominazione_provincia,
      .data$totale_casi_norm_pop,
      .data$lat, .data$long
    ) %>%
    dplyr::arrange(.data$sigla_provincia)

  cm_init <- leaflet::colorNumeric(
    palette = palette_list_t$Person,
    domain = dt_filtered_init$totale_casi_norm_pop
  )


  callModule(id = id, function(input, output, session) {
    ns <- session$ns
    leaflet_rendered_all <- FALSE
    is_label_fixed <- FALSE

    # outputOptions(output,  "mymap", suspendWhenHidden = FALSE)
    ## zona dedicata alle computazioni reattive del modulo, in
    ## particolare la definizione degli output (ricordarsi che tali nomi
    ## NON vanno inseriti (a differenza della controparte in input)
    ## all'interno della chiamata a `ns()`) , options=leaflet::leafletOptions(preferCanvas = T)

    #### DRAW MAP ----------
    output$mymap <- leaflet::renderLeaflet(
      leaflet::leaflet(width = "100%", height = 600) %>%
        htmlwidgets::onRender(sprintf(
          "function(el, x) {
           %s_covidGroupname='%s';
           %s_mapElement=this;
           var count=0;
            function %s_onLayerAddFunction(e) {
                 if(typeof(e.layer.options) !=='undefined' && e.layer.options.group== %s_covidGroupname ) {
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
           this.on('baselayerchange', function(e) {
              Shiny.onInputChange('%s-baselayerChanged', e.name );
            });
           this.on('overlayadd', function(e) {
              Shiny.onInputChange('%s-layerAdded', {name:e.name, ran:Math.random() } );
            });
            this.on('overlayremove', function(e) {
              Shiny.onInputChange('%s-layerRemoved', {name:e.name, ran:Math.random() } );
            });

           $(\".leaflet-control-layers-overlays > label:nth-child(1) > div:nth-child(1)\").append(\"<input style='width: 100px;' title='change transparency to layer' id='%s_opacitySlider' type='range' value='50' step='1'  >\");

           $('#%s_opacitySlider').on('input', function(x) {
              var oo = %s_mapElement;
              var pp = oo.layerManager.getLayerGroup(%s_covidGroupname);
              vv=$(this).val();
              pp.setStyle({'fillOpacity':vv/100, 'opacity':vv/100});
              Shiny.onInputChange('%s-currentWMSopacity', vv);
            });



           }", id, basic_layerlist_list$overlayGroups$Casi_COVID19,
          id, id, id, id, id, id, id, id, id,
          id, id, id, id, id, id, id, id, id, id,
          id, id, id, id, id, id, id, id
        )) %>%

        leaflet::setView(11, 43, 6) %>%
        leaflet::addTiles(
          urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
          attribution = '&copy;<a href="https://carto.com/attributions">OSM CARTO</a>',
          options = leaflet::tileOptions(zIndex = 1, preferCanvas = TRUE, maxZoom = 19, subdomains = "abcd"), group = basic_layerlist_list$baseGroups$osm.bn
        ) %>%
        leaflet::addTiles(
          urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
          attribution = '&copy;<a href="https://carto.com/attributions">OSM CARTO</a>',
          options = leaflet::tileOptions(zIndex = 2, preferCanvas = TRUE, maxZoom = 19, subdomains = "abcd"), group = basic_layerlist_list$baseGroups$osm.light
        ) %>%
        leaflet::hideGroup(as.character(basic_layerlist_list$overlayGroups)) %>%
        leaflet::showGroup(c(basic_layerlist_list$overlayGroups$Casi_COVID19)) %>%
        leaflet::addLayersControl(
          baseGroups = basic_layerlist$baseGroups,
          overlayGroups = basic_layerlist$overlayGroups,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    )

    ### CHANGE DATE ----
    observeEvent(input$dateRangeChanged, {
      updateDateInput(session, "date1", value = dates_list[[as.integer(input[["dateRangeChanged"]])]])
    })

    ### HELP PANEL ----
    observeEvent(input$getHelp, {
      sn(NULL)
    })

    ###  FIX LABEL  ----
    observeEvent(input$layerAdded, {
      req(input$layerAdded == basic_layerlist_list$overlayGroups$Casi_COVID19labels)

      js <- sprintf("
              var layers = %s_mapElement.layerManager.getLayerGroup('%s').getLayers();

              for(var i=0; i < layers.length; i++) {
                 var tt=layers[i].getTooltip();
                 tt.options.sticky=false;
                 tt.options.permanent=true;
                 tt.options.textOnly=true;
                 var tt2=L.tooltip(tt.options);
                 tt2.options.direction='center';
                 tt2.options.style.color=layers[i].options.color;
                 if(tt2.options.style.color=='black') {
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
                 Shiny.onInputChange('%s-is_label_fixed', true);
                     ", id, basic_layerlist_list$overlayGroups$Casi_COVID19, id)
      shinyjs::runjs(js)
    })


    ###  REMOVE FIXED LABEL  ----
    observeEvent(input$layerRemoved, {
      req(input$layerAdded == basic_layerlist_list$overlayGroups$Casi_COVID19labels)

      js <- sprintf("
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

                 Shiny.onInputChange('%s-is_label_fixed', false);
                     ", id, basic_layerlist_list$overlayGroups$Casi_COVID19, id)
      shinyjs::runjs(js)
    })
    ###  CHANGE OUTLINE of polygons ----
    observeEvent(input$baselayerChanged, {
      print(input$baselayerChanged)
      col <- "white"
      if (input$baselayerChanged == basic_layerlist_list$baseGroups$osm.light) {
        col <- "black"
      }

      if (!is.null(input$is_label_fixed) && input$is_label_fixed) {
        coltt <- col
      } else {
        coltt <- "black"
      }

      js <- sprintf("
              var group = %s_mapElement.layerManager.getLayerGroup('%s'); //.getLayers();
              group.setStyle({  'color': '%s' });
              layers=group.getLayers();
              for(var i=0; i < layers.length; i++) {
                 var tt=layers[i].getTooltip();
                 tt.options.color='%s';

              }
                     ", id, basic_layerlist_list$overlayGroups$Casi_COVID19, col, coltt)
      shinyjs::runjs(js)
    })



    #### DRAW POLYGONS FIRST RENDER ----------
    observeEvent(input$leaflet_rendered, {
      sn()
      # cm<-current_palett_function()
      fill_colors <- cm_init(dt_filtered_init[["totale_casi_norm_pop"]])


      for (i in seq_along(province_polygons2019)) {
        leaflet::leafletProxy("mymap") %>%
          leaflet::addPolygons(
            data = province_polygons2019[i, ], weight = 1,
            color = "#FFFFFF",
            fillColor = fill_colors[[i]],
            highlightOptions = leaflet::highlightOptions(
              stroke = TRUE,
              weight = 3
            ),
            options = leaflet::pathOptions(
              interactive = TRUE,
              sigla = province_polygons2019$SIGLA[[i]]
            ),
            opacity = 1, fillOpacity = 0.5,
            label = "",
            labelOptions = leaflet::labelOptions(# zIndex = 100,
              # noHide = TRUE,
              # textOnly = TRUE,
              # permanent = TRUE,
              style = list(
                "font-size" = "12px",
                "font-weight" = "bold",
                "color" = "black",
                "text-shadow" = "0px 0px 7px black"
              ),
              opacity = 1
            ),


            layerId = sprintf(
              "%s_%s__%s",
              id,
              basic_layerlist_list$overlayGroups$Casi_COVID19,
              province_polygons2019$SIGLA[[i]]
            ),
            group = basic_layerlist_list$overlayGroups$Casi_COVID19
          )
      }

      draw_responsive$resume()
    })



    #### DRAW RESPONSIVE ----------
    draw_responsive <- observe(suspended = TRUE, {
      req(
        input$leaflet_rendered, input$leaflet_rendered_all,
        input$variableName,
        input$date1,
        input$palette, dates_list,
        (input[["scale.fixed"]] || (input[["scale.fixed"]] == FALSE))
      )

      ## trovo labels per ultima data

      # print("triggered")

      dt_filtered <- current_data()
      if (is.null(dt_filtered) || nrow(dt_filtered) < 1) {
        showNotification("No data found for selected day.", duration = 15, type = "error")
        return(NULL)
      }

      fn <- dir.funct_[[input[["scale.funct_"]]]]
      cm <- current_palett_function()

      # print(input$is_label_fixed)
      if (!is.null(input$is_label_fixed) && input$is_label_fixed) {
        templ <- ifelse(!is.element(input[["variableName"]], c("delta", "totale_casi")),
          "%.2f", "%.0f"
        )

        label <- sprintf(
          templ,
          dt_filtered[[input[["variableName"]]]]
        )

        label[label == "NA"] <- ""
      } else {
        templ <- ifelse(!is.element(input[["variableName"]], c("delta", "totale_casi")),
          "%s (%s)<br>%.2f", "%s (%s)<br>%.0f"
        )
        label <- sprintf(
          templ,
          dt_filtered[["denominazione_provincia"]],
          dt_filtered[["sigla_provincia"]],
          dt_filtered[[input[["variableName"]]]]
        )

        label[label == "NA"] <- ""
      }


      json_string <- sprintf(
        "\"%s\":{ col:\"%s\", lab:\"%s\"}",
        dt_filtered[["sigla_provincia"]],
        cm(fn(dt_filtered[[input$variableName]])),
        label
      )

      op <- isolate(input$currentWMSopacity)

      if (is.null(op)) {
        op <- 0.6
      } else {
        op <- as.integer(isolate(input$currentWMSopacity)) / 100
      }

      go_slider <- ""
      if (!is.null(input$isPlaying) && input$isPlaying$state) {
        go_slider <- sprintf("
        var dateSlider = document.getElementById('%s-dateRangeSlider');
        var nv=parseInt(dateSlider.noUiSlider.get());
        nv++;
        if(nv>%d) nv=1;
        console.log(nv);
        dateSlider.noUiSlider.setHandle(0, nv, true); ", id, length(dates_list))
      } else {
        go_slider <- " "
      }

      js <- sprintf(
        "
              var valueMap = {%s};
              var layers = %s_mapElement.layerManager.getLayerGroup('%s').getLayers();

              if(layers.length!= Object.keys(valueMap).length   ) {
                 alert(layers.length+' OPS problem');
              } else {
                for(var i=0; i < layers.length; i++) {
                   layers[i].setStyle({  'fillColor': valueMap[ layers[i].options.sigla ].col });
                   layers[i].setTooltipContent(  valueMap[ layers[i].options.sigla ].lab  );
                }
              }
              %s
                     ", paste(collapse = ",", json_string),
        id, basic_layerlist_list$overlayGroups$Casi_COVID19,
        sprintf(go_slider, id)
      )
      shinyjs::runjs(js)
    })


    #### get data ----
    current_data <- reactive({
      req(input$date1, input$variableName)
      data_to_use %>%
        dplyr::filter(as.Date(.data$data) == input$date1) %>%
        dplyr::select(
          .data$denominazione_provincia,
          .data$sigla_provincia,
          .data[[input[["variableName"]]]],
          .data$lat, .data$long
        ) %>%
        dplyr::arrange(.data$sigla_provincia)
    })


    #### current_palett_function ----
    current_palett_function <- reactive({
      dt <- current_data()
      req(dt, input[["palette"]], input[["variableName"]], input[["scale.funct_"]])

      fn <- dir.funct_[[input[["scale.funct_"]]]]

      if (!input[["scale.fixed"]]) {
        domain <- fn(dt[[input[["variableName"]]]])
      } else {
        domain <- fn(data_to_use[[input[["variableName"]]]])
      }

      if (is.null(domain)) {
        print("problema")
        return(NULL)
      }
      domain[is.infinite(domain)] <- 0

      pal <- tryCatch(
        {
          leaflet::colorNumeric(
            palette = palette_list_t[[input[["palette"]]]],
            domain = domain
          )
        },
        error = function(e) {
          print(domain)
        },
        warning = function(w) {
          print(w)
        }
      )


      leaflet::leafletProxy("mymap") %>%
        leaflet::addLegend("bottomright",
          pal = pal, values = domain,
          layerId = "Legend_Casi_COVID19",
          labFormat = leaflet::labelFormat(
            prefix = "", big.mark = " ",
            transform = inv_funct_[[input$scale.funct_]]
          ),
          opacity = 1
        )

      pal
    })




    sn <- function(dur = 5) {
      showNotification(
        id = ns("initialNotification"), HTML(sprintf(
          "
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


                            ", format(data_prima, "%A %e %B %YY"),
          format(data_ultima, "%A %e %B %YY")
        )),
        duration = dur, type = "warning"
      )
    }
  })
}

## To be copied in the UI
# mod_maps_ui("maps_1")

## To be copied in the server
# mod_maps_server("maps_1")
