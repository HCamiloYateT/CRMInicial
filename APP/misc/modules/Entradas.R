# Módulo: Entradas ----
# Análisis de café recibido: calidad, factor de rendimiento, origen y granulometría

# UI ----

mod_entradas_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    ## Fila 1: KPIs de volumen con CajaModal ----
    fluidRow(
      column(width = 4, CajaModalUI(ns("kpi_num_entradas"))),
      column(width = 4, CajaModalUI(ns("kpi_kls_brutos"))),
      column(width = 4, CajaModalUI(ns("kpi_kls_netos")))
    ),
    
    ## Fila 2: Distribución de calidad + tabla detalle ----
    fluidRow(
      
      column(
        width = 5,
        bs4Card(
          title       = "Distribución por Calidad",
          width       = 12,
          status      = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          tableOutput(ns("DistribucionCalidad"))
        )
      ),
      
      column(
        width = 7,
        bs4Card(
          title       = "Kilos Mensuales (Entradas)",
          width       = 12,
          status      = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput(ns("SerieResumenEntradas"), height = "250px")
        )
      )
    ),
    
    ## Fila 3: Mapa de orígenes + tabla detalle mapa ----
    fluidRow(
      
      column(
        width = 8,
        bs4Card(
          title       = "Mapa de Orígenes del Café",
          width       = 12,
          status      = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          leafletOutput(ns("MapaOrigenes"), height = "400px")
        )
      ),
      
      column(
        width = 4,
        bs4Card(
          title       = "Detalle por Municipio",
          width       = 12,
          status      = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          DT::dataTableOutput(ns("DetalleMapa"))
        )
      )
    ),
    
    ## Fila 4: Factor de rendimiento (serie + caja) ----
    fluidRow(
      
      column(
        width = 8,
        bs4Card(
          title       = "Serie del Factor de Rendimiento",
          width       = 12,
          status      = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("SerieFactorRendimiento"), height = "260px")
        )
      ),
      
      column(
        width = 4,
        bs4Card(
          title       = "Distribución del Factor",
          width       = 12,
          status      = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("CajaFactorRendimiento"), height = "260px")
        )
      )
    ),
    
    ## Fila 5: Humedad (serie + caja) ----
    fluidRow(
      
      column(
        width = 8,
        bs4Card(
          title       = "Serie de Humedad",
          width       = 12,
          status      = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("SerieHumedad"), height = "260px")
        )
      ),
      
      column(
        width = 4,
        bs4Card(
          title       = "Distribución de Humedad",
          width       = 12,
          status      = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("CajaHumedad"), height = "260px")
        )
      )
    ),
    
    ## Fila 6: Granulometría (serie + cajas) ----
    fluidRow(
      
      column(
        width = 8,
        bs4Card(
          title       = "Serie de Granulometría",
          width       = 12,
          status      = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("SerieGranulometria"), height = "260px")
        )
      ),
      
      column(
        width = 4,
        bs4Card(
          title       = "Distribución de Granulometría",
          width       = 12,
          status      = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("CajaGranulometria"), height = "260px")
        )
      )
    )
  )
}

# Server ----

mod_entradas_server <- function(id, entradas_r, facturas_r) {
  moduleServer(id, function(input, output, session) {
    
    ## KPIs con CajaModal ----
    
    CajaModal(
      id            = "kpi_num_entradas",
      valor         = reactive({
        comma(
          entradas_r() %>% select(SucCod, RegNro) %>% distinct() %>% nrow(),
          accuracy = 1
        )
      }),
      texto         = "Entradas",
      icono         = "truck",
      mostrar_boton = TRUE,
      contenido_modal = function() {
        div(DT::dataTableOutput(session$ns("dt_entradas_detalle")))
      },
      titulo_modal = "Detalle de Entradas"
    )
    
    output$dt_entradas_detalle <- DT::renderDataTable({
      DT::datatable(
        entradas_r() %>%
          select(RegNro, RegFchEnt, Sucursal, CalNom, KilosNetos,
                 FactorRendimiento, EvaPorHum) %>%
          arrange(desc(RegFchEnt)),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })
    
    CajaModal(
      id    = "kpi_kls_brutos",
      valor = reactive(comma(sum(entradas_r()$KilosNetos, na.rm = TRUE), accuracy = 1)),
      texto = "Kilos Brutos",
      icono = "weight-hanging"
    )
    
    CajaModal(
      id    = "kpi_kls_netos",
      valor = reactive(comma(sum(entradas_r()$KilosNetos, na.rm = TRUE), accuracy = 1)),
      texto = "Kilos Netos",
      icono = "weight-hanging"
    )
    
    ## Distribución por calidad ----
    output$DistribucionCalidad <- renderTable({
      entradas_r() %>%
        mutate(TotalKilos = sum(KilosNetos)) %>%
        group_by(Calidad = CalNom) %>%
        summarise(
          Kilos      = comma(sum(KilosNetos)),
          Porcentaje = percent(sum(KilosNetos) / unique(TotalKilos))
        ) %>%
        arrange(desc(Porcentaje))
    }, spacing = "xs", width = "100%")
    
    ## Serie mensual de kilos en entradas ----
    output$SerieResumenEntradas <- renderPlotly({
      aux1 <- facturas_r() %>%
        group_by(Fecha = floor_date(FCoFch, unit = "month")) %>%
        summarise(Kilos = sum(kilos), .groups = "drop")
      
      plot_ly(
        data = aux1, x = ~Fecha, y = ~Kilos,
        type = "scatter", mode = "lines+markers",
        line = list(width = 2), marker = list(size = 5),
        name = "Kilos", color = I("#000000"),
        hoverinfo = "text", hoverlabel = list(align = "left"),
        hovertext = paste0(
          "<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
          "<br>Kilos: ", comma(aux1$Kilos, accuracy = 0.01)
        )
      ) %>%
        layout(
          title  = list(text = "Kilos en Entradas",
                        font = list(family = "Arial, sans-serif", size = 18)),
          xaxis  = list(title = "Fecha", gridcolor = "#CCD1D1"),
          yaxis  = list(tickformat = "s", title = ""),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    ## Mapa de calor + marcadores radiales de orígenes ----
    output$MapaOrigenes <- renderLeaflet({
      aux1 <- entradas_r() %>%
        group_by(PerRazSoc, NomDepPro, MunPro, LatPro, LngPro) %>%
        summarise(
          Lat            = max(Lat),
          Lng            = max(Lng),
          Entradas       = n(),
          Kilos          = sum(KilosNetos),
          FactorPromedio = 96.8,
          .groups = "drop"
        )
      
      labs <- lapply(seq(nrow(aux1)), function(i) {
        paste0(
          "<b>Departamento: </b>", aux1[i, "NomDepPro"],
          "<br><b>Municipio: </b>", aux1[i, "MunPro"],
          "<br><b>Entradas: </b>",  aux1[i, "Entradas"],
          "<br><b>Kilos: </b>",     comma(aux1$Kilos[i], accuracy = 1),
          "<br><b>Factor: </b>",    round(aux1$FactorPromedio[i], 2)
        )
      })
      
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 10, zoomControl = FALSE)) %>%
        setView(lat = max(aux1$Lat), lng = max(aux1$Lng), zoom = 7) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addMarkers(data = aux1, lng = ~Lng, lat = ~Lat, label = ~PerRazSoc) %>%
        addHeatmap(
          data = aux1, lng = ~LngPro, lat = ~LatPro,
          intensity = ~Kilos, max = 0.1, blur = 35, group = "Mapa de Calor"
        ) %>%
        addCircleMarkers(
          data   = aux1, lng = ~LngPro, lat = ~LatPro,
          radius = ~scales::rescale(Kilos, to = c(10, 25)),
          label  = lapply(labs, htmltools::HTML),
          group  = "Radial", stroke = FALSE
        ) %>%
        addLayersControl(
          baseGroups = c("Mapa de Calor", "Radial"),
          options    = layersControlOptions(collapsed = TRUE)
        )
    })
    
    ## Tabla detalle por municipio ----
    output$DetalleMapa <- DT::renderDataTable({
      aux1 <- entradas_r() %>%
        group_by(Mun, NomDepPro, MunPro) %>%
        summarise(
          Entradas       = n(),
          Kilos          = sum(KilosNetos),
          FactorPromedio = mean(FactorRendimiento, na.rm = TRUE),
          .groups = "drop"
        )
      
      DT::datatable(
        aux1,
        rownames  = FALSE,
        colnames  = c("Mpio. Cliente", "Depto. Procedencia",
                      "Mpio. Procedencia", "Entradas", "Kilos", "Factor Prom."),
        options   = list(
          pageLength = 6, dom = "tp", searching = FALSE,
          scrollX = TRUE, ordering = TRUE
        )
      ) %>%
        DT::formatRound(4:5, digits = 0) %>%
        DT::formatRound(6, digits = 2) %>%
        DT::formatStyle(1:6, lineHeight = "90%")
    })
    
    ## Función interna: serie mensual de métrica de calidad ----
    serie_calidad <- function(df, var_y, etiqueta_y, titulo) {
      aux1 <- df %>%
        group_by(Fecha = floor_date(RegFchEnt, unit = "month")) %>%
        summarise(
          Valor = weighted.mean(.data[[var_y]], KilosNetos, na.rm = TRUE),
          .groups = "drop"
        )
      
      plot_ly(
        data = aux1, x = ~Fecha, y = ~Valor,
        type = "scatter", mode = "lines+markers",
        line = list(width = 2, color = "#212F3D"),
        marker = list(size = 5, color = "#212F3D"),
        name = etiqueta_y,
        hoverinfo = "text", hoverlabel = list(align = "left"),
        hovertext = paste0(
          "<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
          "<br>", etiqueta_y, ": ", comma(aux1$Valor, accuracy = 0.01)
        )
      ) %>%
        layout(
          title  = list(text = titulo,
                        font = list(family = "Arial, sans-serif", size = 18)),
          xaxis  = list(title = "Fecha", gridcolor = "#CCD1D1"),
          yaxis  = list(title = etiqueta_y, rangemode = "tozero", tickformat = "s",
                        gridcolor = "#CCD1D1"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    }
    
    ## Función interna: caja de distribución de métrica de calidad ----
    caja_calidad <- function(df, var_y, formato_y = ",") {
      aux1 <- df %>% transmute(Var = .data[[var_y]])
      plot_ly(
        data = aux1, y = ~Var, type = "box",
        box = list(visible = TRUE), meanline = list(visible = TRUE),
        points = FALSE, name = "_"
      ) %>%
        layout(
          yaxis = list(tickformat = formato_y, rangemode = "tozero", title = ""),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    }
    
    output$SerieFactorRendimiento <- renderPlotly({
      serie_calidad(entradas_r(), "FactorRendimiento", "Factor", "Factor de Rendimiento")
    })
    output$CajaFactorRendimiento <- renderPlotly({
      caja_calidad(entradas_r(), "FactorRendimiento")
    })
    
    output$SerieHumedad <- renderPlotly({
      serie_calidad(entradas_r(), "EvaPorHum", "Humedad (%)", "Humedad")
    })
    output$CajaHumedad <- renderPlotly({
      caja_calidad(entradas_r(), "EvaPorHum")
    })
    
    ## Serie de granulometría (cuatro componentes) ----
    output$SerieGranulometria <- renderPlotly({
      aux1 <- entradas_r() %>%
        group_by(Fecha = floor_date(RegFchEnt, unit = "month")) %>%
        summarise(
          PctExcelso = mean(PctExcelso),
          PctPasilla = mean(PctPasilla),
          PctRipio   = mean(PctRipio),
          PctMerma   = mean(PctMerma),
          .groups = "drop"
        ) %>%
        pivot_longer(
          PctExcelso:PctMerma, names_to = "Resultado", values_to = "Valor"
        ) %>%
        mutate(Resultado = gsub("Pct", "", Resultado))
      
      plot_ly(
        data = aux1, x = ~Fecha, y = ~Valor, color = ~Resultado,
        type = "scatter", mode = "lines+markers",
        line = list(width = 2), marker = list(size = 5),
        hoverinfo = "text", hoverlabel = list(align = "left"),
        hovertext = paste0(
          "<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
          "<br>", aux1$Resultado, ": ", percent(aux1$Valor, accuracy = 0.01)
        )
      ) %>%
        layout(
          title  = list(text = "Granulometría",
                        font = list(family = "Arial, sans-serif", size = 18)),
          xaxis  = list(title = "Fecha", gridcolor = "#CCD1D1"),
          yaxis  = list(tickformat = ".0%", rangemode = "tozero", title = "",
                        gridcolor = "#CCD1D1"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.22)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    ## Cajas de distribución por componente granulométrico ----
    output$CajaGranulometria <- renderPlotly({
      aux1 <- entradas_r() %>%
        select(PctExcelso, PctPasilla, PctRipio, PctMerma) %>%
        pivot_longer(
          PctExcelso:PctMerma, names_to = "Resultado", values_to = "Var"
        ) %>%
        mutate(Resultado = gsub("Pct", "", Resultado))
      
      plot_ly(
        data = aux1, x = ~Resultado, y = ~Var, type = "box",
        split = ~Resultado, box = list(visible = TRUE),
        meanline = list(visible = TRUE), points = FALSE
      ) %>%
        layout(
          yaxis = list(tickformat = ".0%", rangemode = "tozero", title = ""),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}