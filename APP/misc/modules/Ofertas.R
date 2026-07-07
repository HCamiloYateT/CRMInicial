# Módulo: Ofertas ----
# Análisis completo de ofertas: KPIs, Sankey, cosechas y alturas de mora

# UI ----

mod_ofertas_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    ## Fila 1: KPIs con CajaModal ----
    fluidRow(
      column(width = 3, CajaModalUI(ns("kpi_num_ofertas"))),
      column(width = 3, CajaModalUI(ns("kpi_kls_ofertas"))),
      column(width = 3, CajaModalUI(ns("kpi_valor_ofertas"))),
      column(width = 3, CajaModalUI(ns("kpi_anticipos_ofertas")))
    ),
    
    ## Fila 2: Score cumplimiento + Sankey kilos ----
    fluidRow(
      
      column(
        width = 4,
        bs4Card(
          title       = "Score de Cumplimiento",
          width       = 12,
          status      = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput(ns("ScoreCumplimiento"), height = "200px")
        )
      ),
      
      column(
        width = 8,
        bs4Card(
          title       = "Distribución por Sucursal y Estado (Kilos)",
          width       = 12,
          status      = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput(ns("SankeyKilos"), height = "300px")
        )
      )
    ),
    
    ## Fila 3: Sankey anticipos + tabla cosechas ----
    fluidRow(
      
      column(
        width = 8,
        bs4Card(
          title       = "Distribución por Sucursal y Estado (Anticipos)",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("SankeyAnticipos"), height = "300px")
        )
      ),
      
      column(
        width = 4,
        bs4Card(
          title       = "Resumen de Cosechas",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          tableOutput(ns("ResumenCosechas"))
        )
      )
    ),
    
    ## Fila 4: Curvas de cosecha ----
    fluidRow(
      
      column(
        width = 6,
        bs4Card(
          title       = "Cosechas – Kilos",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("CosechasKilos"), height = "300px")
        )
      ),
      
      column(
        width = 6,
        bs4Card(
          title       = "Cosechas – Anticipos",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("CosechasAnticipos"), height = "300px")
        )
      )
    ),
    
    ## Fila 5: Alturas de mora ----
    fluidRow(
      
      column(
        width = 12,
        bs4Card(
          title       = "Alturas de Mora por Oferta",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("BarrasAlturas"), height = "300px")
        )
      )
    ),
    
    ## Fila 6: Alturas de mora por cosecha (kilos y anticipos) ----
    fluidRow(
      
      column(
        width = 6,
        bs4Card(
          title       = "Alturas de Mora por Cosecha – Kilos",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("BarrasAlturasVintKls"), height = "300px")
        )
      ),
      
      column(
        width = 6,
        bs4Card(
          title       = "Alturas de Mora por Cosecha – Anticipos",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("BarrasAlturasVintAnt"), height = "300px")
        )
      )
    )
  )
}

# Server ----

mod_ofertas_server <- function(id, ofertas_r, liquidacion_r, rfm_r) {
  moduleServer(id, function(input, output, session) {
    
    ## KPIs con CajaModal ----
    
    CajaModal(
      id            = "kpi_num_ofertas",
      valor         = reactive(comma(nrow(ofertas_r()), accuracy = 1)),
      texto         = "Ofertas",
      icono         = "bars",
      mostrar_boton = TRUE,
      contenido_modal = function() {
        div(DT::dataTableOutput(session$ns("dt_ofertas_detalle")))
      },
      titulo_modal = "Detalle de Ofertas"
    )
    
    output$dt_ofertas_detalle <- DT::renderDataTable({
      DT::datatable(
        ofertas_r() %>%
          select(OfeNro, OfeEst, OfeFch, OfeTotKls, AnticiposGirados) %>%
          arrange(desc(OfeFch)),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })
    
    CajaModal(
      id    = "kpi_kls_ofertas",
      valor = reactive(comma(sum(ofertas_r()$OfeTotKls, na.rm = TRUE), accuracy = 1)),
      texto = "Kilos Ofertados",
      icono = "weight-hanging"
    )
    
    CajaModal(
      id    = "kpi_valor_ofertas",
      valor = reactive(dollar(sum(ofertas_r()$ValorOfe, na.rm = TRUE), accuracy = 1)),
      texto = "Valor de las Ofertas",
      icono = "money-bills"
    )
    
    CajaModal(
      id    = "kpi_anticipos_ofertas",
      valor = reactive(
        dollar(sum(ofertas_r()$AnticiposGirados, na.rm = TRUE), accuracy = 1)
      ),
      texto = "Anticipos Girados",
      icono = "hand-holding-dollar"
    )
    
    ## Gauge de score de cumplimiento ----
    output$ScoreCumplimiento <- renderPlotly({
      val <- if (nrow(rfm_r()) > 0) as.numeric(rfm_r()$ScoreCumplimiento[1]) else 0
      
      # Usar ImprimirGauge de racafeGraph si está disponible; si no, gauge nativo
      if (exists("ImprimirGauge", mode = "function")) {
        ImprimirGauge(val = val, limites = c(400, 700), rango = c(0, 1000),
                      Directo = FALSE, formato = "numero")
      } else {
        color_barra <- dplyr::case_when(val < 400 ~ "#C0392B", val < 700 ~ "#D4AC0D",
                                        TRUE ~ "#1E8449")
        plot_ly(
          type = "indicator", mode = "gauge+number", value = val,
          number = list(font = list(size = 24, color = color_barra)),
          gauge  = list(
            axis  = list(range = list(0, 1000), tickwidth = 1),
            bar   = list(color = color_barra),
            steps = list(list(range = c(0,   400), color = "#FADBD8"),
                         list(range = c(400, 700), color = "#FDEBD0"),
                         list(range = c(700, 1000), color = "#D5F5E3")),
            threshold = list(line = list(color = "black", width = 3),
                             thickness = 0.75, value = val)
          )
        ) %>%
          layout(margin = list(l = 20, r = 20, t = 40, b = 20),
                 paper_bgcolor = "rgba(0,0,0,0)") %>%
          config(displayModeBar = FALSE)
      }
    })
    
    ## Sankey helper: construye nodos y links para un Varplot dado ----
    construir_sankey <- function(df, Varplot) {
      aux1 <- bind_rows(
        df %>%
          group_by(Origen = "TOTAL", Destino = Sucursal) %>%
          summarise(
            Ofertas    = n(),
            Kilos      = sum(OfeTotKls, na.rm = TRUE),
            Anticipos  = sum(AnticiposGirados, na.rm = TRUE),
            .groups = "drop"
          ),
        df %>%
          group_by(Origen = Sucursal, Destino = OfeEst) %>%
          summarise(
            Ofertas    = n(),
            Kilos      = sum(OfeTotKls, na.rm = TRUE),
            Anticipos  = sum(AnticiposGirados, na.rm = TRUE),
            .groups = "drop"
          )
      ) %>%
        mutate(
          PctOfertas   = Ofertas  / sum(Ofertas[Origen == "TOTAL"]),
          PctKilos     = Kilos    / sum(Kilos[Origen == "TOTAL"]),
          PctAnticipos = Anticipos / sum(Anticipos[Origen == "TOTAL"]),
          Var          = !!sym(Varplot)
        )
      
      n_seq <- length(unique(aux1$Destino))
      
      info_nodo <- function(o, k, a, po, pk, pa) {
        paste0(
          "<b>Ofertas: </b>", comma(o, accuracy = 1),
          " <b>(", percent(po, accuracy = 0.01), ")</b>",
          "<br><b>Kilos: </b>", comma(k, accuracy = 1),
          " <b>(", percent(pk, accuracy = 0.01), ")</b>",
          "<br><b>Anticipos: </b>", comma(a, accuracy = 1),
          " <b>(", percent(pa, accuracy = 0.01), ")</b>"
        )
      }
      
      Nodes <- bind_cols(
        node = seq(0, n_seq),
        bind_rows(
          aux1 %>%
            filter(Origen == "TOTAL") %>%
            group_by(name = Origen) %>%
            summarise(
              Ofertas = sum(Ofertas), Kilos = sum(Kilos), Anticipos = sum(Anticipos),
              PctOfertas = sum(PctOfertas), PctKilos = sum(PctKilos),
              PctAnticipos = sum(PctAnticipos)
            ) %>%
            mutate(
              color = "black",
              info  = info_nodo(Ofertas, Kilos, Anticipos,
                                PctOfertas, PctKilos, PctAnticipos)
            ) %>%
            select(name, color, info),
          aux1 %>%
            group_by(name = Destino) %>%
            summarise(
              Ofertas = sum(Ofertas), Kilos = sum(Kilos), Anticipos = sum(Anticipos),
              PctOfertas = sum(PctOfertas), PctKilos = sum(PctKilos),
              PctAnticipos = sum(PctAnticipos)
            ) %>%
            mutate(
              color = recode(name, !!!c(colores_sucursal, colores_estado)),
              info  = info_nodo(Ofertas, Kilos, Anticipos,
                                PctOfertas, PctKilos, PctAnticipos)
            ) %>%
            select(name, color, info)
        )
      )
      
      Links <- aux1 %>%
        left_join(Nodes, by = c("Origen" = "name")) %>%
        left_join(Nodes, by = c("Destino" = "name")) %>%
        mutate(
          info = paste0(
            "<b>De: ", Origen, " a ", Destino, "</b>",
            "<br><b>Kilos: </b>", comma(Kilos, accuracy = 1),
            " <b>(", percent(PctKilos, accuracy = 0.01), ")</b>"
          )
        ) %>%
        select(source = node.x, target = node.y, value = Var, info)
      
      plot_ly(
        type        = "sankey",
        orientation = "h",
        arrangement = "fixed",
        node = list(
          label        = Nodes$name,
          color        = Nodes$color,
          pad          = 15, thickness = 20,
          customdata   = Nodes$info,
          hoverlabel   = list(align = "left"),
          hovertemplate = "<b>%{label}</b><br>%{customdata}<extra></extra>"
        ),
        link = list(
          source        = Links$source,
          target        = Links$target,
          value         = Links$value,
          customdata    = Links$info,
          hoverlabel    = list(align = "left"),
          hovertemplate = "%{customdata}<extra></extra>"
        )
      ) %>%
        layout(
          title  = paste("Distribución de Ofertas –", Varplot),
          font   = list(size = 10)
        ) %>%
        config(displayModeBar = FALSE)
    }
    
    output$SankeyKilos     <- renderPlotly({ construir_sankey(ofertas_r(), "Kilos") })
    output$SankeyAnticipos <- renderPlotly({ construir_sankey(ofertas_r(), "Anticipos") })
    
    ## Tabla resumen de cosechas por vintage ----
    output$ResumenCosechas <- renderTable({
      ofertas_r() %>%
        filter(OfeEst == "Cumplida") %>%
        mutate(
          FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, OfeFch),
          FechaCumplimiento = if_else(
            FechaCumplimiento == as.Date("1753-01-01"), OfeFch, FechaCumplimiento
          ),
          Vintage = .vintage_label(FechaCumplimiento)
        ) %>%
        arrange(.vintage_orden(Vintage)) %>%
        group_by(Cosecha = Vintage) %>%
        summarise(
          Ofertas   = comma(n_distinct(OfeNro), accuracy = 1),
          Kilos     = comma(sum(OfeTotKls), accuracy = 1),
          Anticipos = dollar(sum(AnticiposGirados, na.rm = TRUE), accuracy = 1)
        )
    }, spacing = "xs", width = "100%")
    
    ## Curvas de cosecha kilos y anticipos ----
    output$CosechasKilos     <- renderPlotly({
      construir_cosechas(liquidacion_r(), "Kilos",     paleta)
    })
    output$CosechasAnticipos <- renderPlotly({
      construir_cosechas(liquidacion_r(), "Anticipos", paleta)
    })
    
    ## Función auxiliar: datos de alturas de mora ----
    datos_alturas <- function(df, varplot) {
      aux1 <- df %>%
        filter(!is.na(Fecha)) %>%
        mutate(
          FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
          FechaCumplimiento = if_else(
            FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento
          ),
          TOB = pmax(0, as.numeric(difftime(
            as.Date(Fecha), as.Date(FechaCumplimiento), units = "days"
          ))),
          Vintage = .vintage_label(FechaCumplimiento)
        ) %>%
        filter(!Vintage %in% c("1973", "1753")) %>%
        group_by(SucCod, OfeNro) %>%
        mutate(KilosAcum = cumsum(kilos)) %>%
        ungroup()
      
      aux1 %>%
        group_by(SucCod, OfeNro, Vintage) %>%
        summarise(
          Altura        = max(TOB),
          KilosOriginal = max(KilosOriginal),
          Anticipos     = sum(AntGirado),
          .groups = "drop"
        ) %>%
        mutate(
          Bucket = case_when(
            Altura <= 0  ~ "Oferta al día",
            Altura <= 7  ~ "De 1 a 7 días",
            Altura <= 15 ~ "De 8 a 15 días",
            Altura <= 30 ~ "De 16 a 30 días",
            Altura <= 60 ~ "De 31 a 60 días",
            Altura <= 90 ~ "De 61 a 90 días",
            TRUE         ~ "Más de 90 días"
          )
        )
    }
    
    ## Barras de altura de mora (total) ----
    output$BarrasAlturas <- renderPlotly({
      aux2 <- datos_alturas(liquidacion_r() %>% filter(OfeEst == "Cumplida"), "Kilos")
      
      resumen <- aux2 %>%
        mutate(TotOfertas = n(), TotKilos = sum(KilosOriginal),
               TotAnticipos = sum(Anticipos)) %>%
        group_by(Bucket) %>%
        summarise(
          Ofertas      = n(),
          PctOfertas   = Ofertas / unique(TotOfertas),
          Kilos        = sum(KilosOriginal),
          PctKilos     = Kilos / unique(TotKilos),
          Anticipos    = sum(Anticipos),
          PctAnticipos = Anticipos / unique(TotAnticipos)
        )
      
      resumen$Bucket <- factor(
        resumen$Bucket,
        levels = c("Oferta al día", "De 1 a 7 días", "De 8 a 15 días",
                   "De 16 a 30 días", "De 31 a 60 días",
                   "De 61 a 90 días", "Más de 90 días"),
        ordered = TRUE
      )
      
      subplot(
        plot_ly(resumen, x = ~Ofertas, y = ~Bucket, type = "bar",
                marker = list(color = "#1F618D"),
                hovertext = paste0("<b>", resumen$Bucket, "</b>",
                                   "<br>Ofertas: ", comma(resumen$Ofertas),
                                   " <b>(", percent(resumen$PctOfertas, accuracy = 0.1), ")</b>")),
        plot_ly(resumen, x = ~Kilos, y = ~Bucket, type = "bar",
                marker = list(color = "#117A65"),
                hovertext = paste0("<b>", resumen$Bucket, "</b>",
                                   "<br>Kilos: ", comma(resumen$Kilos),
                                   " <b>(", percent(resumen$PctKilos, accuracy = 0.1), ")</b>")),
        plot_ly(resumen, x = ~Anticipos, y = ~Bucket, type = "bar",
                marker = list(color = "#B03A2E"),
                hovertext = paste0("<b>", resumen$Bucket, "</b>",
                                   "<br>Anticipos: ", dollar(resumen$Anticipos),
                                   " <b>(", percent(resumen$PctAnticipos, accuracy = 0.1), ")</b>")),
        nrows = 1, shareY = TRUE, titleX = TRUE
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    ## Barras de altura por cosecha: helper reutilizable ----
    render_alturas_vintage <- function(df_liq, varplot) {
      aux2 <- datos_alturas(df_liq %>% filter(OfeEst == "Cumplida"), varplot)
      
      resumen <- aux2 %>%
        group_by(Vintage) %>%
        mutate(TotOfertas = n(), TotKilos = sum(KilosOriginal),
               TotAnticipos = sum(Anticipos)) %>%
        group_by(Bucket, Vintage) %>%
        summarise(
          Ofertas      = n(),
          PctOfertas   = Ofertas / unique(TotOfertas),
          Kilos        = sum(KilosOriginal),
          PctKilos     = Kilos / unique(TotKilos),
          Anticipos    = sum(Anticipos),
          PctAnticipos = Anticipos / unique(TotAnticipos),
          .groups = "drop"
        ) %>%
        mutate(VarPct = !!sym(paste0("Pct", varplot)))
      
      resumen$Bucket <- factor(
        resumen$Bucket,
        levels = c("Oferta al día", "De 1 a 7 días", "De 8 a 15 días",
                   "De 16 a 30 días", "De 31 a 60 días",
                   "De 61 a 90 días", "Más de 90 días"),
        ordered = TRUE
      )
      # Ordenar vintages cronológicamente para eje X del bar chart
      resumen$Vintage <- .vintage_factor(resumen$Vintage)
      
      col <- rev(RColorBrewer::brewer.pal(11, "RdYlGn"))
      
      plot_ly(
        resumen, x = ~Vintage, y = ~VarPct, color = ~Bucket, type = "bar",
        colors = col, hoverinfo = "text", hoverlabel = list(align = "left"),
        hovertext = paste0(
          "Cosecha: ", resumen$Vintage,
          "<br>Altura: ", resumen$Bucket,
          "<br>Kilos: ", comma(resumen$Kilos),
          " <b>(", percent(resumen$PctKilos, accuracy = 0.1), ")</b>",
          "<br>Anticipos: ", dollar(resumen$Anticipos),
          " <b>(", percent(SiError_0(resumen$PctAnticipos), accuracy = 0.1), ")</b>"
        )
      ) %>%
        layout(
          barmode = "stack",
          yaxis   = list(tickformat = ".0%", title = ""),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.07)
        ) %>%
        config(displayModeBar = FALSE)
    }
    
    output$BarrasAlturasVintKls <- renderPlotly({
      render_alturas_vintage(liquidacion_r(), "Kilos")
    })
    output$BarrasAlturasVintAnt <- renderPlotly({
      render_alturas_vintage(liquidacion_r(), "Anticipos")
    })
  })
}