mod_resumen_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    ## Fila 1: Datos de contacto + radar RFM ----
    fluidRow(
      
      column(
        width = 4,
        bs4Card(
          title       = "Información de Contacto",
          width       = 12,
          status      = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          tableOutput(ns("TablaContacto"))
        )
      ),
      
      column(
        width = 4,
        bs4Card(
          title       = "Perfil RFM",
          width       = 12,
          status      = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput(ns("RadarResumen"), height = "280px")
        )
      ),
      
      column(
        width = 4,
        bs4Card(
          title       = "Resumen de Ofertas",
          width       = 12,
          status      = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          tableOutput(ns("TablaResumenOfertas"))
        )
      )
    ),
    
    ## Fila 2: KPIs rápidos con CajaModal ----
    fluidRow(
      column(width = 3, CajaModalUI(ns("kpi_res_ofertas"))),
      column(width = 3, CajaModalUI(ns("kpi_res_kls_ofertas"))),
      column(width = 3, CajaModalUI(ns("kpi_res_anticipos"))),
      column(width = 3, CajaModalUI(ns("kpi_res_facturas")))
    ),
    
    ## Fila 3: Series históricas de ofertas y facturas ----
    fluidRow(
      
      column(
        width = 6,
        bs4Card(
          title       = "Evolución de Ofertas",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("SerieResumenOfertas"), height = "240px")
        )
      ),
      
      column(
        width = 6,
        bs4Card(
          title       = "Evolución de Facturación",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("SerieResumenFacturas"), height = "240px")
        )
      )
    ),
    
    ## Fila 4: Cosechas de ofertas ----
    fluidRow(
      column(
        width = 12,
        bs4Card(
          title       = "Cosechas de Ofertas (Kilos)",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotlyOutput(ns("ResumenCosechasKilos"), height = "300px")
        )
      )
    )
  )
}
mod_resumen_server <- function(id, ofertas_r, rfm_r, liquidacion_r,
                               facturas_r, entradas_r) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Tabla de datos de contacto del cliente ----
    output$TablaContacto <- renderTable({
      req(nrow(ofertas_r()) > 0)
      ofertas_r() %>%
        select(
          Nombre          = PerRazSoc,
          `Tipo de Persona` = PerTipPer,
          Departamento    = NomDep,
          Municipio       = Mun,
          `Dirección`     = PerDir,
          `Teléfono`      = PerTel
        ) %>%
        distinct() %>%
        pivot_longer(Nombre:`Teléfono`, names_to = "Item", values_to = "Registro")
    }, spacing = "xs", width = "100%")
    
    ## Radar de scores RFM comparado con el promedio general ----
    output$RadarResumen <- renderPlotly({
      req(nrow(ofertas_r()) > 0)
      
      scores_cliente <- rfm_r() %>%
        slice(1) %>%
        select(ScoreCumplimiento:ScoreRecompra)
      
      scores_general <- ResultadosRFM %>%
        select(ScoreCumplimiento:ScoreRecompra) %>%
        summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
      
      dimensiones <- c(
        "Cumplimiento", "Frecuencia", "Recencia",
        "Volumen", "Fuga", "Sobrevida", "Recompra"
      )
      
      plot_ly(type = "scatterpolar", mode = "markers", fill = "toself") %>%
        add_trace(
          r         = as.numeric(scores_cliente),
          theta     = dimensiones,
          name      = unique(rfm_r()$customer_id),
          color     = I("#5DADE2"),
          hoverinfo = "text",
          hovertext = paste0(
            "<b>", unique(rfm_r()$customer_id), "</b>",
            "<br><b>", dimensiones, ": </b>",
            comma(as.numeric(scores_cliente))
          )
        ) %>%
        add_trace(
          r         = as.numeric(scores_general),
          theta     = dimensiones,
          name      = "General",
          color     = I("#99A3A4"),
          hoverinfo = "text",
          hovertext = paste0(
            "<b>General</b>",
            "<br><b>", dimensiones, ": </b>",
            comma(as.numeric(scores_general))
          )
        ) %>%
        layout(
          margin = m,
          legend = list(
            orientation = "h", xanchor = "center",
            x = 0.5, y = -0.18,
            font = list(family = "Arial, sans-serif", size = 14, color = "black")
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    ## Tabla resumen de ofertas por estado ----
    output$TablaResumenOfertas <- renderTable({
      aux0 <- ofertas_r()
      bind_cols(
        `Ofertas Pendientes` = aux0 %>%
          filter(OfeEst == "Pendiente") %>%
          summarise(paste(OfeNro, collapse = " | ")) %>%
          as.character(),
        `Última Oferta` = aux0 %>%
          arrange(desc(OfeFch)) %>%
          slice(1) %>% select(OfeNro) %>% as.character(),
        `Fecha Última Oferta` = aux0 %>%
          arrange(desc(OfeFch)) %>%
          slice(1) %>% select(OfeFch) %>% format("%d %b %y") %>% as.character(),
        `Kilos Última Oferta` = aux0 %>%
          arrange(desc(OfeFch)) %>%
          slice(1) %>% pull(OfeTotKls) %>% comma() %>% as.character(),
        `Anticipo Última Oferta` = aux0 %>%
          arrange(desc(OfeFch)) %>%
          slice(1) %>% pull(AnticiposGirados) %>% dollar() %>% as.character(),
        `Última Oferta Cumplida` = aux0 %>%
          filter(OfeEst == "Cumplida") %>%
          arrange(desc(OfeFch)) %>%
          slice(1) %>% select(OfeNro) %>% as.character(),
        `Fecha Última Cumplida` = aux0 %>%
          filter(OfeEst == "Cumplida") %>%
          arrange(desc(OfeFch)) %>%
          slice(1) %>% select(OfeFch) %>% format("%d %b %y") %>% as.character()
      ) %>%
        pivot_longer(
          `Ofertas Pendientes`:`Fecha Última Cumplida`,
          names_to  = "Item",
          values_to = "Registro"
        )
    }, spacing = "xs", width = "100%")
    
    ## KPIs con CajaModal ----
    
    CajaModal(
      id     = "kpi_res_ofertas",
      valor  = reactive(comma(nrow(ofertas_r()), accuracy = 1)),
      texto  = "Ofertas Totales",
      icono  = "handshake",
      colores = reactive(c(fondo = "white"))
    )
    
    CajaModal(
      id    = "kpi_res_kls_ofertas",
      valor = reactive(comma(sum(ofertas_r()$OfeTotKls, na.rm = TRUE), accuracy = 1)),
      texto = "Kilos Ofertados",
      icono = "weight-hanging",
      colores = reactive(c(fondo = "white"))
    )
    
    CajaModal(
      id    = "kpi_res_anticipos",
      valor = reactive(dollar(sum(ofertas_r()$AnticiposGirados, na.rm = TRUE))),
      texto = "Anticipos Girados",
      icono = "hand-holding-dollar",
      colores = reactive(c(fondo = "white"))
    )
    
    CajaModal(
      id    = "kpi_res_facturas",
      valor = reactive({
        comma(
          facturas_r() %>% select(SucCod, FCoNro) %>% distinct() %>% nrow(),
          accuracy = 1
        )
      }),
      texto = "Facturas",
      icono = "file-invoice",
      colores = reactive(c(fondo = "white"))
    )
    
    ## Serie histórica de ofertas (kilos + anticipos por mes) ----
    output$SerieResumenOfertas <- renderPlotly({
      aux1 <- ofertas_r() %>%
        group_by(Fecha = floor_date(OfeFch, unit = "month")) %>%
        summarise(
          Kilos                  = sum(OfeTotKls),
          `Anticipo Girado (Miles)` = sum(AnticiposGirados, na.rm = TRUE) / 1000
        )
      
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
        add_trace(
          y    = ~`Anticipo Girado (Miles)`,
          name = "Anticipos (Miles COP)", color = I("#df8879"),
          hovertext = paste0(
            "<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
            "<br>Anticipos: ", comma(aux1$`Anticipo Girado (Miles)`, accuracy = 0.01)
          )
        ) %>%
        layout(
          xaxis = list(title = "Fecha", gridcolor = "#CCD1D1"),
          yaxis = list(tickformat = "s", title = ""),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.22)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    ## Serie histórica de facturación (kilos por mes) ----
    output$SerieResumenFacturas <- renderPlotly({
      aux1 <- facturas_r() %>%
        group_by(Fecha = floor_date(FCoFch, unit = "month")) %>%
        summarise(
          Kilos                    = sum(kilos),
          `Valor Facturado (Miles)` = sum(ValorFacturado, na.rm = TRUE) / 1000
        )
      
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
        add_trace(
          y    = ~`Valor Facturado (Miles)`,
          name = "Valor (Miles COP)", color = I("#df8879"),
          hovertext = paste0(
            "<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
            "<br>Valor: ", comma(aux1$`Valor Facturado (Miles)`, accuracy = 0.01)
          )
        ) %>%
        layout(
          xaxis = list(title = "Fecha", gridcolor = "#CCD1D1"),
          yaxis = list(tickformat = "s", title = ""),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.22)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    ## Cosechas de kilos en resumen (igual que tab ofertas pero compacto) ----
    output$ResumenCosechasKilos <- renderPlotly({
      construir_cosechas(liquidacion_r(), varplot = "Kilos", paleta = paleta)
    })
  })
}

