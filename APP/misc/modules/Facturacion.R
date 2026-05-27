# Módulo: Facturación ----
# Análisis de ventas: KPIs RFM, distribuciones y series con forecast Prophet

# UI ----

mod_facturacion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    ## Fila 1: KPIs de volumen ----
    fluidRow(
      column(width = 4, CajaModalUI(ns("kpi_num_facturas"))),
      column(width = 4, CajaModalUI(ns("kpi_kls_facturas"))),
      column(width = 4, CajaModalUI(ns("kpi_valor_facturas")))
    ),
    
    ## Fila 2: KPIs de scores RFM ----
    fluidRow(
      column(width = 4, CajaModalUI(ns("kpi_recencia"))),
      column(width = 4, CajaModalUI(ns("kpi_frecuencia"))),
      column(width = 4, CajaModalUI(ns("kpi_monto")))
    ),
    
    ## Fila 3: Distribuciones por sucursal y negocio ----
    fluidRow(
      
      column(
        width = 6,
        bs4Card(
          title       = "Distribución por Sucursal",
          width       = 12,
          status      = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput(ns("DistribuicionSucursal"), height = "280px")
        )
      ),
      
      column(
        width = 6,
        bs4Card(
          title       = "Distribución por Tipo de Negocio",
          width       = 12,
          status      = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput(ns("DistribuicionNegocio"), height = "280px")
        )
      )
    ),
    
    ## Fila 4: Series con forecast Prophet ----
    fluidRow(
      
      column(
        width = 6,
        bs4Card(
          title       = "Serie de Kilos + Proyección",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          dygraphOutput(ns("SerieFacturasKilos"), height = "280px")
        )
      ),
      
      column(
        width = 6,
        bs4Card(
          title       = "Serie de Valor + Proyección",
          width       = 12,
          status      = "white",
          solidHeader = FALSE,
          collapsible = TRUE,
          dygraphOutput(ns("SerieFacturasValor"), height = "280px")
        )
      )
    )
  )
}

# Server ----

mod_facturacion_server <- function(id, facturas_r, rfm_r) {
  moduleServer(id, function(input, output, session) {
    
    ## KPIs de volumen con CajaModal ----
    
    CajaModal(
      id            = "kpi_num_facturas",
      valor         = reactive({
        comma(
          facturas_r() %>% select(SucCod, FCoNro) %>% distinct() %>% nrow(),
          accuracy = 1
        )
      }),
      texto         = "Facturas",
      icono         = "bars",
      mostrar_boton = TRUE,
      contenido_modal = function() {
        div(DT::dataTableOutput(session$ns("dt_facturas_detalle")))
      },
      titulo_modal = "Detalle de Facturas"
    )
    
    output$dt_facturas_detalle <- DT::renderDataTable({
      DT::datatable(
        facturas_r() %>%
          select(FCoNro, FCoFch, Sucursal, TNeCod, kilos, ValorFacturado) %>%
          arrange(desc(FCoFch)),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })
    
    CajaModal(
      id    = "kpi_kls_facturas",
      valor = reactive(comma(sum(facturas_r()$kilos, na.rm = TRUE), accuracy = 1)),
      texto = "Kilos Facturados",
      icono = "weight-hanging"
    )
    
    CajaModal(
      id    = "kpi_valor_facturas",
      valor = reactive(
        dollar(sum(facturas_r()$ValorFacturado, na.rm = TRUE), accuracy = 1)
      ),
      texto = "Valor Facturado",
      icono = "money-bills"
    )
    
    ## KPIs de scores RFM ----
    
    CajaModal(
      id    = "kpi_recencia",
      valor = reactive(comma(rfm_r()$recency_score, accuracy = 1)),
      texto = "Score de Recencia",
      icono = "arrow-up-wide-short"
    )
    
    CajaModal(
      id    = "kpi_frecuencia",
      valor = reactive(comma(rfm_r()$frequency_score, accuracy = 1)),
      texto = "Score de Frecuencia",
      icono = "arrow-up-wide-short"
    )
    
    CajaModal(
      id    = "kpi_monto",
      valor = reactive(comma(rfm_r()$monetary_score, accuracy = 1)),
      texto = "Score de Volumen",
      icono = "arrow-up-wide-short"
    )
    
    ## Distribución por sucursal (dona) ----
    output$DistribuicionSucursal <- renderPlotly({
      facturas_r() %>%
        group_by(Sucursal) %>%
        summarise(Kilos = sum(kilos)) %>%
        plot_ly() %>%
        add_pie(
          labels = ~Sucursal, values = ~Kilos,
          type = "pie", hole = 0.5, sort = FALSE,
          marker = list(line = list(width = 2))
        ) %>%
        layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.07)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    ## Distribución por tipo de negocio (dona) ----
    output$DistribuicionNegocio <- renderPlotly({
      facturas_r() %>%
        group_by(TNeCod) %>%
        summarise(Kilos = sum(kilos)) %>%
        mutate(
          TNeCod = recode(TNeCod,
                          OFE = "Ofertas", DIA = "Café al día", CSN = "Café sin Negociar"
          )
        ) %>%
        plot_ly() %>%
        add_pie(
          labels = ~TNeCod, values = ~Kilos,
          type = "pie", hole = 0.5, sort = FALSE,
          marker = list(line = list(width = 2))
        ) %>%
        layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.07)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    ## Función interna: serie mensual con forecast Prophet + dygraph ----
    serie_prophet_dygraph <- function(df, varplot) {
      formato <- if (varplot == "Kilos") "coma" else "dinero"
      Form    <- FormatoJS(formato)
      
      aux1 <- df %>%
        group_by(Fecha = floor_date(as.Date(FCoFch), unit = "month")) %>%
        summarise(
          Kilos = sum(kilos),
          Valor = sum(ValorFacturado),
          .groups = "drop"
        ) %>%
        mutate(Var = !!sym(varplot))
      
      # Requiere mínimo 6 observaciones para ajustar el modelo
      if (nrow(aux1) < 6) {
        ts_simple <- xts::xts(matrix(aux1$Var, ncol = 1), order.by = aux1$Fecha)
        colnames(ts_simple) <- varplot
        return(dygraph(ts_simple) %>%
                 dySeries(varplot, color = "#212F3D", strokeWidth = 2) %>%
                 dyAxis("y", valueFormatter = Form, axisLabelFormatter = Form) %>%
                 dyOptions(drawPoints = TRUE, pointSize = 2, gridLineColor = "#CCD1D1") %>%
                 dyLegend(show = "auto"))
      }
      
      resultado <- tryCatch({
        m_prophet <- aux1 %>%
          select(ds = Fecha, y = Var) %>%
          prophet(interval.width = 0.80, yearly.seasonality = TRUE,
                  weekly.seasonality = FALSE, daily.seasonality = FALSE)
        
        future   <- make_future_dataframe(m_prophet, periods = 12,
                                          freq = "month", include_history = TRUE)
        forecast <- predict(m_prophet, future)
        
        # Construir dygraph manualmente: evita incompatibilidades de dyplot.prophet
        real_ts <- xts::xts(aux1$Var, order.by = aux1$Fecha)
        pred_ts <- xts::xts(
          forecast %>%
            select(yhat_lower, yhat, yhat_upper) %>%
            as.matrix(),
          order.by = as.Date(forecast$ds)
        )
        colnames(real_ts) <- "Real"
        colnames(pred_ts) <- c("Inferior", "Proyectado", "Superior")
        combined <- merge(real_ts, pred_ts, join = "outer")
        
        dygraph(combined) %>%
          dySeries("Real",                                     color = "#212F3D",
                   strokeWidth = 2, drawPoints = TRUE, pointSize = 3) %>%
          dySeries(c("Inferior", "Proyectado", "Superior"),   color = "#1F618D",
                   strokeWidth = 1) %>%
          dyAxis("x", drawGrid = FALSE,
                 valueFormatter     = "function(d){return moment(d).format('MMM-YY');}",
                 axisLabelFormatter = "function(d){return moment(d).format('MMM-YY');}") %>%
          dyAxis("y", label = varplot, axisLabelWidth = 90,
                 valueFormatter = Form, axisLabelFormatter = Form) %>%
          dyOptions(digitsAfterDecimal = 0, gridLineColor = "#CCD1D1",
                    strokeWidth = 2) %>%
          dyLegend(show = "auto")
        
      }, error = function(e) {
        message("[Facturacion] Prophet falló para ", varplot, ": ", e$message)
        ts_simple <- xts::xts(matrix(aux1$Var, ncol = 1), order.by = aux1$Fecha)
        colnames(ts_simple) <- varplot
        dygraph(ts_simple) %>%
          dySeries(varplot, color = "#212F3D", strokeWidth = 2) %>%
          dyAxis("y", valueFormatter = Form, axisLabelFormatter = Form) %>%
          dyOptions(drawPoints = TRUE, pointSize = 2, gridLineColor = "#CCD1D1") %>%
          dyLegend(show = "auto")
      })
      resultado %>%
        dyAxis(
          "x", drawGrid = FALSE,
          valueFormatter    = 'function(d){return moment(d).format("MMM-YY");}',
          axisLabelFormatter = 'function(d){return moment(d).format("MMM-YY");}'
        ) %>%
        dyAxis("y",
               label            = varplot,
               axisLabelWidth   = 90,
               valueFormatter   = Form,
               axisLabelFormatter = Form
        ) %>%
        dyOptions(
          drawPoints         = TRUE,
          pointSize          = 2,
          strokeWidth        = 2,
          digitsAfterDecimal = 0,
          colors             = c("#212F3D", "#196F3D"),
          gridLineColor      = "#CCD1D1"
        )
    }
    
    output$SerieFacturasKilos <- renderDygraph({
      serie_prophet_dygraph(facturas_r(), "Kilos")
    })
    
    output$SerieFacturasValor <- renderDygraph({
      serie_prophet_dygraph(facturas_r(), "Valor")
    })
  })
}