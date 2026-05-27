function(input, output, session) {

  # Datos reactivos ----
  usuario <- reactive({
    if (is.null(session$user)) "HCYATE" else str_to_upper(session$user)
  })
  grupo <- reactive({
    if (is.null(session$group)) "ANALÍTICA" else stringr::str_to_upper(session$group)
  })
  
  filtrar_por_trilladora <- function(df, campo_sucursal = "Sucursal") {
    reactive({
      req(user_info()$Trilladora)
      if (user_info()$Trilladora == "TODAS") {
        df
      } else {
        df %>% filter(.data[[campo_sucursal]] == user_info()$Trilladora)
      }
    })
  }
  
  liquidacion_u <- filtrar_por_trilladora(Liquidacion)
  ofertas_u     <- filtrar_por_trilladora(Ofertas)
  facturas_u    <- filtrar_por_trilladora(Facturas)
  entradas_u    <- filtrar_por_trilladora(Entradas)
  rfm_u <- reactive({
    req(user_info()$Trilladora)
    if (user_info()$Trilladora == "TODAS") {
      ResultadosRFM
    } else {
      ResultadosRFM %>%
        filter(grepl(user_info()$Trilladora, Sucursales, ignore.case = TRUE))
    }
  })
  
  # Selector de cliente ----
  
  ## Lista de clientes disponibles según filtro de usuario
  clientes_disponibles <- reactive({
    req(credentials()$user_auth)
    sort(unique(ofertas_u()$PerRazSoc))
  })
  
  ## Renderizar el selectInput de clientes en el sidebar
  output$ui_selector_cliente <- renderUI({
    req(credentials()$user_auth)
    selectInput(
      inputId   = "ClienteInicial",
      label     = tags$small("Cliente"),
      choices   = clientes_disponibles(),
      selected  = clientes_disponibles()[1],
      width     = "100%"
    )
  })
  
  # Datos filtrados por cliente seleccionado ----
  
  liquidacion_f <- reactive({
    req(input$ClienteInicial)
    liquidacion_u() %>% filter(PerRazSoc == input$ClienteInicial)
  })
  
  ofertas_f <- reactive({
    req(input$ClienteInicial)
    ofertas_u() %>% filter(PerRazSoc == input$ClienteInicial)
  })
  
  rfm_f <- reactive({
    req(input$ClienteInicial)
    rfm_u() %>% filter(customer_id == input$ClienteInicial)
  })
  
  facturas_f <- reactive({
    req(input$ClienteInicial)
    facturas_u() %>% filter(PerRazSoc == input$ClienteInicial)
  })
  
  entradas_f <- reactive({
    req(input$ClienteInicial)
    entradas_u() %>% filter(PerRazSoc == input$ClienteInicial)
  })
  
  # Alerta de cliente bloqueado ----
  
  output$ui_bloqueado <- renderUI({
    req(input$ClienteInicial)
    id_cliente <- Facturas %>%
      filter(PerRazSoc == input$ClienteInicial) %>%
      select(IdClienteInicial) %>%
      distinct() %>%
      as.numeric()
    
    if (!is.na(id_cliente) && id_cliente %in% bloqueados) {
      div(
        class = "alert alert-danger alert-dismissible mx-3 mt-2",
        icon("ban"), " Cliente bloqueado para transar",
        tags$button(
          type              = "button",
          class             = "close",
          `data-dismiss`    = "alert",
          `aria-label`      = "Close",
          tags$span(`aria-hidden` = "true", HTML("&times;"))
        )
      )
    }
  })
  
  
  
  # Outputs ----
  ## Header ----
  output$user <- renderUI({
    FormatearTexto(paste(usuario()) %>% HTML, negrita = T, tamano_pct = 0.75, alineacion = "center", color = "#999")
  })
  ## Módulos hijos ----
  
  ## Resumen general del cliente
  mod_resumen_server(
    id           = "resumen",
    ofertas_r    = ofertas_f,
    rfm_r        = rfm_f,
    liquidacion_r = liquidacion_f,
    facturas_r   = facturas_f,
    entradas_r   = entradas_f
  )
  
  ## Análisis de ofertas
  mod_ofertas_server(
    id            = "ofertas",
    ofertas_r     = ofertas_f,
    liquidacion_r = liquidacion_f
  )
  
  ## Análisis de facturación
  mod_facturacion_server(
    id         = "facturacion",
    facturas_r = facturas_f,
    rfm_r      = rfm_f
  )
  
  ## Análisis de entradas de café
  mod_entradas_server(
    id         = "entradas",
    entradas_r = entradas_f,
    facturas_r = facturas_f
  )
  
  }

