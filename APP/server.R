function(input, output, session) {
  
  # Identidad del usuario ----
  # En Posit Connect usa session$user; en local cae al usuario por defecto
  usuario <- reactive({
    if (is.null(session$user)) "HCYATE" else str_to_upper(session$user)
  })
  grupo <- reactive({
    if (is.null(session$group)) "ANALÍTICA" else str_to_upper(session$group)
  })
  
  # Credenciales y perfil de acceso ----
  # credentials: siempre autenticado (sin shinyauthr); compatible con la firma esperada
  # user_info:   busca la trilladora asignada en tabla_usuarios (parametros.R)
  # En producción con SSO: reemplazar ambos reactivos por shinyauthr::loginServer()
  credentials <- reactive({ list(user_auth = TRUE) })
  
  user_info <- reactive({
    usr  <- usuario()
    fila <- tabla_usuarios %>% filter(toupper(usuario) == usr)
    if (nrow(fila) == 0) list(Trilladora = "TODAS") else as.list(fila[1, ])
  })
  
  # Filtro por trilladora según perfil del usuario ----
  # Encapsula la lógica de acceso: "TODAS" devuelve el data frame completo
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
  
  # Datos filtrados por trilladora ----
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
  
  ## Lista de clientes disponibles según filtro de trilladora
  clientes_disponibles <- reactive({
    req(credentials()$user_auth)
    sort(unique(na.omit(ofertas_u()$PerRazSoc)))
  })
  
  ## Poblar el selectizeInput del sidebar vía server-side (evita serializar lista completa al browser)
  observe({
    req(credentials()$user_auth, length(clientes_disponibles()) > 0)
    updateSelectizeInput(
      session,
      inputId  = "ClienteInicial",
      choices  = clientes_disponibles(),
      selected = clientes_disponibles()[1],
      server   = TRUE
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
    ids_cliente <- Facturas %>%
      filter(PerRazSoc == input$ClienteInicial) %>%
      pull(IdClienteInicial) %>%
      unique()
    
    if (any(ids_cliente %in% bloqueados, na.rm = TRUE)) {
      div(
        class = "alert alert-danger alert-dismissible mx-3 mt-2",
        icon("ban"), " Cliente bloqueado para transar",
        tags$button(
          type           = "button",
          class          = "close",
          `data-dismiss` = "alert",
          `aria-label`   = "Close",
          tags$span(`aria-hidden` = "true", HTML("&times;"))
        )
      )
    }
  })
  
  # Preloaders y control de waiter ----
  
  ## Ocultar el waiter inicial una vez que la sesión está lista
  observe({
    waiter_hide()
  }) %>% bindEvent(session$clientData$url_hostname, once = TRUE)
  
  ## Botón actualizar: mostrar animación y recargar la sesión
  observeEvent(input$BTN_Actualizar, {
    waiter_show(
      html  = preloader_actualizar$html,
      color = preloader_actualizar$color
    )
    session$reload()
  })
  
  ## Waiter al cambiar de cliente: mostrar mientras reactivos recalculan
  observeEvent(input$ClienteInicial, {
    waiter_show(
      html  = preloader_calculando$html,
      color = preloader_calculando$color
    )
  }, ignoreInit = TRUE)
  
  ## Ocultar waiter cuando el primer dato filtrado esté listo
  observe({
    req(input$ClienteInicial, !is.null(ofertas_f()))
    waiter_hide()
  })
  
  # Outputs de header ----
  output$user <- renderUI({
    FormatearTexto(
      HTML(paste(usuario())),
      negrita = TRUE, tamano_pct = 0.75, alineacion = "center", color = "#999"
    )
  })
  
  # Módulos hijos ----
  
  ## Resumen general del cliente
  mod_resumen_server(
    id            = "resumen",
    ofertas_r     = ofertas_f,
    rfm_r         = rfm_f,
    liquidacion_r = liquidacion_f,
    facturas_r    = facturas_f,
    entradas_r    = entradas_f
  )
  
  ## Análisis de ofertas (rfm_r agregado para el gauge de cumplimiento)
  mod_ofertas_server(
    id            = "ofertas",
    ofertas_r     = ofertas_f,
    liquidacion_r = liquidacion_f,
    rfm_r         = rfm_f
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
