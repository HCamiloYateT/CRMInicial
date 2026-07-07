function(input, output, session) {
  
  # Identidad del usuario ----
  # En Posit Connect usa session$user / session$group; en local cae a valores por defecto
  usuario <- reactive({
    if (is.null(session$user)) "HCYATE" else str_to_upper(session$user)
  })
  grupo <- reactive({
    if (is.null(session$group)) "ANALÍTICA" else str_to_upper(session$group)
  })
  
  # Sucursal activa según grupo de Posit Connect ----
  # Regla: si el grupo empieza con "TRILLADORA" (después de normalizar tildes),
  # se resuelve el nombre en el catálogo sucs; en cualquier otro grupo → todas.
  sucursal_usuario <- reactive({
    # Normalizar tildes para comparar con sucs (ya pasó por LimpiarNombres)
    grp <- toupper(iconv(grupo(), to = "ASCII//TRANSLIT", sub = ""))
    
    if (!stringr::str_starts(grp, "TRILLADORA")) {
      return("TODAS")
    }
    
    # Extraer sufijo: "TRILLADORA ARENALES" → "ARENALES"
    sufijo <- stringr::str_remove(grp, "^TRILLADORA\\s+")
    
    # Buscar en catálogo: comparación en mayúsculas (sucs$Sucursal ya sin tildes)
    match_suc <- sucs %>%
      dplyr::filter(
        toupper(as.character(Sucursal)) == sufijo |
          as.character(Sucursal) == paste0("Trilladora ", stringr::str_to_title(sufijo))
      )
    
    if (nrow(match_suc) == 0) "TODAS" else as.character(match_suc$Sucursal[1])
  })
  # Filtro por trilladora según perfil del usuario ----
  # Encapsula la lógica de acceso: "TODAS" devuelve el data frame completo
  filtrar_por_trilladora <- function(df, campo_sucursal = "Sucursal") {
    reactive({
      waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
      on.exit(waiter_hide())
      if (sucursal_usuario() == "TODAS") {
        df
      } else {
        df %>% filter(.data[[campo_sucursal]] == sucursal_usuario())
      }
    })
  }
  filtrar_por_trilladora <- function(df, campo_sucursal = "Sucursal") {
    function(){
      if (sucursal_usuario() == "TODAS") {
        df
      } else {
        df %>% filter(.data[[campo_sucursal]] == sucursal_usuario())
      }
    }
  }
  
  # Datos filtrados por trilladora ----
  liquidacion_u <- filtrar_por_trilladora(Liquidacion)
  ofertas_u     <- filtrar_por_trilladora(Ofertas)
  facturas_u    <- filtrar_por_trilladora(Facturas)
  entradas_u    <- filtrar_por_trilladora(Entradas)
  rfm_u <- reactive({
    waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
    on.exit(waiter_hide())
    if (sucursal_usuario() == "TODAS") {
      ResultadosRFM
    } else {
      ResultadosRFM %>%
        filter(grepl(sucursal_usuario(), Sucursales, ignore.case = TRUE))
    }
  })
  
  # Selector de cliente ----
  
  ## Lista de clientes disponibles según filtro de trilladora
  clientes_disponibles <- reactive({
    sort(unique(na.omit(ofertas_u()$PerRazSoc)))
  })
  
  ## Poblar el selectizeInput del sidebar vía server-side (evita serializar lista completa)
  observe({
    updateSelectizeInput(
      session,
      inputId  = "ClienteInicial",
      choices  = clientes_disponibles(),
      selected = clientes_disponibles()[1],
      server   = TRUE
    )
  })
  
  # Cliente activo — controlado por el botón Aplicar ----
  # La reactividad de los módulos solo fluye cuando el usuario confirma la selección.
  # En la carga inicial se aplica automáticamente el primer cliente disponible.
  cliente_activo <- reactiveVal(NULL)
  
  ## Carga inicial: aplicar primer cliente sin necesidad de presionar el botón
  observeEvent(clientes_disponibles(), {
    req(length(clientes_disponibles()) > 0)
    if (is.null(cliente_activo())) cliente_activo(clientes_disponibles()[1])
  }, once = TRUE, ignoreNULL = TRUE)
  
  ## Botón Aplicar: confirmar el proveedor seleccionado en el selectize
  observeEvent(input$BTN_Aplicar, {
    req(input$ClienteInicial)
    cliente_activo(input$ClienteInicial)
  })
  
  # Datos filtrados por cliente activo ----
  # Todos los módulos consumen estas reactivas; se recalculan SOLO al cambiar cliente_activo()
  
  liquidacion_f <- reactive({
    req(cliente_activo())
    liquidacion_u() %>% filter(PerRazSoc == cliente_activo())
  })
  
  ofertas_f <- reactive({
    req(cliente_activo())
    ofertas_u() %>% filter(PerRazSoc == cliente_activo())
  })
  
  rfm_f <- reactive({
    req(cliente_activo())
    rfm_u() %>% filter(customer_id == cliente_activo())
  })
  
  facturas_f <- reactive({
    req(cliente_activo())
    facturas_u() %>% filter(PerRazSoc == cliente_activo())
  })
  
  entradas_f <- reactive({
    req(cliente_activo())
    entradas_u() %>% filter(PerRazSoc == cliente_activo())
  })
  
  # Alerta de cliente bloqueado ----
  
  output$ui_bloqueado <- renderUI({
    req(cliente_activo())
    ids_cliente <- Facturas %>%
      filter(PerRazSoc == cliente_activo()) %>%
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
    waiter_show(html = preloader_actualizar$html, color = preloader_actualizar$color)
    session$reload()
  })
  
  ## Waiter al aplicar cliente (botón o carga inicial): mostrar mientras recalculan módulos
  observe({
    req(cliente_activo())
    waiter_show(html = preloader_calculando$html, color = preloader_calculando$color)
  }) %>% bindEvent(cliente_activo(), ignoreNULL = TRUE, ignoreInit = FALSE)
  
  ## Ocultar waiter cuando el primer dato filtrado esté disponible
  observe({
    req(cliente_activo(), !is.null(ofertas_f()))
    waiter_hide()
  })
  
  # Outputs de header ----
  
  ## Nombre del usuario autenticado
  output$user <- renderUI({
    FormatearTexto(
      HTML(paste(usuario())),
      negrita = TRUE, tamano_pct = 0.75, alineacion = "center", color = "#999"
    )
  })
  
  ## Sucursal activa: badge con el nombre resuelto desde el grupo del usuario
  output$sucursal_label <- renderUI({
    suc <- sucursal_usuario()
    # Color del badge: rojo corporativo para trilladora específica, gris para "Todas"
    badge_color <- if (suc == "TODAS") "#aaa" else "#c0392b"
    tags$span(
      style = paste0(
        "display:inline-flex;align-items:center;gap:5px;",
        "font-size:0.72rem;font-weight:600;",
        "background:", badge_color, ";color:#fff;",
        "border-radius:10px;padding:2px 9px;white-space:nowrap;"
      ),
      icon("building", style = "font-size:0.65rem;"),
      suc
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