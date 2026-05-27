# Sidebar principal ----
# tabName de cada subItem debe coincidir exactamente con los bs4TabItem de body.R
sidebar <- bs4DashSidebar(
  status        = "danger",
  expandOnHover = FALSE,
  
  ## Selector de cliente — choices enviados desde server con server=TRUE ----
  tags$div(
    style = "padding: 8px 12px 4px;",
    selectizeInput(
      inputId = "ClienteInicial",
      label   = tags$small("Cliente"),
      choices = NULL,
      width   = "100%"
    )
  ),
  
  ## Alerta de cliente bloqueado ----
  uiOutput("ui_bloqueado"),
  
  ## Menú de navegación ----
  bs4SidebarMenu(
    id = "menu_principal",
    
    bs4SidebarMenuItem(
      "Resumen", icon = icon("gauge"),
      bs4SidebarMenuSubItem("General", tabName = "tab_resumen",
                            icon = icon("chart-pie"))
    ),
    
    bs4SidebarMenuItem(
      "Ofertas", icon = icon("handshake"),
      bs4SidebarMenuSubItem("Análisis de Ofertas", tabName = "tab_ofertas",
                            icon = icon("list"))
    ),
    
    bs4SidebarMenuItem(
      "Facturación", icon = icon("file-invoice-dollar"),
      bs4SidebarMenuSubItem("Análisis de Facturación", tabName = "tab_facturacion",
                            icon = icon("list"))
    ),
    
    bs4SidebarMenuItem(
      "Entradas", icon = icon("truck-ramp-box"),
      bs4SidebarMenuSubItem("Análisis de Entradas", tabName = "tab_entradas",
                            icon = icon("list"))
    )
    
    ## Comunicaciones: pendiente de implementación (sin tab en body.R)
    # bs4SidebarMenuItem(
    #   "Comunicaciones", icon = icon("comments"),
    #   bs4SidebarMenuSubItem("Mensajes", tabName = "tab_comunicaciones",
    #                         icon = icon("list"))
    # )
  )
)
