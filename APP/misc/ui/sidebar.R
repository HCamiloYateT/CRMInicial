sidebar <- bs4DashSidebar(status = "danger", expandOnHover = FALSE,
                          bs4SidebarMenu(
                            id = "menu_principal",
                            bs4SidebarMenuItem("Resumen", icon = icon("gauge"),
                                               bs4SidebarMenuSubItem("General", tabName = "ResumenGeneral", icon = icon("chart-pie"))),
                            bs4SidebarMenuItem("Ofertas", icon = icon("handshake"),
                                               bs4SidebarMenuSubItem("Tabla de Ofertas", tabName = "OfertasTabla", icon = icon("list"))),
                            bs4SidebarMenuItem("Facturación", icon = icon("file-invoice-dollar"),
                                               bs4SidebarMenuSubItem("Tabla de Facturas", tabName = "FacturacionTabla", icon = icon("list"))),
                            bs4SidebarMenuItem("Entradas", icon = icon("truck-ramp-box"),
                                               bs4SidebarMenuSubItem("Tabla de Entradas", tabName = "EntradasTabla", icon = icon("list"))),
                            bs4SidebarMenuItem("Comunicaciones", icon = icon("comments"),
                                               bs4SidebarMenuSubItem("Mensajes", tabName = "ComunicacionesTabla", icon = icon("list"))
                                               )
                            )
                          )
