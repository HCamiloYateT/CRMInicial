generalidades_tab <- tabPanel(icon = ph("presentation-chart", weight = "bold"), 
                              "Generalidades",
                              # Filtros ----
                              fixedPanel(style = "z-index: 10;", top = 20, left = "auto", right = 20, bottom = "auto",
                                         dropdown(
                                           tags$div(style="height: overflow-y: auto;",
                                                    fluidRow(
                                                      column(12, 
                                                             pickerInput("GenSucursal", label =h6("Sucursal"), 
                                                                         choices = Unicos(Entradas$Sucursal),
                                                                         multiple = F, width = "100%"))
                                                    )
                                           ),
                                           style = "pill", icon = icon("filter"), size = "lg",
                                           status = "danger", width = "600px", right = T, 
                                           tooltip = tooltipOptions (title = "Filtros"),
                                           animate = animateOptions(
                                             enter = animations$fading_entrances$fadeInRight,
                                             exit = animations$fading_exits$fadeOutRightBig)
                                         )
                              ),
                              # Contenido ----
                              uiOutput("TextoGeneralidades"),
                              fluidRow(
                                column(6,
                                       h6("Existencias de Pergamino"),
                                       dataTableOutput("Gen_Ind_Existencias")),
                                column(6,
                                       h6("Café sin Negociar"),
                                       dataTableOutput("Gen_Ind_CSN"))
                              ),
                              fluidRow(
                                column(12,
                                       h6("Ofertas Pendientes por Altura"),
                                       dataTableOutput("Gen_Ofertas"),
                                       modalDialogUI(modalId = "DetalleOFEPEN",
                                                     title = "Detalle Ofertas Pendientes",  easyClose = T,  button = NULL,
                                                     footer = actionButton("Cerrar_DetalleOFEPEN", "Cerrar"),
                                                     dataTableOutput("DetalleOFEPEN")
                                       )
                                )
                              )
                              # ----
)