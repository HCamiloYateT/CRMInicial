controlbar <- bs4DashControlbar(id = "controlbar", skin = "light", pinned = NULL,
                                overlay = FALSE, width = "500px", type = "tabs",
                                title = "Control",
                                controlbarMenu(id = "Filtros", type = "tabs",
                                               controlbarItem("Filtros",
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
                                                              
                                                              ## Botón para confirmar el proveedor seleccionado ----
                                                              # La reactividad de los módulos solo se dispara al presionar este botón,
                                                              # evitando recomputaciones en cada tecla del selectize.
                                                              tags$div(
                                                                style = "padding: 0px 12px 10px;",
                                                                actionButton(
                                                                  inputId = "BTN_Aplicar",
                                                                  label   = "Aplicar selección",
                                                                  icon    = icon("check"),
                                                                  class   = "btn btn-danger btn-sm w-100"
                                                                )
                                                              ),
                                                              
                                                              ## Alerta de cliente bloqueado ----
                                                              uiOutput("ui_bloqueado"),
                                                              
                                                              )
                                               )
                                )
