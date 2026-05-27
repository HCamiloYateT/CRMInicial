pick_opt <- function(cho, fem=T){
  
  tod <- ifelse(fem, "Todas", "Todos")
  
  res <- list(`live-search` = TRUE,
              `actions-box` = TRUE,
              `deselect-all-text` = paste("Deseleccionar", tod),
              `select-all-text` = paste("Seleccionar", tod),
              `selected-text-format` = paste0("count > ", length(cho) -1),
              `count-selected-text` = tod,
              `none-selected'ext` = "")
  return(res)
} 


entradas_tab <- tabPanel(icon = ph("truck", weight = "bold"), "Entradas",
                         # Filtros ----
                         fixedPanel(style = "z-index: 10;", top = 20, left = "auto", right = 20, bottom = "auto",
                                    dropdown(
                                      tags$div(style="height: overflow-y: auto;",
                                               fluidRow(
                                                 column(12, 
                                                        pickerInput("EntSucursal", label =h6("Sucursal"), 
                                                                    choices = Unicos(Entradas$Sucursal),
                                                                    multiple = F, width = "100%"))
                                                 ),
                                               fluidRow(
                                                 column(6, 
                                                        dateRangeInput(inputId = "EntFecha", label = h6("Fecha de Entrada"),
                                                                       start = PrimerDia(Sys.Date()), end = max(Entradas$RegFchEnt),                                                           
                                                                       min = min(Entradas$RegFchEnt), max = max(Entradas$RegFchEnt),
                                                                       format = "dd-mm-yyyy", language = "es",
                                                                       separator = "a", width = "100%"),
                                                        materialSwitch(inputId = "EntTodasFec", 
                                                                       label = tags$p("Todas las fechas de entrada"),
                                                                       value = F, status = "danger", width = "100%")),
                                                 column(6, pickerInput("EntCalidad", label =h6("Calidad"), 
                                                                       choices = Unicos(Entradas$CalNom),
                                                                       selected =  Unicos(Entradas$CalNom),
                                                                       options = pick_opt(Unicos(Entradas$CalNom), fem = F), 
                                                                       multiple = T, width = "100%"))
                                                 ),
                                               fluidRow(
                                                 column(6, 
                                                        pickerInput("EntCooperativa", label =h6("Cooperativa"), 
                                                                    choices = Unicos(Entradas$RazonSocial),
                                                                    selected =  Unicos(Entradas$RazonSocial),
                                                                    options = pick_opt(Unicos(Entradas$RazonSocial), fem = F), 
                                                                    multiple = T, width = "100%")),
                                                 column(6,
                                                        pickerInput("EntAsociado", label =h6("Asociado"), 
                                                                    choices = Unicos(Entradas$Asociado),
                                                                    selected =  Unicos(Entradas$Asociado),
                                                                    options = pick_opt(Unicos(Entradas$Asociado), fem = F), 
                                                                    multiple = T, width = "100%"))
                                                 ),
                                               fluidRow(
                                                 column(6, 
                                                        pickerInput("EntDepto", label =h6("Departamento"), 
                                                                    choices = Unicos(Entradas$Departamento),
                                                                    selected =  Unicos(Entradas$Departamento),
                                                                    options = pick_opt(Unicos(Entradas$Departamento), fem = F), 
                                                                    multiple = T, width = "100%")),
                                                 column(6,
                                                        pickerInput("EntMunicipio", label =h6("Municipio"), 
                                                                    choices = Unicos(Entradas$Municipio),
                                                                    selected = Unicos(Entradas$Municipio),
                                                                    multiple = T, width = "100%"))
                                                 ),
                                               fluidRow(
                                                 column(4),
                                                 column(4, style="text-align: center;",
                                                        actionBttn(inputId = "Aplicar", label = "Aplicar Filtros",
                                                                   style = "unite", color = "danger", size = "sm",
                                                                   icon = icon("check"), block=T)),
                                                 column(4)
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
                         fluidRow(
                           column(6, 
                                  h6("Calidad de Pergamino"), 
                                  TablaDimensionUI("Calidad", "Calidad")),
                           column(6, 
                                  ClienteUI("Proveedor"))
                           ),
                         fluidRow(
                           column(6, 
                                  h6("Cooperativa"), 
                                  TablaDimensionUI("Cooperativa", "Cooperativa")),
                           column(6, 
                                  h6("Asociado"), 
                                  TablaDimensionUI("Asociado", "Asociado"))
                           ),
                         fluidRow(
                           column(6, 
                                  h6("Departamento"), 
                                  TablaDimensionUI("Departamento", "Depto")),
                           column(6, 
                                  h6("Municipio"), 
                                  TablaDimensionUI("Municipio", "Mpio"))
                           ),
                         h6("Calidad"),
                         fluidRow(
                           column(3),
                           column(6, 
                                  dataTableOutput("EVAs"),
                                  modalDialogUI(modalId = "DetalleEVA",
                                                title = "Detalle Evaluación de Recibo",  easyClose = T,  button = NULL,
                                                footer = actionButton("Cerrar_DetalleEVA", "Cerrar"),
                                                EVAUI("DetalleEVA")
                                                )
                                  ),
                           column(3)
                           )
                         # ----
                         )
