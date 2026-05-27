## Funciones

PrepEntradaDim <- function(dat, dim){
  if (nrow(dat)>0) {
    dat %>% 
      group_by(!!as.name(dim)) %>% 
      summarise(
        Entradas = n(),
        KilosAntes = sum(KilosNetos),
        KilosDespues = sum(KilosNegoc),
        FecUltEntrada = as.Date(max(RegFchEnt))
      ) %>% 
      arrange(desc(KilosDespues)) %>% 
      janitor::adorn_totals("row", name = "TOTAL")
  }
  
}
AdicionarBotonDetalle <- function(tabla){
  req(tabla)
  if(nrow(tabla)>0){
    tabla %>% 
      mutate(D = tags$div(id="Detalle", style = "cursor: pointer; text-align: center;",
                          ph("magnifying-glass-plus", fill = "#922B21", title = "Detalle")) %>% as.character)
  }
  
  
}  
ImprimirDimEntrada <- function(dat){
  
  if (nrow(dat) >0) {
    v1 <- dat %>% select(1) %>% .[[1]]
    cols1 <- ifelse(v1 == 'TOTAL','bold','normal')
    noms <- c("", "Entradas", "Kilos Ant. Bonif/Desc", "Kilos Dep. Bonif/Desc", "Fecha Últ. Entrada")
    
    datatable(dat, escape = F, rownames=F, colnames = noms, extensions = "FixedColumns",
              selection = "none", style = "default",
              options=list(pageLength = nrow(dat), dom = 't', searching= T,scrollX=0.1,
                           autoWidth = F, ordering= T, autoWidth = F,
                           columnDefs = list(list(width = "49%", targets = 0),
                                             list(width = "10%", targets = 1:4),
                                             list(className = 'dt-center', targets = 4)),
                           language = lang)) %>%
      formatRound(2:4, digits = 0) %>% 
      formatStyle(1, target = "row", fontWeight = styleEqual(v1, cols1))
    
  } 
  
}
ImprimirDimEntradaDetalle <- function(dat){
  
  aux1 <- dat
  filas <- 1:nrow(aux1)
  colus <- ncol(aux1)-1
  mat <- expand.grid(filas, colus) %>% as.matrix
  noms <- c("", "Entradas", "Kilos Ant. Bonif/Desc", "Kilos Dep. Bonif/Desc", "Fecha Últ. Entrada", "")
  
  v1 <- aux1 %>% select(1) %>% .[[1]]
  cols1 <- ifelse(v1 == 'TOTAL','bold','normal')
  
  if(!is.null(mat)){
    datatable(aux1, escape = F, rownames=F, colnames = noms, extensions = "FixedColumns",
              selection = list(target='cell', mode = "single", selectable = mat), style = "default",
              options=list(pageLength = nrow(aux1), dom = 'tf', searching= T,scrollX=0.1,
                           autoWidth = F, ordering= T, autoWidth = F,
                           columnDefs = list(list(width = "39%", targets = 0),
                                             list(width = "15%", targets = 1:4),
                                             list(className = 'dt-center', targets = 4),
                                             list(width = "1%", className = 'dt-center', targets =colus)
                           ),
                           language = lang)) %>%
      formatRound(2:4, digits = 0) %>%
      formatStyle(1, target = "row", fontWeight = styleEqual(v1, cols1))
    }
    
  }

## Entradas ----

DimIndivudualUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, dataTableOutput(ns("Tabla"), width = "100%"))
    )
  )
}
DimIndivudual <- function(id, dat, dim) {
  moduleServer(id, function(input, output, session){
    
    output$Tabla <- renderDataTable({
      req(dat())  
      aux1 <- dat() %>%
        PrepEntradaDim(dim) %>% 
        as.data.frame()
      
      ImprimirDimEntrada(aux1)
      
    })
    
  })
}

AntiguedadUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(5, 
             h6("Distribución por Antigüedad de entrada"),
             dataTableOutput(ns("TablaAntiguedad"), width = "100%")),
      column(7, 
             h6("Serie Diaria de Entradas"),
             plotlyOutput(ns("SerieAntiguedad"), width = "100%")),
    )
    
  )
}
Antiguedad <-  function(id, dat) {
  moduleServer(id, function(input, output, session){ 
    
    output$TablaAntiguedad <- renderDataTable({
      req(dat())
      
      aux1 <- dat() %>%  
        mutate(Periodo = str_to_sentence(case_when(year(RegFchEnt) < year(Sys.Date()) ~ format(RegFchEnt, "%Y"),
                                                   semester(RegFchEnt) < semester(Sys.Date()) ~ paste0(quarter(RegFchEnt), "Q-", year(RegFchEnt)),
                                                   T ~ format(RegFchEnt, "%B %Y")))) %>% 
        PrepEntradaDim("Periodo") %>% 
        as.data.frame
      
      ImprimirDimEntrada(aux1)
      
      
      
      
      
    })
    output$SerieAntiguedad <- renderPlotly({
      
      dat() %>%  
        group_by(Fecha = RegFchEnt) %>% 
        summarise(KilosAntes = sum(KilosNetos),
                  KilosDespues = sum(KilosNegoc)) %>% 
        plot_ly(x=~Fecha) %>% 
        add_trace(y=~KilosAntes, color = I("steelblue"), type = "scatter", mode="lines+markers", marker = list(size = 5), name = "Kilos Ant. Bonif/Desc") %>% 
        add_trace(y=~KilosDespues, color = I("darkorchid"), type = "scatter", mode="lines+markers", marker = list(size = 5), name = "Kilos Desp. Bonif/Desc") %>% 
        layout(title = list(text="Entradas (Kg)", 
                            font=list(family = "Arial, sans-serif",size = 16,color = "black")),
               xaxis = list(title = ""),
               yaxis = list(title="Entradas (Kg)", tickformat = ",.0f", visible=T, rangemode = "tozero",
                            title=list(text="", font= list(family = "Arial, sans-serif",
                                                           size = 16,color = "black")), 
                            tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.1)) %>% 
        config(locale = "es", displayModeBar=F)
      
      
    })
    
    
  })
}

GeografiaUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(5, 
             h6("Departamento"),
             dataTableOutput(ns("Depto"))),
      column(7, 
             h6("Municipio"),
             dataTableOutput(ns("Mpio")))
    )
  )
}
Geografia <-  function(id, dat) {
  moduleServer(id, function(input, output, session){
    
    output$Depto <- renderDataTable({
      req(dat())  

      aux1 <- dat() %>%
        PrepEntradaDim("Departamento") %>% 
        as.data.frame()
      
      ImprimirDimEntrada(aux1)


    })
    output$Mpio <- renderDataTable({
    req(dat())  
    aux1 <- dat() %>%
      PrepEntradaDim("Municipio") %>% 
      as.data.frame()

    ImprimirDimEntrada(aux1)
      
    })
    
  })
}

ClienteUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, h6("Cliente"),
             dataTableOutput(ns("TablaCliente"), width = "100%"))
    )
  )
}
Cliente <- function(id, dat) {
  moduleServer(id, function(input, output, session){
    
    output$TablaCliente <- renderDataTable({
      req(dat())  
      aux1 <- dat() %>%
        PrepEntradaDim("Proveedor") %>% 
        AdicionarBotonDetalle %>% 
        as.data.frame()
      
      ImprimirDimEntradaDetalle(aux1)
      
    })
    
  })
}

EVAUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(5, 
             h6("Distribución del Precio de Carga"), 
             plotlyOutput(ns("PrecioCarga"), width = "100%", height = "150px"),
             h6("Distribución de la Humedad"), 
             plotlyOutput(ns("Humedad"), width = "100%", height="150px"),
             h6("Distribución del Factor de Rendimiento"), 
             plotlyOutput(ns("FactorRdto"), width = "100%", height="150px")
             ),
      column(7, 
             h6("Granulometria"), 
             plotlyOutput(ns("Granulometria"), width = "100%", height="570px"))
      )
    )
}
EVA <- function(id, dat) {
  moduleServer(id, function(input, output, session){
    
    output$PrecioCarga <- renderPlotly({
      
      aux1 <- dat() %>% 
        select(Fecha = RegFchEnt, value = PrecioCarga) %>% 
        distinct() %>% 
        na.omit()
      
      plot_ly(aux1, x=~value, type="box", color = I("steelblue"), 
              boxmean = T, boxpoints = 'all',  pointpos =0, 
              jitter = 0.5, hoveron="points", hoverlabel = list(align = "left"), hoverinfo="text",
              hovertext = paste0("<br><b>Fecha de Entrada: </b>", format(aux1$Fecha, "%d%b%Y"),
                                 "<br><b>Precio de Carga: </b>", dollar(aux1$value, accuracy = 1),
                                 "<br><br><b>Precio Mínimo: </b>", dollar(min(aux1$value), accuracy = 1),
                                 "<br><b>Precio Promedio: </b>", dollar(mean(aux1$value), accuracy = 1),
                                 "<br><b>Precio Máximo: </b>", dollar(max(aux1$value), accuracy = 1)
                                 
              )) %>% 
        layout(title = "",
               xaxis = list(title="", tickformat = "$,.0f"),
               yaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               showlegend=F) %>% 
        config(locale = "es",displayModeBar=F)
      
      
      
      
    })
    output$Humedad <- renderPlotly({
      
      aux1 <- dat() %>% 
        mutate(EvaPorHum = EvaPorHum/100) %>% 
        select(Fecha = RegFchEnt, value = EvaPorHum) %>% 
        distinct() %>% 
        na.omit()
      
      plot_ly(aux1, x=~value, type="box", color = I("steelblue"), 
              boxmean = T, boxpoints = 'all',  pointpos =0, 
              jitter = 0.5, hoveron="points", hoverlabel = list(align = "left"), hoverinfo="text",
              hovertext = paste0("<br><b>Fecha de Entrada: </b>", format(aux1$Fecha, "%d%b%Y"),
                                 "<br><b>Humedad: </b>", percent(aux1$value, accuracy = 0.1),
                                 "<br><br><b>Humedad Mínima: </b>", percent(min(aux1$value), accuracy = 0.1),
                                 "<br><b>Humedad Promedia: </b>", percent(mean(aux1$value), accuracy = 0.1),
                                 "<br><b>Humedad Máxima: </b>", percent(max(aux1$value), accuracy = 0.1)
                                 
              )) %>% 
        layout(title = "",
               xaxis = list(title="", tickformat = ",.2%"),
               yaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               showlegend=F) %>% 
        config(locale = "es",displayModeBar=F)
      
    })
    output$FactorRdto <- renderPlotly({
      
      aux1 <- dat() %>% 
        select(Fecha = RegFchEnt, value = FactorRendimiento) %>% 
        distinct() %>% 
        na.omit()
      
      plot_ly(aux1, x=~value, type="box", color = I("steelblue"), 
              boxmean = T, boxpoints = 'all',  pointpos =0, 
              jitter = 0.5, hoveron="points", hoverlabel = list(align = "left"), hoverinfo="text",
              hovertext = paste0("<br><b>Fecha de Entrada: </b>", format(aux1$Fecha, "%d%b%Y"),
                                 "<br><b>Precio de Carga: </b>", comma(aux1$value, accuracy = 0.01),
                                 "<br><br><b>Precio Mínimo: </b>", comma(min(aux1$value), accuracy = 0.01),
                                 "<br><b>Precio Promedio: </b>", comma(mean(aux1$value), accuracy = 0.01),
                                 "<br><b>Precio Máximo: </b>", comma(max(aux1$value), accuracy = 0.01)
                                 
              )) %>% 
        layout(title = "",
               xaxis = list(title="", tickformat = ",.2f"),
               yaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               showlegend=F) %>% 
        config(locale = "es",displayModeBar=F)
      
    })
    output$Granulometria <- renderPlotly({
      
      aux1 <- dat() %>% 
        select(Fecha = RegFchEnt, PctExcelso:PctMerma) %>% 
        pivot_longer(PctExcelso:PctMerma) %>% 
        mutate(name = str_to_upper(gsub("Pct", "", name))) %>% 
        distinct() %>% 
        na.omit() %>% 
        mutate(name = factor(name, c("MERMA","RIPIO","PASILLA","CONSUMO","EXCELSO")))
      
      plot_ly(aux1, x=~value, type="box", color = ~name, 
              boxmean = T, boxpoints = 'all',  pointpos =0, 
              jitter = 0.5, hoveron="points", hoverlabel = list(align = "left"), hoverinfo="text",
              hovertext = paste0("<br><b>Fecha de Entrada: </b>", format(aux1$Fecha, "%d%b%Y"),
                                 "<br><b>", aux1$name,": </b>", percent(aux1$value, accuracy = 0.01))) %>% 
        layout(title = "",
               xaxis = list(title="", tickformat = ",.2%"),
               yaxis = list(zeroline = FALSE, showline = FALSE, showgrid = FALSE),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               showlegend=F) %>% 
        config(locale = "es",displayModeBar=F)
      
    })

    
  })
}

DetalleCalNomUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    fluidRow(
      column(12, AntiguedadUI(ns("Antiguedad")))
    ),
    fluidRow(
      column(12, GeografiaUI(ns("Geografia")))
    ),
    fluidRow(
      column(6, ClienteUI(ns("Cliente"))),
      column(6, h6("Cooperativa"), 
             DimIndivudualUI(ns("Cooperativa")))
    ),
    fluidRow(
      column(6, h6("Asociado"), 
             DimIndivudualUI(ns("Asociado"))),
      column(6, h6("Conductor"), 
             DimIndivudualUI(ns("Conductor")))
    ),
    fluidRow(
      column(12, h6("Evaluación de Recibo"), 
             EVAUI(ns("EVA")))
      )
    )
}
DetalleCalNom <- function(id, dat, tit) {
  moduleServer(id, function(input, output, session){
    
    output$Titulo <- renderUI({
      h5(paste("Detalle de entradas para la calidad: ", tit))
      
    })
    
    DimIndivudual("Calidad", dat, "CalNom")
    Antiguedad("Antiguedad", dat)
    Geografia("Geografia", dat)
    Cliente("Cliente", dat)
    DimIndivudual("Cooperativa", dat, "RazonSocial")
    DimIndivudual("Asociado", dat, "Asociado")
    DimIndivudual("Conductor", dat, "Conductor")
    EVA("EVA", dat)
    
  })
}

DetalleCooperativaUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    fluidRow(
      column(3, h6("Calidad de Pergamino")),
      column(6,br(), br(),
             DimIndivudualUI(ns("Calidad"))),
      column(3)
    ),
    fluidRow(
      column(12, AntiguedadUI(ns("Antiguedad")))
    ),
    fluidRow(
      column(12, GeografiaUI(ns("Geografia")))
    ),
    fluidRow(
      column(6, ClienteUI(ns("Cliente"))),
      column(6, h6("Conductor"), 
             DimIndivudualUI(ns("Conductor")))
    ),
    fluidRow(
      column(12, h6("Evaluación de Recibo"), 
             EVAUI(ns("EVA")))
    )
  )
}
DetalleCooperativa <- function(id, dat, tit) {
  moduleServer(id, function(input, output, session){
    
    output$Titulo <- renderUI({
      h5(paste("Detalle de entradas para la calidad: ", tit))
      
    })
    
    DimIndivudual("Calidad", dat, "CalNom")
    Antiguedad("Antiguedad", dat)
    Geografia("Geografia", dat)
    Cliente("Cliente", dat)
    DimIndivudual("Cooperativa", dat, "RazonSocial")
    DimIndivudual("Asociado", dat, "Asociado")
    DimIndivudual("Conductor", dat, "Conductor")
    EVA("EVA", dat)
    
  })
}

DetalleAsociadoUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    fluidRow(
      column(3, h6("Calidad de Pergamino")),
      column(6,br(), br(),
             DimIndivudualUI(ns("Calidad"))),
      column(3)
    ),
    fluidRow(
      column(12, AntiguedadUI(ns("Antiguedad")))
    ),
    fluidRow(
      column(12, GeografiaUI(ns("Geografia")))
    ),
    fluidRow(
      column(6, h6("Cooperativa"), 
             DimIndivudualUI(ns("Cooperativa"))),
      column(6, h6("Conductor"), 
             DimIndivudualUI(ns("Conductor")))
    ),
    fluidRow(
      column(12, h6("Evaluación de Recibo"), 
             EVAUI(ns("EVA")))
    )
  )
}
DetalleAsociado <- function(id, dat, tit) {
  moduleServer(id, function(input, output, session){
    
    output$Titulo <- renderUI({
      h5(paste("Detalle de entradas para la calidad: ", tit))
      
    })
    
    DimIndivudual("Calidad", dat, "CalNom")
    Antiguedad("Antiguedad", dat)
    Geografia("Geografia", dat)
    Cliente("Cliente", dat)
    DimIndivudual("Cooperativa", dat, "RazonSocial")
    DimIndivudual("Asociado", dat, "Asociado")
    DimIndivudual("Conductor", dat, "Conductor")
    EVA("EVA", dat)
    
  })
}

DetalleDeptoUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    fluidRow(
      column(3, h6("Calidad de Pergamino")),
      column(6,br(), br(),
             DimIndivudualUI(ns("Calidad"))),
      column(3)
    ),
    fluidRow(
      column(12, AntiguedadUI(ns("Antiguedad")))
    ),
    fluidRow(
      column(12, GeografiaUI(ns("Geografia")))
    ),
    fluidRow(
      column(6, ClienteUI(ns("Cliente"))),
      column(6, h6("Cooperativa"), 
             DimIndivudualUI(ns("Cooperativa")))
    ),
    fluidRow(
      column(6, h6("Asociado"), 
             DimIndivudualUI(ns("Asociado"))),
      column(6, h6("Conductor"), 
             DimIndivudualUI(ns("Conductor")))
    ),
    fluidRow(
      column(12, h6("Evaluación de Recibo"), 
             EVAUI(ns("EVA")))
    )
  )
}
DetalleDepto <- function(id, dat, tit) {
  moduleServer(id, function(input, output, session){
    
    output$Titulo <- renderUI({
      h5(paste("Detalle de entradas para la calidad: ", tit))
      
    })
    
    DimIndivudual("Calidad", dat, "CalNom")
    Antiguedad("Antiguedad", dat)
    Geografia("Geografia", dat)
    Cliente("Cliente", dat)
    DimIndivudual("Cooperativa", dat, "RazonSocial")
    DimIndivudual("Asociado", dat, "Asociado")
    DimIndivudual("Conductor", dat, "Conductor")
    EVA("EVA", dat)
    
  })
}

DetalleMpioUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    fluidRow(
      column(3, h6("Calidad de Pergamino")),
      column(6,br(), br(),
             DimIndivudualUI(ns("Calidad"))),
      column(3)
    ),
    fluidRow(
      column(12, AntiguedadUI(ns("Antiguedad")))
    ),
    fluidRow(
      column(12, GeografiaUI(ns("Geografia")))
    ),
    fluidRow(
      column(6, ClienteUI(ns("Cliente"))),
      column(6, h6("Cooperativa"), 
             DimIndivudualUI(ns("Cooperativa")))
    ),
    fluidRow(
      column(6, h6("Asociado"), 
             DimIndivudualUI(ns("Asociado"))),
      column(6, h6("Conductor"), 
             DimIndivudualUI(ns("Conductor")))
    ),
    fluidRow(
      column(12, h6("Evaluación de Recibo"), 
             EVAUI(ns("EVA")))
    )
  )
}
DetalleMpio <- function(id, dat, tit) {
  moduleServer(id, function(input, output, session){
    
    output$Titulo <- renderUI({
      h5(paste("Detalle de entradas para la calidad: ", tit))
      
    })
    
    DimIndivudual("Calidad", dat, "CalNom")
    Antiguedad("Antiguedad", dat)
    Geografia("Geografia", dat)
    Cliente("Cliente", dat)
    DimIndivudual("Cooperativa", dat, "RazonSocial")
    DimIndivudual("Asociado", dat, "Asociado")
    DimIndivudual("Conductor", dat, "Conductor")
    EVA("EVA", dat)
    
  })
}

DetalleDimensionUI <- function(id, det){
  ns <- NS(id)
  tagList(
    if (det=="Calidad") DetalleCalNomUI(ns("Detalle"))
    else if (det=="Cooperativa") DetalleCooperativaUI(ns("Detalle"))
    else if (det=="Asociado") DetalleAsociadoUI(ns("Detalle"))
    else if (det=="Depto") DetalleDeptoUI(ns("Detalle"))
    else if (det=="Mpio") DetalleDeptoUI(ns("Detalle"))
  )
}
DetalleDimension <- function(id, dat, dim, tit) {
  moduleServer(id, function(input, output, session){

    DetalleCalNom("Detalle", dat, tit)
    DetalleCooperativa("Detalle", dat, tit)
    DetalleAsociado("Detalle", dat, tit)
    DetalleDepto("Detalle", dat, tit)
    DetalleMpio("Detalle", dat, tit)
    
  })
}

TablaDimensionUI <- function(id, det){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, 
             dataTableOutput(ns("Tabla"), width = "100%"),
             modalDialogUI(modalId = ns("DetalleDimension"),
                           title = "",  easyClose = T,  button = NULL,
                           footer = actionButton(ns("Cerrar_DetalleDimension"), "Cerrar"),
                           DetalleDimensionUI(ns("DetDimension"), det)
                           )
             
      )
    )
  )
}
TablaDimension <- function(id, tabla, dim) {
  moduleServer(id, function(input, output, session){ 
    
    df <- reactive({
      req(tabla())
      tabla() %>% 
        PrepEntradaDim(dim) %>% 
        AdicionarBotonDetalle() %>% 
        as.data.frame
    })

    output$Tabla <- renderDataTable({

      ImprimirDimEntradaDetalle(df())

    })
    df2 <- reactive({
      req(input$Tabla_cells_selected)
      seleccion = df()[input$Tabla_cells_selected[1],1] %>% .[[1]]
      tabla() %>%
        filter(if(seleccion=="TOTAL")T else !!as.name(dim) == seleccion) 
    })
    observe({
      req(input$Tabla_cells_selected)
      if (!is.na(input$Tabla_cells_selected[1])) {
        showModalUI("DetalleDimension")
      }
    })
    
    
    DetalleDimension("DetDimension", df2, dim = dim, tit = df()[input$Tabla_cells_selected[1],1])
    observeEvent(input$Cerrar_DetalleDimension, {
      hideModalUI("DetalleDimension")
    })
    
  })
}




    
## Individual ----

IndividualUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             selectizeInput(ns("ClienteInicial"), h6("Seleccione el cliente inicial"), 
                            width = "100%", choices = "", multiple = F)),
      column(2),
      column(4, uiOutput(ns("Bloqueados")))  
    ),
    tabsetPanel(
      #### Resumen ----
      tabPanel("Resumen",
               fluidRow(
                 column(12, style="text-align: right; margin: 3px -3px 0 auto;",
                        actionButton("", "?", 
                                     style="color: #F4F6F7; background-color: #5499C7;font-weight: bold; font-size: 12px;
                                                         border-color: #1A5276;border-radius: 50%; width: 30px; height: 30px; padding: 1px 1px;
                                                         ") %>%
                          bsPopover("Glosario",
                                    paste0("<b>Volumen:</b>",
                                           "<br> Score relativo del cliente según el total de sacos en entradas realizadas por el cliente en el tiempo de análisis",
                                           "<br><br><b>Recencia:</b>",
                                           "<br> Score relativo del cliente según los días transcurridos desde la última entrada",
                                           "<br><br><b>Frecuencia:</b>",
                                           "<br> Score relativo del cliente según el número de entradas por período de tiempo.",
                                           "<br><br><b>Cumplimiento:</b>",
                                           "<br> Score derivado del cálculo de la probabilidad que un cliente tarde más de 30 días en cumplir una oferta",
                                           "<br><br><b>Recompra:</b>",
                                           "<br> Score derivado del cálculo de la probabilidad que un cliente continúe con sus entregas en la periodicidad observada históricamente",
                                           "<br><br><b>Sobrevida:</b>",
                                           "<br> Score derivado del cálculo de la probabilidad que un cliente entregue café en los próximos seis meses",
                                           "<br><br><b>Fuga:</b>",
                                           "<br> Score derivado del cálculo de la probabilidad que un cliente entregue café en los próximos tres meses"
                                    ),
                                    "left", html = T)
                 )
               ),
               icon = ph("address-book", weight = "fill"),
               br(),br(),
               fluidRow(
                 column(4,
                        box(width = "100%", title= "Contacto", height = "500px",
                            status="danger", solidHeader = F, collapsible = T, 
                            collapsed = F,
                            tableOutput(ns("TablaContacto"))
                        )
                 ),
                 column(8,
                        plotlyOutput(ns("RadarResumen"), height = "500px"),
                        tags$p(style="text-align: center;font-size: 12px; font-weight: bold;", 
                               "*Siendo 0 el peor score y 1,000 el mejor en todas las dimensiones")
                 )
               ),
               h4("Ofertas"),
               fluidRow(
                 column(4,
                        tableOutput(ns("TablaResumenOfertas"))
                 ),
                 column(8,
                        tabBox(
                          title = "", id = "TabsOfertas", height = "500px", 
                          width = "100%",
                          tabPanel("Histórico", 
                                   icon = ph("chart-line"),
                                   plotlyOutput(ns("SerieResumenOfertas"), height = "500px", width = "100%")),
                          tabPanel("Cumplimiento", 
                                   icon = ph("list-checks"),
                                   plotlyOutput(ns("ResumenCosechasKilos"), height = "500px", width = "100%"))
                        )
                 )
               ),
               h4("Facturas"),
               fluidRow(
                 column(4,
                        tableOutput(ns("TablaResumenFacturas"))
                 ),
                 column(8,
                        plotlyOutput(ns("SerieResumenFacturas"), height = "500px", width = "100%")
                 )
               ),
               h4("Entradas"),
               fluidRow(
                 column(4,
                        tableOutput(ns("TablaResumenEntradas"))
                 ),
                 column(8,
                        plotlyOutput(ns("SerieResumenEntradas"), height = "500px", width = "100%")
                 )
               )
      ),
      #### Ofertas ----
      tabPanel("Ofertas",
               icon = ph("handshake", weight = "fill"),
               br(),
               h4("Ofertas por Cliente"),
               br(),
               fluidRow(
                 column(2,br(),br(),br(),infoBoxOutput(ns("VB_NumOfertas"), width = 12)),
                 column(2,br(),br(),br(),infoBoxOutput(ns("VB_KlsOfertas"), width = 12)),
                 column(3,br(),br(),br(),infoBoxOutput(ns("VB_ValorOfertas"), width = 12)),
                 column(3,br(),br(),br(),infoBoxOutput(ns("VB_AnticiposOfertas"), width = 12)),
                 column(2,h4("Score de Cumplimiento"),
                        plotlyOutput(ns("ScoreCumplimiento"), width = "100%", height = "150px", inline = F))
               ),
               br(),
               fluidRow(
                 column(6, 
                        h6("Distribución de Ofertas"), 
                        tabBox(
                          title = "", id = "TabsDistrOfertas", height = "500px", 
                          width = "100%",
                          tabPanel("Kilos", 
                                   icon = ph("scales"),
                                   plotlyOutput(ns("SankeyKilos"), height = "500px", width = "100%")),
                          tabPanel("Anticipos", 
                                   icon = ph("money"),
                                   plotlyOutput(ns("SankeyAnticipos"), height = "500px", width = "100%")
                          )
                        )
                 ),
                 column(6, h6("Cumplimiento de Ofertas"), 
                        tabBox(
                          title = "", id = "TabsDistrOfertas", height = "500px", 
                          width = "100%",
                          tabPanel("Resumen", 
                                   icon = ph("table"),
                                   h4("Ofertas Cumplidas"),
                                   br(),
                                   tableOutput(ns("ResumenCosechas"))),
                          tabPanel("Kilos", 
                                   icon = ph("scales"),
                                   plotlyOutput(ns("CosechasKilos"), height = "500px", width = "100%")),
                          tabPanel("Anticipos", 
                                   icon = ph("money"),
                                   plotlyOutput(ns("CosechasAnticipos"), height = "500px", width = "100%")
                          )
                        )
                 )
               ),
               br(),
               h4("Alturas de Mora"),
               fluidRow(
                 column(6, plotlyOutput(ns("BarrasAlturas"), height = "500px", width = "100%")),
                 column(6, 
                        tabBox(
                          title = "", id = "TabsAlturas", height = "500px", 
                          width = "100%",
                          tabPanel("Kilos", 
                                   icon = ph("scales"),
                                   plotlyOutput(ns("BarrasAlturasVintKls"), height = "500px", width = "100%")),
                          tabPanel("Anticipos", 
                                   icon = ph("money"),
                                   plotlyOutput(ns("BarrasAlturasVintAnt"), height = "500px", width = "100%")
                          )
                        )
                 )
               )
      ),
      #### Facturacion ----
      tabPanel("Facturación",
               icon = ph("receipt", weight = "fill"),
               br(),
               h4("Faturación por Cliente"),
               br(),
               fluidRow(
                 column(2,infoBoxOutput(ns("VB_NumFacturas"), width = 12)),
                 column(2,infoBoxOutput(ns("VB_KlsFacturas"), width = 12)),
                 column(2,infoBoxOutput(ns("VB_ValorFacturas"), width = 12)),
                 column(2,infoBoxOutput(ns("VB_RecenciaFacturas"), width = 12)),
                 column(2,infoBoxOutput(ns("VB_FrecuenciaFacturas"), width = 12)),
                 column(2,infoBoxOutput(ns("VB_MontoFacturas"), width = 12))
               ),
               h4("Pronósticos de Facturación"),
               fluidRow(
                 column(2,
                        verticalLayout(
                          plotlyOutput(ns("DistribuicionSucursal"), height = "250px", width = "100%"),
                          br(),
                          plotlyOutput(ns("DistribuicionNegocio"), height = "250px", width = "100%")
                        )
                 ),
                 column(10,
                        tabBox(
                          title = "", id = "TabsFacturas", height = "600px", 
                          width = "100%",
                          tabPanel("Kilos", 
                                   icon = ph("scales"),
                                   dygraphOutput(ns("SerieFacturasKilos"), height = "500px", width = "100%")),
                          tabPanel("Valor Facturado", 
                                   icon = ph("money"),
                                   dygraphOutput(ns("SerieFacturasValor"), height = "500px", width = "100%")
                          )
                        )
                 )
               )
      ),
      #### Entradas ----
      tabPanel("Entradas",
               icon = ph("truck", weight = "fill"),
               br(),
               h4("Faturación por Cliente"),
               br(),
               fluidRow(
                 column(4,br(),br(),infoBoxOutput(ns("VB_NumEntradas"), width = 12)),
                 column(4,br(),br(),infoBoxOutput(ns("VB_KlsEntradasBrutos"), width = 12)),
                 # column(3,br(),br(),infoBoxOutput("VB_KlsEntradasNetos", width = 12)),
                 column(4,tableOutput(ns("DistribucionCalidad")))
               ),
               h4("Ubicación Geográfica"),
               fluidRow(
                 column(5, leafletOutput(ns("MapaOrigenes"), height = "500px")),
                 column(7, dataTableOutput(ns("DetalleMapa")))
               ),
               h4("Evaluación de Recibo"),
               br(),
               fluidRow(
                 column(4, plotlyOutput(ns("SerieFactorRendimiento"))),
                 column(2, plotlyOutput(ns("CajaFactorRendimiento"))),
                 column(4, plotlyOutput(ns("SerieHumedad"))),
                 column(2, plotlyOutput(ns("CajaHumedad"))),
               ),
               fluidRow(
                 column(8, plotlyOutput(ns("SerieGranulometria"))),
                 column(4, plotlyOutput(ns("CajaGranulometria")))
                 )
               )
      ### ----
    )
  )
}

# Individual <- function(id, dat) {
#   moduleServer(id, function(input, output, session){
#     
#     liquidacion_f <- reactive({
#       req(input$ClienteInicial)
#       liquidacion_u() %>% filter(PerRazSoc == input$ClienteInicial)
#     })
#     ofertas_f <- reactive({
#       req(input$ClienteInicial)
#       ofertas_u() %>% filter(PerRazSoc == input$ClienteInicial)
#     })
#     rfm_f <- reactive({
#       req(input$ClienteInicial)
#       rfm_u() %>% filter(customer_id == input$ClienteInicial)
#       
#     })
#     facturas_f <- reactive({
#       req(input$ClienteInicial)
#       facturas_u() %>% filter(PerRazSoc == input$ClienteInicial)
#       
#     })
#     entradas_f <- reactive({
#       req(input$ClienteInicial)
#       entradas_u() %>% filter(PerRazSoc == input$ClienteInicial)
#       
#     })
#     
#     ## Cliente Inicial ----
#     
#     output$Bloqueados <- renderUI({
#       
#       id <- Facturas %>% filter(PerRazSoc== input$ClienteInicial) %>% select(IdClienteInicial) %>% distinct() %>% as.numeric()
#       
#       if(id %in% bloqueados){
#         h3("Cliente Bloqueado para tranzar", style = "color:red;")}
#       
#     })
#     
#     ### Resumen -----
#     
#     # output$test <- renderDataTable({
#     #   datatable(rfm_u(), options = list(scrollX = TRUE))
#     # })
#     
#     output$TablaContacto <- renderTable({
#       
#       if (nrow(ofertas_f())>0) {
#         aux0 <- ofertas_f() %>% 
#           select(Nombre = PerRazSoc, 
#                  `Tipo de Persona` = PerTipPer,
#                  `Departamento` = NomDep,
#                  `Municipio` = Mun,
#                  `Dirección` = PerDir,
#                  `Teléfono` = PerTel
#           ) %>% 
#           distinct() %>%  
#           pivot_longer(Nombre:Teléfono, names_to = "Item", values_to = "Registro")
#       }
#       
#       aux0
#       
#     }, spacing = 'xs', width = "100%")
#     output$RadarResumen <- renderPlotly({
#       
#       if (nrow(ofertas_f())>0) {      
#         
#         aux0 <- rfm_f() 
#         aux1 <- aux0 %>% 
#           select(ScoreCumplimiento:ScoreRecompra) 
#         
#         aux2 <- ResultadosRFM %>% 
#           select(ScoreCumplimiento:ScoreRecompra) %>% 
#           summarise_all(mean, na.rm=T)
#         
#         plot_ly(type = 'scatterpolar',  mode ="markers",
#                 fill = 'toself') %>% 
#           add_trace(type = 'scatterpolar', mode ="markers",
#                     r = aux1 %>% as.numeric(),
#                     theta = c('Cumplimiento','Frecuencia','Recencia','Volumen','Fuga','Sobrevida','Recompra'),
#                     name =  aux0$customer_id %>% unique(), color=I("#5DADE2"),
#                     hoverinfo = "text", hoverlabel = list(aux1 = "left"),
#                     hovertext = paste0("<b>", aux0$customer_id %>% unique(),"<b>",
#                                        "<br><b>", c('Cumplimiento','Frecuencia','Recencia','Volumen','Fuga','Sobrevida','Recompra'),
#                                        ": </b>", aux1 %>% as.numeric() %>% comma)) %>% 
#           add_trace(type = 'scatterpolar', mode ="markers",
#                     r = aux2 %>% as.numeric(),
#                     theta = c('Cumplimiento','Frecuencia','Recencia','Volumen','Fuga','Sobrevida','Recompra'),        
#                     name =  "General", color=I("#99A3A4"),
#                     hoverinfo = "text", hoverlabel = list(aux1 = "left"),
#                     hovertext = paste0("<b>", "General","<b>",
#                                        "<br><b>", c('Cumplimiento','Frecuencia','Recencia','Volumen','Fuga','Sobrevida','Recompra'),
#                                        ": </b>", aux1 %>% as.numeric() %>% comma)) %>% 
#           layout(margin = m,
#                  legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.18, 
#                                font=list(family = "Arial, sans-serif",size = 14,color = "black"))) %>% 
#           config(displayModeBar=F)
#       }
#     })
#     
#     output$TablaResumenOfertas <- renderTable({
#       
#       aux0 <- ofertas_f() 
#       
#       bind_cols(
#         `Ofertas Pendientes`= aux0 %>% filter(OfeEst == "PENDIENTE") %>% summarise(paste(OfeNro, collapse = "|")) %>% as.character(),
#         `Última Oferta`= aux0 %>% arrange(desc(OfeFch)) %>% filter(row_number()==1) %>% select(OfeNro) %>% as.character(),
#         `Fecha Última Oferta`= aux0 %>% arrange(desc(OfeFch)) %>% filter(row_number()==1) %>% select(OfeFch) %>% format("%d %b %y") %>% as.character(),
#         `Kilos Última Oferta`= aux0 %>% arrange(desc(OfeFch)) %>% filter(row_number()==1) %>% select(OfeTotKls) %>% .[[1]] %>% comma() %>% as.character() ,
#         `Anticipo Última Oferta`= aux0 %>% arrange(desc(OfeFch)) %>% filter(row_number()==1) %>% select(AnticiposGirados) %>% .[[1]] %>% dollar %>% as.character(),
#         `Última Oferta Cumplida`= aux0 %>% filter(OfeEst =="CUMPLIDA") %>% arrange(desc(OfeFch)) %>% filter(row_number()==1) %>% select(OfeNro) %>% as.character(),
#         `Fecha Última Oferta Cumplida`= aux0 %>% filter(OfeEst =="CUMPLIDA") %>% arrange(desc(OfeFch)) %>% filter(row_number()==1) %>% select(OfeFch) %>% format("%d %b %y") %>% as.character(),
#         `Kilos Última Oferta Cumplida`= aux0 %>% filter(OfeEst =="CUMPLIDA") %>% arrange(desc(OfeFch)) %>% filter(row_number()==1) %>% select(OfeTotKls) %>% .[[1]] %>% comma() %>%  as.character(),
#         `Anticipo Última Oferta Cumplida`= aux0 %>% filter(OfeEst =="CUMPLIDA") %>% arrange(desc(OfeFch)) %>% filter(row_number()==1) %>% select(AnticiposGirados) %>% .[[1]] %>% dollar %>% as.character()) %>% 
#         pivot_longer(`Ofertas Pendientes`:`Anticipo Última Oferta Cumplida`, names_to = "Item", values_to = "Registro")
#       
#     }, spacing = 'xs', width = "100%")
#     output$SerieResumenOfertas <- renderPlotly({
#       
#       aux0 <- ofertas_f()
#       
#       aux1 <- aux0 %>% 
#         group_by(Fecha = floor_date(OfeFch, unit = "month")) %>% 
#         summarise(Kilos = sum(OfeTotKls),
#                   `Anticipo Girado (Miles)` = sum(AnticiposGirados, na.rm = T)/1000)
#       
#       plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Kilos, type = "scatter", mode="lines+markers", 
#               line = list(width = 2), marker = list(size = 5), name = "Kilos", color=I("#000000"),
#               textposition = 'bottom', text = ~comma(aux1$Kilos, accuracy = 1),
#               hoverlabel = list(align = "left"), hoverinfo = "text",
#               hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
#                                  "<br>","Kilos"," :", comma(aux1$Kilos, accuracy = 0.01))) %>% 
#         add_trace(x = aux1$Fecha , y= aux1$`Anticipo Girado (Miles)`, type = "scatter", mode="lines+markers",
#                   line = list(width = 2), marker = list(size = 5), name = "Anticipos (Miles de Pesos)", color=I("#df8879"),
#                   textposition = 'bottom', text = ~comma(aux1$`Anticipo Girado (Miles)`, accuracy = 1),
#                   hoverlabel = list(align = "left"), hoverinfo = "text",
#                   hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
#                                      "<br>","Anticipos"," :", comma(aux1$`Anticipo Girado (Miles)`, accuracy = 0.01))) %>% 
#         layout(title = list(text="Ofertas",
#                             font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
#                xaxis = list(gridcolor="#CCD1D1",
#                             title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                yaxis = list(tickformat = "s",
#                             title= list(text= "", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22,
#                              font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>%
#         config(displayModeBar=F)
#       
#     })
#     output$ResumenCosechasKilos <- renderPlotly({
#       
#       varplot="Kilos"
#       
#       aux0 <- liquidacion_f() %>% 
#         filter(OfeEst == "CUMPLIDA")
#       
#       aux1 <- aux0 %>%
#         filter(!is.na(Fecha)) %>% 
#         mutate(FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
#                FechaCumplimiento = if_else(FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento),
#                TOB = pmax(0,as.numeric(difftime(as.Date(Fecha), as.Date(FechaCumplimiento), units = "days")))) %>% 
#         select(SucCod, OfeNro, FecOferta, FechaCumplimiento, Fecha ,KilosOriginal, kilos, AntGirado, DescuentoAnt, TOB) %>% 
#         group_by(Vintage = ifelse(year(FechaCumplimiento) <2023, 
#                                   paste(year(FechaCumplimiento)),  
#                                   paste0(year(FechaCumplimiento), " Q", quarter(FechaCumplimiento)))) %>% 
#         filter(Vintage != 1973) %>% 
#         group_by(SucCod,OfeNro) %>% 
#         mutate(KilosTest = ifelse(row_number()==1, KilosOriginal, 0),
#                KilosAcum = cumsum(kilos),
#                Ultima = ifelse(row_number()==n(), 1, 0),
#                Verif = KilosOriginal-KilosAcum) %>% 
#         ungroup()
#       
#       aux2 <- aux1 %>% 
#         group_by(Vintage, TOB) %>% 
#         summarise(kilos = sum(kilos),
#                   AnticipoGirado = sum(AntGirado),
#                   AnticipoDescontado = sum(DescuentoAnt)) %>% 
#         ungroup() %>% 
#         left_join(aux1 %>% 
#                     select(OfeNro, Vintage, KilosOriginal) %>% 
#                     distinct() %>% 
#                     group_by(Vintage) %>% 
#                     summarise(KilosOriginal = sum(KilosOriginal)),
#                   by = "Vintage"
#         ) %>% 
#         group_by(Vintage) %>% 
#         mutate(Kilos = (KilosOriginal-cumsum(kilos))/KilosOriginal,
#                Anticipos = (cumsum(AnticipoGirado)-cumsum(AnticipoDescontado))/cumsum(AnticipoGirado),
#                Var = !!sym(varplot)
#         ) %>% 
#         complete(TOB = 0:60) %>% 
#         mutate(Var = ifelse(TOB ==0, 1, Var)) %>% 
#         fill(Var, .direction = "up")
#       
#       ncols <- length(unique(aux2$Vintage))
#       cols <- paleta[1:ncols]
#       names(cols) <-  unique(aux2$Vintage)
#       
#       plot_ly(data = aux2, x = ~TOB, y = ~Var, type = 'scatter', mode = 'lines', color = ~Vintage, colors = cols,
#               line = list(width=2), textposition = 'bottom', text = ~percent(Var, accuracy = 0.1), hoverinfo = "text", 
#               hoverlabel = list(align = "left"),
#               hovertext = paste0("<b>", aux2$Vintage, "</b>",
#                                  "<br>Dias de Cumplimiento :", comma(aux2$TOB, accuracy = 1),
#                                  "<br>Porcentaje de Saldo", varplot, " : ", percent(aux2$Var, accuracy = 0.1))) %>% 
#         layout(shapes= list(vline(30, color = "#D35400"),
#                             vline(60, color = "red")),
#                title = list(text= paste("Cosechas de Ofertas (", varplot, ")"), 
#                             font=list(family = "Arial, sans-serif",size = 18,color = "black")),
#                xaxis = list(title=list(text="Días después de oferta", font= list(family = "Arial, sans-serif",size = 16,color = "black")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ",.0%", rangemode = "tozero",
#                             title=list(text="Porcentaje de Saldo", font= list(family = "Arial, sans-serif",size = 16,color = "black")), 
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.18, 
#                              font=list(family = "Arial, sans-serif",size = 14,color = "black"))
#         ) %>% 
#         config(displayModeBar=F)
#       
#       
#     })
#     
#     output$TablaResumenFacturas <- renderTable({
#       
#       aux0 <- facturas_f()
#       
#       bind_cols(
#         `Última Factura`= aux0 %>% arrange(desc(FCoFch)) %>% filter(row_number()==1) %>% select(FCoNro) %>% as.character(),
#         `Fecha Última Factura`= aux0 %>% arrange(desc(FCoFch)) %>% filter(row_number()==1) %>% select(FCoFch) %>% format("%d %b %y") %>% as.character(),
#         `Kilos Última Factura`= aux0 %>% arrange(desc(FCoFch)) %>% filter(row_number()==1) %>% select(kilos) %>% .[[1]] %>% comma() %>% as.character(),
#         `Valor Última Factura`= aux0 %>% arrange(desc(FCoFch)) %>% filter(row_number()==1) %>% select(ValorFacturado) %>% .[[1]] %>% dollar() %>% as.character()
#       ) %>% 
#         pivot_longer(`Última Factura`:`Valor Última Factura`, names_to = "Item", values_to = "Registro")
#       
#     }, spacing = 'xs', width = "100%")
#     output$SerieResumenFacturas <- renderPlotly({
#       
#       aux0 <- facturas_f()
#       
#       aux1 <- aux0 %>% 
#         group_by(Fecha = floor_date(FCoFch, unit = "month")) %>% 
#         summarise(Kilos = sum(kilos),
#                   `Valor Facturado (Miles)` = sum(ValorFacturado, na.rm = T)/1000)
#       
#       plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Kilos, type = "scatter", mode="lines+markers",
#               line = list(width = 2), marker = list(size = 5), name = "Kilos", color=I("#000000"),
#               textposition = 'bottom', text = ~comma(aux1$Kilos, accuracy = 1),
#               hoverlabel = list(align = "left"), hoverinfo = "text",
#               hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
#                                  "<br>","Kilos"," :", comma(aux1$Kilos, accuracy = 0.01))) %>% 
#         add_trace(x = aux1$Fecha , y= aux1$`Valor Facturado (Miles)`, type = "scatter", mode="lines+markers",
#                   line = list(width = 2), marker = list(size = 5), name = "Valor Facturado (Miles)", color=I("#df8879"),
#                   textposition = 'bottom', text = ~comma(aux1$`Valor Facturado (Miles)`, accuracy = 1),
#                   hoverlabel = list(align = "left"), hoverinfo = "text", 
#                   hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
#                                      "<br>","Valor"," :", comma(aux1$`Valor Facturado (Miles)`, accuracy = 0.01))) %>% 
#         layout(title = list(text="Facturas",
#                             font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
#                xaxis = list(gridcolor="#CCD1D1",
#                             title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                yaxis = list(tickformat = "s",
#                             title= list(text= "", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22,
#                              font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>%
#         config(displayModeBar=F)
#       
#     })
#     
#     output$TablaResumenEntradas <- renderTable({
#       
#       aux0 <- entradas_f()
#       
#       bind_cols(
#         `Última Entrada`= aux0 %>% arrange(desc(RegFchEnt)) %>% filter(row_number()==1) %>% select(RegNro) %>% as.character(),
#         `Fecha Última Entrada`= aux0 %>% arrange(desc(RegFchEnt)) %>% filter(row_number()==1) %>% select(RegFchEnt) %>% format("%d %b %y") %>% as.character(),
#         `Kilos Última Entrada`= aux0 %>% arrange(desc(RegFchEnt)) %>% filter(row_number()==1) %>% select(KilosNetos) %>% .[[1]] %>% comma() %>% as.character(),
#         `Calidad Última Entrada`= aux0 %>% arrange(desc(RegFchEnt)) %>% filter(row_number()==1) %>% select(CalNom) %>% as.character(),
#         
#       ) %>% 
#         pivot_longer(`Última Entrada`:`Calidad Última Entrada`, names_to = "Item", values_to = "Registro")
#       
#     }, spacing = 'xs', width = "100%")
#     output$SerieResumenEntradas <- renderPlotly({
#       
#       aux0 <- facturas_f()
#       
#       aux1 <- aux0 %>% 
#         group_by(Fecha = floor_date(FCoFch, unit = "month")) %>% 
#         summarise(Kilos = sum(kilos))
#       
#       plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Kilos, type = "scatter", mode="lines+markers",
#               line = list(width = 2), marker = list(size = 5), name = "Kilos", color=I("#000000"),
#               textposition = 'bottom', text = ~comma(aux1$Kilos, accuracy = 1),
#               hoverlabel = list(align = "left"), hoverinfo = "text",
#               hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
#                                  "<br>","Kilos"," :", comma(aux1$Kilos, accuracy = 0.01))) %>% 
#         layout(title = list(text="Kilos en Entradas",
#                             font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
#                xaxis = list(gridcolor="#CCD1D1",
#                             title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                yaxis = list(tickformat = "s",
#                             title= list(text= "", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22,
#                              font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>%
#         config(displayModeBar=F)
#       
#     })
#     
#     ### Ofertas -----
#     
#     output$VB_NumOfertas <- renderInfoBox({
#       
#       aux1 <- ofertas_f() %>% nrow()
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Ofertas",
#         icon = icon("bars"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_KlsOfertas <- renderInfoBox({
#       
#       aux1 <- sum(ofertas_f()$OfeTotKls, na.rm = T)
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Kilos",
#         icon = icon("weight-scale"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_ValorOfertas <- renderInfoBox({
#       
#       aux1 <- sum(ofertas_f()$ValorOfe, na.rm = T)
#       
#       infoBox(
#         value = dollar(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Valor de las Ofertas",
#         icon = icon("money-bills"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_AnticiposOfertas <- renderInfoBox({
#       
#       aux1 <- sum(ofertas_f()$AnticiposGirados, na.rm = T)
#       
#       infoBox(
#         value = dollar(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Anticipos Girados",
#         icon = icon("hand-holding-dollar"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$ScoreCumplimiento <- renderPlotly({
#       
#       set.seed(31415)
#       aux <- rfm_f()$ScoreCumplimiento %>% round()
#       aux <-ifelse(is.na(aux), runif(1, 500, 700), aux)
#       
#       ImprimirGauge(val = aux, limites = c(400, 700), rango = c(0,1000), F, "numero")
#       
#     })
#     
#     output$SankeyKilos <- renderPlotly({
#       
#       Varplot = "Kilos"
#       
#       aux0 <- ofertas_f() 
#       
#       aux1 <- bind_rows(
#         aux0 %>% 
#           group_by(Origen = "TOTAL", Destino = Sucursal) %>% 
#           summarise(Ofertas = n(),
#                     Kilos = sum(OfeTotKls, na.rm = T),
#                     Anticipos = sum(AnticiposGirados, na.rm = T)),
#         aux0 %>% 
#           group_by(Origen = Sucursal, Destino = OfeEst) %>% 
#           summarise(Ofertas = n(),
#                     Kilos = sum(OfeTotKls, na.rm = T),
#                     Anticipos = sum(AnticiposGirados, na.rm = T))) %>% 
#         group_by("Auxiliar") %>% 
#         mutate(PctOfertas = Ofertas / sum(ifelse(Origen == "TOTAL", Ofertas, 0)),
#                PctKilos = Kilos  / sum(ifelse(Origen == "TOTAL", Kilos, 0)),
#                PctAnticipos = Anticipos  / sum(ifelse(Origen == "TOTAL", Anticipos, 0)),
#                Var = !!sym(Varplot)
#         ) %>% 
#         ungroup()
#       
#       n_seq <- length(unique(aux1$Destino))
#       Nodes <- bind_cols(node=seq(0,n_seq), 
#                          bind_rows(
#                            aux1 %>% 
#                              filter(Origen == "TOTAL") %>% 
#                              group_by(name = Origen) %>% 
#                              summarise(Ofertas = sum(Ofertas),
#                                        Kilos = sum(Kilos),
#                                        Anticipos = sum(Anticipos),
#                                        PctOfertas = sum(PctOfertas),
#                                        PctKilos = sum(PctKilos),
#                                        PctAnticipos = sum(PctAnticipos)
#                              ) %>% 
#                              mutate(color = "black",
#                                     info = paste0("<b>Ofertas: </b>", comma(Ofertas, accuracy = 1),
#                                                   "<br><b>Kilos: </b>", comma(Kilos, accuracy = 1),
#                                                   "<br><b>Anticipos: </b>", comma(Anticipos, accuracy = 1))
#                              ) %>% 
#                              select(name, color, info),
#                            aux1 %>% 
#                              group_by(name = Destino) %>% 
#                              summarise(Ofertas = sum(Ofertas),
#                                        Kilos = sum(Kilos),
#                                        Anticipos = sum(Anticipos),
#                                        PctOfertas = sum(PctOfertas),
#                                        PctKilos = sum(PctKilos),
#                                        PctAnticipos = sum(PctAnticipos)) %>% 
#                              mutate(color = recode(name, 
#                                                    "BACHUÉ"="#7D3C98", "MEDELLÍN"="#2980B9", "POPAYÁN"="#17A589",
#                                                    "ARMENIA"="#229954", "ARENALES"="#D4AC0D", "PEREIRA"="#D35400",
#                                                    "BUCARAMANGA"="#82E0AA", "HUILA"="#5D6D7E",
#                                                    "CUMPLIDA" = "#0B5345", "TRASLADADA" = "#6E2C00", "ANULADA" = "#1B2631", 
#                                                    "CRUZADA" = "#7D6608", "CANCELADA" = "#512E5F", "COBRO JURÍDICO" = "#641E16", 
#                                                    "PENDIENTE" = "#78281F"),
#                                     info = paste0("<b>Ofertas: </b>", comma(Ofertas, accuracy = 1),
#                                                   "<b> (" , percent(PctOfertas, accuracy = 0.01), ")</b>",
#                                                   "<br><b>Kilos: </b>", comma(Kilos, accuracy = 1),
#                                                   "<b> (" , percent(PctKilos, accuracy = 0.01), ")</b>",
#                                                   "<br><b>Anticipos: </b>", comma(Anticipos, accuracy = 1),
#                                                   "<b> (" , percent(PctAnticipos, accuracy = 0.01), ")</b>")
#                              ) %>% 
#                              select(name, color, info)
#                          )
#       )
#       
#       Links <- aux1 %>% 
#         left_join(Nodes, by=c("Origen"="name")) %>% 
#         left_join(Nodes, by=c("Destino"="name")) %>% 
#         mutate(info = paste0("<b>De: ", Origen, " a ", Destino,"</b>",
#                              "<br><b>Ofertas: </b>", comma(Ofertas, accuracy = 1),
#                              "<b> (" , percent(PctOfertas, accuracy = 0.01), ")</b>",
#                              "<br><b>Kilos: </b>", comma(Kilos, accuracy = 1),
#                              "<b> (" , percent(PctKilos, accuracy = 0.01), ")</b>",
#                              "<br><b>Anticipos: </b>", comma(Anticipos, accuracy = 1),
#                              "<b> (" , percent(PctAnticipos, accuracy = 0.01), ")</b>")) %>%     
#         select(Origen, source=node.x, Destino, target=node.y, value= Var, info=info)
#       
#       fig <- plot_ly(type = "sankey", orientation = "h", arrangement='fixed',
#                      node = list(
#                        label = Nodes$name,
#                        color = Nodes$color,
#                        pad = 15,
#                        thickness = 20,
#                        customdata = c(Nodes$info),
#                        hoverlabel = list(align = "left"),
#                        hovertemplate = paste0('<b>%{label}',"</b>", 
#                                               '<br>%{customdata}<br>',
#                                               '<extra></extra>')
#                      ),
#                      link = list(
#                        source = Links$source,
#                        target = Links$target,
#                        value = Links$value,
#                        customdata = c(Links$info),
#                        hoverlabel = list(align = "left"),
#                        hovertemplate = paste0('%{customdata}', '<extra></extra>')
#                      )) %>% 
#         layout(title = "Distribución de Ofertas por Sucursal y Estado", font = list(size = 10)) %>%
#         config(displayModeBar=F)
#       fig
#       
#     })
#     output$SankeyAnticipos <- renderPlotly({
#       
#       Varplot = "Anticipos"
#       
#       aux0 <- ofertas_f() 
#       
#       aux1 <- bind_rows(
#         aux0 %>% 
#           group_by(Origen = "TOTAL", Destino = Sucursal) %>% 
#           summarise(Ofertas = n(),
#                     Kilos = sum(OfeTotKls, na.rm = T),
#                     Anticipos = sum(AnticiposGirados, na.rm = T)),
#         aux0 %>% 
#           group_by(Origen = Sucursal, Destino = OfeEst) %>% 
#           summarise(Ofertas = n(),
#                     Kilos = sum(OfeTotKls, na.rm = T),
#                     Anticipos = sum(AnticiposGirados, na.rm = T))) %>% 
#         group_by("Auxiliar") %>% 
#         mutate(PctOfertas = Ofertas / sum(ifelse(Origen == "TOTAL", Ofertas, 0)),
#                PctKilos = Kilos  / sum(ifelse(Origen == "TOTAL", Kilos, 0)),
#                PctAnticipos = Anticipos  / sum(ifelse(Origen == "TOTAL", Anticipos, 0)),
#                Var = !!sym(Varplot)
#         ) %>% 
#         ungroup()
#       
#       n_seq <- length(unique(aux1$Destino))
#       Nodes <- bind_cols(node=seq(0,n_seq), 
#                          bind_rows(
#                            aux1 %>% 
#                              filter(Origen == "TOTAL") %>% 
#                              group_by(name = Origen) %>% 
#                              summarise(Ofertas = sum(Ofertas),
#                                        Kilos = sum(Kilos),
#                                        Anticipos = sum(Anticipos),
#                                        PctOfertas = sum(PctOfertas),
#                                        PctKilos = sum(PctKilos),
#                                        PctAnticipos = sum(PctAnticipos)
#                              ) %>% 
#                              mutate(color = "black",
#                                     info = paste0("<b>Ofertas: </b>", comma(Ofertas, accuracy = 1),
#                                                   "<br><b>Kilos: </b>", comma(Kilos, accuracy = 1),
#                                                   "<br><b>Anticipos: </b>", comma(Anticipos, accuracy = 1))
#                              ) %>% 
#                              select(name, color, info),
#                            aux1 %>% 
#                              group_by(name = Destino) %>% 
#                              summarise(Ofertas = sum(Ofertas),
#                                        Kilos = sum(Kilos),
#                                        Anticipos = sum(Anticipos),
#                                        PctOfertas = sum(PctOfertas),
#                                        PctKilos = sum(PctKilos),
#                                        PctAnticipos = sum(PctAnticipos)) %>% 
#                              mutate(color = recode(name, 
#                                                    "BACHUÉ"="#7D3C98", "MEDELLÍN"="#2980B9", "POPAYÁN"="#17A589",
#                                                    "ARMENIA"="#229954", "ARENALES"="#D4AC0D", "PEREIRA"="#D35400",
#                                                    "BUCARAMANGA"="#82E0AA", "HUILA"="#5D6D7E",
#                                                    "CUMPLIDA" = "#0B5345", "TRASLADADA" = "#6E2C00", "ANULADA" = "#1B2631", 
#                                                    "CRUZADA" = "#7D6608", "CANCELADA" = "#512E5F", "COBRO JURÍDICO" = "#641E16", 
#                                                    "PENDIENTE" = "#78281F"),
#                                     info = paste0("<b>Ofertas: </b>", comma(Ofertas, accuracy = 1),
#                                                   "<b> (" , percent(PctOfertas, accuracy = 0.01), ")</b>",
#                                                   "<br><b>Kilos: </b>", comma(Kilos, accuracy = 1),
#                                                   "<b> (" , percent(PctKilos, accuracy = 0.01), ")</b>",
#                                                   "<br><b>Anticipos: </b>", comma(Anticipos, accuracy = 1),
#                                                   "<b> (" , percent(PctAnticipos, accuracy = 0.01), ")</b>")
#                              ) %>% 
#                              select(name, color, info)
#                          )
#       )
#       
#       Links <- aux1 %>% 
#         left_join(Nodes, by=c("Origen"="name")) %>% 
#         left_join(Nodes, by=c("Destino"="name")) %>% 
#         mutate(info = paste0("<b>De: ", Origen, " a ", Destino,"</b>",
#                              "<br><b>Ofertas: </b>", comma(Ofertas, accuracy = 1),
#                              "<b> (" , percent(PctOfertas, accuracy = 0.01), ")</b>",
#                              "<br><b>Kilos: </b>", comma(Kilos, accuracy = 1),
#                              "<b> (" , percent(PctKilos, accuracy = 0.01), ")</b>",
#                              "<br><b>Anticipos: </b>", comma(Anticipos, accuracy = 1),
#                              "<b> (" , percent(PctAnticipos, accuracy = 0.01), ")</b>")) %>%     
#         select(Origen, source=node.x, Destino, target=node.y, value= Var, info=info)
#       
#       fig <- plot_ly(type = "sankey", orientation = "h", arrangement='fixed',
#                      node = list(
#                        label = Nodes$name,
#                        color = Nodes$color,
#                        pad = 15,
#                        thickness = 20,
#                        customdata = c(Nodes$info),
#                        hoverlabel = list(align = "left"),
#                        hovertemplate = paste0('<b>%{label}',"</b>", 
#                                               '<br>%{customdata}<br>',
#                                               '<extra></extra>')
#                      ),
#                      link = list(
#                        source = Links$source,
#                        target = Links$target,
#                        value = Links$value,
#                        customdata = c(Links$info),
#                        hoverlabel = list(align = "left"),
#                        hovertemplate = paste0('%{customdata}', '<extra></extra>')
#                      )) %>% 
#         layout(title = "Distribución de Ofertas por Sucursal y Estado", font = list(size = 10)) %>%
#         config(displayModeBar=F)
#       fig
#       
#     })
#     
#     output$ResumenCosechas <- renderTable({
#       
#       aux0 <- ofertas_f() %>% 
#         filter(OfeEst == "CUMPLIDA")
#       
#       aux1 <- aux0 %>%
#         mutate(FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, OfeFch),
#                FechaCumplimiento = if_else(FechaCumplimiento == as.Date("1753-01-01"), OfeFch, FechaCumplimiento),
#                Vintage = ifelse(year(FechaCumplimiento) <2023, 
#                                 paste(year(FechaCumplimiento)),  
#                                 paste0(year(FechaCumplimiento), " Q", quarter(FechaCumplimiento))
#                )) %>% 
#         group_by(Cosecha = Vintage) %>% 
#         summarise(Ofertas = comma(n_distinct(OfeNro), accuracy = 1),
#                   Kilos = comma(sum(OfeTotKls), accuracy = 1),
#                   Anticipos = dollar(sum(AnticiposGirados, na.rm = T), accuracy = 1)) 
#       
#       aux1
#       
#     }, spacing = 'xs', width = "100%")
#     output$CosechasKilos <- renderPlotly({
#       
#       varplot="Kilos"
#       
#       aux0 <- liquidacion_f() %>% 
#         filter(OfeEst == "CUMPLIDA")
#       
#       aux1 <- aux0 %>%
#         filter(!is.na(Fecha)) %>% 
#         mutate(FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
#                FechaCumplimiento = if_else(FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento),
#                TOB = pmax(0,as.numeric(difftime(as.Date(Fecha), as.Date(FechaCumplimiento), units = "days")))) %>% 
#         select(SucCod, OfeNro, FecOferta, FechaCumplimiento, Fecha ,KilosOriginal, kilos, AntGirado, DescuentoAnt, TOB) %>% 
#         group_by(Vintage = ifelse(year(FechaCumplimiento) <2023, 
#                                   paste(year(FechaCumplimiento)),  
#                                   paste0(year(FechaCumplimiento), " Q", quarter(FechaCumplimiento)))) %>% 
#         filter(Vintage != 1973) %>% 
#         group_by(SucCod,OfeNro) %>% 
#         mutate(KilosTest = ifelse(row_number()==1, KilosOriginal, 0),
#                KilosAcum = cumsum(kilos),
#                Ultima = ifelse(row_number()==n(), 1, 0),
#                Verif = KilosOriginal-KilosAcum) %>% 
#         ungroup()
#       
#       aux2 <- aux1 %>% 
#         group_by(Vintage, TOB) %>% 
#         summarise(kilos = sum(kilos),
#                   AnticipoGirado = sum(AntGirado),
#                   AnticipoDescontado = sum(DescuentoAnt)) %>% 
#         ungroup() %>% 
#         left_join(aux1 %>% 
#                     select(OfeNro, Vintage, KilosOriginal) %>% 
#                     distinct() %>% 
#                     group_by(Vintage) %>% 
#                     summarise(KilosOriginal = sum(KilosOriginal)),
#                   by = "Vintage"
#         ) %>% 
#         group_by(Vintage) %>% 
#         mutate(Kilos = (KilosOriginal-cumsum(kilos))/KilosOriginal,
#                Anticipos = (cumsum(AnticipoGirado)-cumsum(AnticipoDescontado))/cumsum(AnticipoGirado),
#                Var = !!sym(varplot),
#                Var = ifelse(Var <0, 0, Var)
#         ) %>% 
#         complete(TOB = 0:60) %>% 
#         filter(TOB <=90) %>% 
#         mutate(Var = ifelse(TOB ==0, 1, Var)) %>% 
#         fill(Var, .direction = "up")
#       
#       ncols <- length(unique(aux2$Vintage))
#       cols <- paleta[1:ncols]
#       names(cols) <-  unique(aux2$Vintage)
#       
#       plot_ly(data = aux2, x = ~TOB, y = ~Var, type = 'scatter', mode = 'lines', color = ~Vintage, colors = cols,
#               line = list(width=2), textposition = 'bottom', text = ~percent(Var, accuracy = 0.1), hoverinfo = "text", 
#               hoverlabel = list(align = "left"),
#               hovertext = paste0("<b>", aux2$Vintage, "</b>",
#                                  "<br>Dias de Cumplimiento :", comma(aux2$TOB, accuracy = 1),
#                                  "<br>Porcentaje de Saldo", varplot, " : ", percent(aux2$Var, accuracy = 0.1))) %>% 
#         layout(shapes= list(vline(30, color = "#D35400"),
#                             vline(60, color = "red")),
#                title = list(text= paste("Cosechas de Ofertas (", varplot, ")"), 
#                             font=list(family = "Arial, sans-serif",size = 18,color = "black")),
#                xaxis = list(title=list(text="Días después de oferta", font= list(family = "Arial, sans-serif",size = 16,color = "black")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ",.0%", 
#                             title=list(text="Porcentaje de Saldo", font= list(family = "Arial, sans-serif",size = 16,color = "black")), 
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.18, 
#                              font=list(family = "Arial, sans-serif",size = 14,color = "black"))
#         ) %>% 
#         config(displayModeBar=F)
#       
#       
#     })
#     output$CosechasAnticipos <- renderPlotly({
#       
#       varplot="Anticipos"
#       
#       aux0 <- liquidacion_f() %>% 
#         filter(OfeEst == "CUMPLIDA")
#       
#       aux1 <- aux0 %>%
#         filter(!is.na(Fecha)) %>% 
#         mutate(FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
#                FechaCumplimiento = if_else(FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento),
#                TOB = pmax(0,as.numeric(difftime(as.Date(Fecha), as.Date(FechaCumplimiento), units = "days")))) %>% 
#         select(SucCod, OfeNro, FecOferta, FechaCumplimiento, Fecha ,KilosOriginal, kilos, AntGirado, DescuentoAnt, TOB) %>% 
#         group_by(Vintage = ifelse(year(FechaCumplimiento) <2023, 
#                                   paste(year(FechaCumplimiento)),  
#                                   paste0(year(FechaCumplimiento), " Q", quarter(FechaCumplimiento)))) %>% 
#         filter(Vintage != 1973) %>% 
#         group_by(SucCod,OfeNro) %>% 
#         mutate(KilosTest = ifelse(row_number()==1, KilosOriginal, 0),
#                KilosAcum = cumsum(kilos),
#                Ultima = ifelse(row_number()==n(), 1, 0),
#                Verif = KilosOriginal-KilosAcum) %>% 
#         ungroup()
#       
#       aux2 <- aux1 %>% 
#         group_by(Vintage, TOB) %>% 
#         summarise(kilos = sum(kilos),
#                   AnticipoGirado = sum(AntGirado),
#                   AnticipoDescontado = sum(DescuentoAnt)) %>% 
#         ungroup() %>% 
#         left_join(aux1 %>% 
#                     select(OfeNro, Vintage, KilosOriginal) %>% 
#                     distinct() %>% 
#                     group_by(Vintage) %>% 
#                     summarise(KilosOriginal = sum(KilosOriginal)),
#                   by = "Vintage"
#         ) %>% 
#         group_by(Vintage) %>% 
#         mutate(Kilos = (KilosOriginal-cumsum(kilos))/KilosOriginal,
#                Anticipos = (cumsum(AnticipoGirado)-cumsum(AnticipoDescontado))/cumsum(AnticipoGirado),
#                Var = !!sym(varplot),
#                Var = ifelse(Var <0, 0, Var)
#         ) %>% 
#         complete(TOB = 0:60) %>% 
#         filter(TOB <=90) %>% 
#         mutate(Var = ifelse(TOB ==0, 1, Var)) %>% 
#         fill(Var, .direction = "up")
#       
#       ncols <- length(unique(aux2$Vintage))
#       cols <- paleta[1:ncols]
#       names(cols) <-  unique(aux2$Vintage)
#       
#       plot_ly(data = aux2, x = ~TOB, y = ~Var, type = 'scatter', mode = 'lines', color = ~Vintage, colors = cols,
#               line = list(width=2), textposition = 'bottom', text = ~percent(Var, accuracy = 0.1), hoverinfo = "text", 
#               hoverlabel = list(align = "left"),
#               hovertext = paste0("<b>", aux2$Vintage, "</b>",
#                                  "<br>Dias de Cumplimiento :", comma(aux2$TOB, accuracy = 1),
#                                  "<br>Porcentaje de Saldo", varplot, " : ", percent(aux2$Var, accuracy = 0.1))) %>% 
#         layout(shapes= list(vline(30, color = "#D35400"),
#                             vline(60, color = "red")),
#                title = list(text= paste("Cosechas de Ofertas (", varplot, ")"), 
#                             font=list(family = "Arial, sans-serif",size = 18,color = "black")),
#                xaxis = list(title=list(text="Días después de oferta", font= list(family = "Arial, sans-serif",size = 16,color = "black")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ",.0%", 
#                             title=list(text="Porcentaje de Saldo", font= list(family = "Arial, sans-serif",size = 16,color = "black")), 
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.18, 
#                              font=list(family = "Arial, sans-serif",size = 14,color = "black"))
#         ) %>% 
#         config(displayModeBar=F)
#       
#       
#     })
#     
#     output$BarrasAlturas <- renderPlotly({
#       
#       aux0 <- liquidacion_f() %>% 
#         filter(OfeEst == "CUMPLIDA")
#       
#       aux1 <- aux0 %>%
#         filter(!is.na(Fecha)) %>% 
#         mutate(FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
#                FechaCumplimiento = if_else(FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento),
#                TOB = pmax(0,as.numeric(difftime(as.Date(Fecha), as.Date(FechaCumplimiento), units = "days")))) %>% 
#         select(SucCod, OfeNro, FecOferta, FechaCumplimiento, Fecha ,KilosOriginal, kilos, AntGirado, DescuentoAnt, TOB) %>% 
#         group_by(Vintage = ifelse(year(FechaCumplimiento) <2023, 
#                                   paste(year(FechaCumplimiento)),  
#                                   paste0(year(FechaCumplimiento), " Q", quarter(FechaCumplimiento)))) %>% 
#         filter(Vintage != 1973) %>% 
#         group_by(SucCod,OfeNro) %>% 
#         mutate(KilosTest = ifelse(row_number()==1, KilosOriginal, 0),
#                KilosAcum = cumsum(kilos),
#                Ultima = ifelse(row_number()==n(), 1, 0),
#                Verif = KilosOriginal-KilosAcum) %>% 
#         ungroup()
#       
#       
#       aux2 <- aux1 %>% 
#         group_by(SucCod, OfeNro) %>% 
#         summarise(Altura = max(TOB)) %>% 
#         mutate(Bucket = case_when(Altura <= 0 ~ "Oferta al día",
#                                   Altura <= 7 ~ "De 1 a 7 días",
#                                   Altura <= 15 ~ "De 8 a 15 días",
#                                   Altura <= 30 ~ "De 16 a 30 días",
#                                   Altura <= 60 ~ "De 31 a 60 días",
#                                   Altura <= 90 ~ "De 61 a 90 días",
#                                   T ~ "Más de 90 días"
#         )
#         ) %>% ungroup() %>% 
#         left_join(aux1 %>% 
#                     group_by(SucCod, OfeNro) %>%  
#                     summarise(KilosOriginal = max(KilosOriginal),
#                               Anticipos = sum(AntGirado)
#                     ), by = c("SucCod", "OfeNro")) %>% 
#         mutate(TotOfertas = n(),
#                TotKilos = sum(KilosOriginal),
#                TotAnticipos = sum(Anticipos)
#         ) %>% 
#         group_by(Bucket) %>% 
#         summarise(Ofertas = n(),
#                   PctOfertas = Ofertas/ unique(TotOfertas),
#                   Kilos = sum(KilosOriginal),
#                   PctKilos = Kilos/ unique(TotKilos),
#                   Anticipos = sum(Anticipos),
#                   PctAnticipos = Anticipos/ unique(TotAnticipos))
#       
#       aux2$Bucket <- factor(aux2$Bucket, levels = c("Oferta al día","De 1 a 7 días","De 8 a 15 días","De 16 a 30 días",
#                                                     "De 31 a 60 días","De 61 a 90 días", "Más de 90 días"), ordered = T)  
#       
#       subplot(
#         plot_ly(aux2, x=~Ofertas, y=~Bucket, type = "bar",
#                 marker = list(color = "#1F618D",
#                               line = list(color = "black", width = 1)),
#                 hoverinfo = "text", hoverlabel = list(align = "left"),
#                 hovertext = paste0("<b>", aux2$Bucket, "</b>",
#                                    "<br>Ofertas: ", comma(aux2$Ofertas), "<b> (", 
#                                    percent(aux2$PctOfertas, accuracy = 0.1), ")</b>")
#         ),
#         plot_ly(aux2, x=~Kilos, y=~Bucket, type = "bar",
#                 marker = list(color = "#117A65",
#                               line = list(color = "black", width = 1)),
#                 hoverinfo = "text", hoverlabel = list(align = "left"),
#                 hovertext = paste0("<b>", aux2$Bucket, "</b>",
#                                    "<br>Kilos: ", comma(aux2$Kilos), "<b> (", 
#                                    percent(aux2$PctKilos, accuracy = 0.1), ")</b>")
#         ),
#         plot_ly(aux2, x=~Anticipos, y=~Bucket, type = "bar",
#                 marker = list(color = "#B03A2E",
#                               line = list(color = "black", width = 1)),
#                 hoverinfo = "text", hoverlabel = list(align = "left"),
#                 hovertext = paste0("<b>", aux2$Bucket, "</b>",
#                                    "<br>Anticipos: ", dollar(aux2$Anticipos), "<b> (", 
#                                    percent(aux2$PctAnticipos, accuracy = 0.1), ")</b>")
#         ),
#         nrows = 1,  shareY=TRUE, titleX = T) %>%  
#         layout(title = list(text=paste(""), 
#                             font=list(family = "Arial, sans-serif",size = 16,color = "black")),
#                xaxis = list(tickformat = "", fixedrange = TRUE, 
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ",", visible=T, fixedrange = TRUE,
#                             title=list(text=""),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                showlegend = F) %>% 
#         config(displayModeBar=F)
#       
#     })
#     output$BarrasAlturasVintKls <- renderPlotly({
#       
#       varplot="Kilos"
#       
#       aux0 <- liquidacion_f() %>% 
#         filter(OfeEst == "CUMPLIDA")
#       
#       aux1 <- aux0 %>%
#         filter(!is.na(Fecha)) %>% 
#         mutate(FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
#                FechaCumplimiento = if_else(FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento),
#                TOB = pmax(0,as.numeric(difftime(as.Date(Fecha), as.Date(FechaCumplimiento), units = "days")))) %>% 
#         select(SucCod, OfeNro, FecOferta, FechaCumplimiento, Fecha ,KilosOriginal, kilos, AntGirado, DescuentoAnt, TOB) %>% 
#         group_by(Vintage = ifelse(year(FechaCumplimiento) <2023, 
#                                   paste(year(FechaCumplimiento)),  
#                                   paste0(year(FechaCumplimiento), " Q", quarter(FechaCumplimiento)))) %>% 
#         filter(Vintage != 1973) %>% 
#         group_by(SucCod,OfeNro) %>% 
#         mutate(KilosTest = ifelse(row_number()==1, KilosOriginal, 0),
#                KilosAcum = cumsum(kilos),
#                Ultima = ifelse(row_number()==n(), 1, 0),
#                Verif = KilosOriginal-KilosAcum) %>% 
#         ungroup()
#       
#       aux2 <- aux1 %>% 
#         group_by(SucCod, OfeNro, Vintage) %>% 
#         summarise(Altura = max(TOB)) %>% 
#         mutate(Bucket = case_when(Altura <= 0 ~ "Oferta al día",
#                                   Altura <= 7 ~ "De 1 a 7 días",
#                                   Altura <= 15 ~ "De 8 a 15 días",
#                                   Altura <= 30 ~ "De 16 a 30 días",
#                                   Altura <= 60 ~ "De 31 a 60 días",
#                                   Altura <= 90 ~ "De 61 a 90 días",
#                                   T ~ "Más de 90 días"
#         )
#         ) %>% ungroup() %>% 
#         left_join(aux1 %>% 
#                     group_by(SucCod, OfeNro) %>%  
#                     summarise(KilosOriginal = max(KilosOriginal),
#                               Anticipos = sum(AntGirado)
#                     ), by = c("SucCod", "OfeNro")) %>% 
#         group_by(Vintage) %>% 
#         mutate(TotOfertas = n(),
#                TotKilos = sum(KilosOriginal),
#                TotAnticipos = sum(Anticipos)
#         ) %>% 
#         group_by(Bucket, Vintage) %>% 
#         summarise(Ofertas = n(),
#                   PctOfertas = Ofertas/ unique(TotOfertas),
#                   Kilos = sum(KilosOriginal),
#                   PctKilos = Kilos/ unique(TotKilos),
#                   Anticipos = sum(Anticipos),
#                   PctAnticipos = Anticipos/ unique(TotAnticipos)) %>% 
#         ungroup() %>% 
#         mutate(Var = !!sym(varplot),
#                VarPct = !!sym(paste0("Pct",varplot)))
#       
#       aux2$Bucket <- factor(aux2$Bucket, levels = c("Oferta al día","De 1 a 7 días","De 8 a 15 días","De 16 a 30 días",
#                                                     "De 31 a 60 días","De 61 a 90 días", "Más de 90 días"), ordered = T)  
#       
#       col = rev(RColorBrewer::brewer.pal(11, "RdYlGn"))
#       
#       plot_ly(aux2, x=~Vintage, y=~VarPct, color = ~Bucket, type = "bar", 
#               colors = col, hoverinfo = "text", hoverlabel = list(align = "left"),
#               hovertext = paste0("Cosecha: ", aux2$Vintage, 
#                                  "<br>Altura de Mora: ", aux2$Bucket, 
#                                  "<br>Ofertas: ", comma(aux2$Ofertas), "<b> (", 
#                                  percent(aux2$PctOfertas, accuracy = 0.1), ")</b>",
#                                  "<br>Kilos: ", comma(aux2$Kilos), "<b> (", 
#                                  percent(aux2$PctKilos, accuracy = 0.1), ")</b>",
#                                  "<br>Anticipos: ", dollar(aux2$Anticipos), "<b> (", 
#                                  percent(sierror_0(aux2$PctAnticipos), accuracy = 0.1), ")</b>"
#               )
#       ) %>% 
#         layout(barmode = 'stack',
#                title = list(text=paste(""), 
#                             font=list(family = "Arial, sans-serif",size = 16,color = "black")),
#                xaxis = list(tickformat = "", fixedrange = TRUE, 
#                             title=list(text=""),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ".0%", visible=T, fixedrange = TRUE,
#                             title=list(text=""),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07, 
#                              font=list(family = "Arial, sans-serif",size = 14,color = "black"))) %>% 
#         config(displayModeBar=F)
#       
#       
#     })
#     output$BarrasAlturasVintAnt <- renderPlotly({
#       
#       varplot="Anticipos"
#       
#       aux0 <- liquidacion_f() %>% 
#         filter(OfeEst == "CUMPLIDA")
#       
#       aux1 <- aux0 %>%
#         filter(!is.na(Fecha)) %>% 
#         mutate(FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
#                FechaCumplimiento = if_else(FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento),
#                TOB = pmax(0,as.numeric(difftime(as.Date(Fecha), as.Date(FechaCumplimiento), units = "days")))) %>% 
#         select(SucCod, OfeNro, FecOferta, FechaCumplimiento, Fecha ,KilosOriginal, kilos, AntGirado, DescuentoAnt, TOB) %>% 
#         group_by(Vintage = ifelse(year(FechaCumplimiento) <2023, 
#                                   paste(year(FechaCumplimiento)),  
#                                   paste0(year(FechaCumplimiento), " Q", quarter(FechaCumplimiento)))) %>% 
#         filter(Vintage != 1973) %>% 
#         group_by(SucCod,OfeNro) %>% 
#         mutate(KilosTest = ifelse(row_number()==1, KilosOriginal, 0),
#                KilosAcum = cumsum(kilos),
#                Ultima = ifelse(row_number()==n(), 1, 0),
#                Verif = KilosOriginal-KilosAcum) %>% 
#         ungroup()
#       
#       
#       aux2 <- aux1 %>% 
#         group_by(SucCod, OfeNro, Vintage) %>% 
#         summarise(Altura = max(TOB)) %>% 
#         mutate(Bucket = case_when(Altura <= 0 ~ "Oferta al día",
#                                   Altura <= 7 ~ "De 1 a 7 días",
#                                   Altura <= 15 ~ "De 8 a 15 días",
#                                   Altura <= 30 ~ "De 16 a 30 días",
#                                   Altura <= 60 ~ "De 31 a 60 días",
#                                   Altura <= 90 ~ "De 61 a 90 días",
#                                   T ~ "Más de 90 días"
#         )
#         ) %>% ungroup() %>% 
#         left_join(aux1 %>% 
#                     group_by(SucCod, OfeNro) %>%  
#                     summarise(KilosOriginal = max(KilosOriginal),
#                               Anticipos = sum(AntGirado)
#                     ), by = c("SucCod", "OfeNro")) %>% 
#         group_by(Vintage) %>% 
#         mutate(TotOfertas = n(),
#                TotKilos = sum(KilosOriginal),
#                TotAnticipos = sum(Anticipos)
#         ) %>% 
#         group_by(Bucket, Vintage) %>% 
#         summarise(Ofertas = n(),
#                   PctOfertas = Ofertas/ unique(TotOfertas),
#                   Kilos = sum(KilosOriginal),
#                   PctKilos = Kilos/ unique(TotKilos),
#                   Anticipos = sum(Anticipos),
#                   PctAnticipos = Anticipos/ unique(TotAnticipos)) %>% 
#         ungroup() %>% 
#         mutate(Var = !!sym(varplot),
#                VarPct = !!sym(paste0("Pct",varplot)))
#       
#       aux2$Bucket <- factor(aux2$Bucket, levels = c("Oferta al día","De 1 a 7 días","De 8 a 15 días","De 16 a 30 días",
#                                                     "De 31 a 60 días","De 61 a 90 días", "Más de 90 días"), ordered = T)  
#       
#       col = rev(RColorBrewer::brewer.pal(11, "RdYlGn"))
#       
#       plot_ly(aux2, x=~Vintage, y=~VarPct, color = ~Bucket, type = "bar", 
#               colors = col, hoverinfo = "text", hoverlabel = list(align = "left"),
#               hovertext = paste0("Cosecha: ", aux2$Vintage, 
#                                  "<br>Altura de Mora: ", aux2$Bucket, 
#                                  "<br>Ofertas: ", comma(aux2$Ofertas), "<b> (", 
#                                  percent(aux2$PctOfertas, accuracy = 0.1), ")</b>",
#                                  "<br>Kilos: ", comma(aux2$Kilos), "<b> (", 
#                                  percent(aux2$PctKilos, accuracy = 0.1), ")</b>",
#                                  "<br>Anticipos: ", dollar(aux2$Anticipos), "<b> (", 
#                                  percent(sierror_0(aux2$PctAnticipos), accuracy = 0.1), ")</b>"
#               )
#       ) %>% 
#         layout(barmode = 'stack',
#                title = list(text=paste(""), 
#                             font=list(family = "Arial, sans-serif",size = 16,color = "black")),
#                xaxis = list(tickformat = "", fixedrange = TRUE, 
#                             title=list(text=""),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ".0%", visible=T, fixedrange = TRUE,
#                             title=list(text=""),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07, 
#                              font=list(family = "Arial, sans-serif",size = 14,color = "black"))) %>% 
#         config(displayModeBar=F)
#       
#       
#     })
#     
#     
#     ### Facturacion -----
#     
#     output$VB_NumFacturas <- renderInfoBox({
#       
#       aux1 <- facturas_f() %>% select(SucCod, FCoNro) %>% distinct() %>% nrow()
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Facturas",
#         icon = icon("bars"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_KlsFacturas <- renderInfoBox({
#       
#       aux1 <- facturas_f()$kilos %>% sum()
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Kilos",
#         icon = icon("weight-scale"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_ValorFacturas <- renderInfoBox({
#       
#       aux1 <- facturas_f()$ValorFacturado %>% sum()
#       
#       infoBox(
#         value = dollar(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Valor Facturado",
#         icon = icon("money-bills"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_RecenciaFacturas <- renderInfoBox({
#       
#       aux1 <- rfm_f()$recency_score
#       aux2 <- rfm_f()$date_most_recent %>% as.Date()
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Score de Recencia",
#         icon = icon("arrow-up-wide-short"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_FrecuenciaFacturas <- renderInfoBox({
#       
#       aux1 <-  aux1 <- rfm_f()$frequency_score
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Score de Frecuencia",
#         icon = icon("arrow-up-wide-short"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_MontoFacturas <- renderInfoBox({
#       
#       aux1 <- aux1 <- rfm_f()$monetary_score
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Score de Volumen",
#         icon = icon("arrow-up-wide-short"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     
#     output$DistribuicionSucursal <- renderPlotly({
#       
#       facturas_f() %>% 
#         group_by(Sucursal) %>% 
#         summarise(Kilos = sum(kilos)) %>% 
#         plot_ly() %>%
#         add_pie(labels = ~ Sucursal, values = ~Kilos, name="Kilos", 
#                 type = 'pie', hole = 0.5, sort = F,
#                 marker = list(line = list(width = 2))) %>%
#         layout(title = "", 
#                legend = list(title = list(text = ""), 
#                              orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07)) %>% 
#         config(displayModeBar=F)
#       
#     })
#     output$DistribuicionNegocio <- renderPlotly({
#       
#       facturas_f() %>% 
#         group_by(TNeCod) %>% 
#         summarise(Kilos = sum(kilos)) %>% 
#         mutate(TNeCod = recode(TNeCod, "OFE"="Ofertas", "DIA"="Café al día", "CSN"="Café sin Negociar")) %>% 
#         plot_ly() %>%
#         add_pie(labels = ~ TNeCod, values = ~Kilos, name="Kilos", 
#                 type = 'pie', hole = 0.5, sort = F,
#                 marker = list(line = list(width = 2))) %>%
#         layout(title = "", 
#                legend = list(title = list(text = ""), 
#                              orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07)) %>% 
#         config(displayModeBar=F)
#       
#     })
#     output$SerieFacturasKilos <- renderDygraph({
#       
#       varplot="Kilos"
#       formato = ifelse(varplot=="Kilos", "coma", "dinero")
#       
#       Form <- FormatoJS(formato)
#       
#       aux0 <- facturas_f()
#       
#       aux1 <- aux0 %>% 
#         group_by(Fecha = floor_date(FCoFch, unit = "month")) %>% 
#         summarise(Kilos = sum(kilos),
#                   Valor = sum(ValorFacturado)) %>% 
#         mutate(Var = !!sym(varplot))
#       
#       m <- aux1 %>%
#         select(ds = Fecha, y = Var) %>%
#         prophet(interval.width = 0.70, weekly.seasonality=TRUE, daily.seasonality=TRUE)
#       
#       future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)
#       forecast <- predict(m, future)
#       
#       dyplot.prophet(m, forecast) %>% 
#         dyAxis("x", drawGrid = FALSE,
#                valueFormatter = 'function(d) { return moment(d).format("MMM-YY");}', 
#                axisLabelFormatter = 'function(d) { return moment(d).format("MMM-YY");}') %>% 
#         dyAxis("y", label = varplot, axisLabelWidth = 90, 
#                valueFormatter = Form, axisLabelFormatter = Form) %>% 
#         dyOptions(drawPoints = TRUE, pointSize = 2,strokeWidth = 2, 
#                   digitsAfterDecimal = 0,
#                   colors = c("#212F3D", "#196F3D"),
#                   gridLineColor = "#CCD1D1") 
#       
#     })
#     output$SerieFacturasValor <- renderDygraph({
#       
#       varplot="Valor"
#       formato = ifelse(varplot=="Kilos", "coma", "dinero")
#       
#       Form <- FormatoJS(formato)
#       
#       aux0 <- facturas_f()
#       
#       aux1 <- aux0 %>% 
#         group_by(Fecha = floor_date(FCoFch, unit = "month")) %>% 
#         summarise(Kilos = sum(kilos),
#                   Valor = sum(ValorFacturado)) %>% 
#         mutate(Var = !!sym(varplot)) 
#       
#       m <- aux1 %>%
#         select(ds = Fecha, y = Var) %>%
#         prophet(interval.width = 0.70, weekly.seasonality=TRUE, daily.seasonality=TRUE)
#       
#       future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = T)
#       forecast <- predict(m, future)
#       
#       dyplot.prophet(m, forecast) %>% 
#         dyAxis("x", drawGrid = FALSE,
#                valueFormatter = 'function(d) { return moment(d).format("MMM-YY");}', 
#                axisLabelFormatter = 'function(d) { return moment(d).format("MMM-YY");}') %>% 
#         dyAxis("y", label = varplot, axisLabelWidth = 90, 
#                valueFormatter = Form, axisLabelFormatter = Form) %>% 
#         dyOptions(drawPoints = TRUE, pointSize = 2,strokeWidth = 2, 
#                   digitsAfterDecimal = 0,
#                   colors = c("#212F3D", "#196F3D"),
#                   gridLineColor = "#CCD1D1") 
#       
#     })
#     
#     ### Entradas -----
#     
#     output$VB_NumEntradas <- renderInfoBox({
#       
#       aux1 <- entradas_f() %>% select(SucCod, RegNro) %>% distinct() %>% nrow()
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Entradas",
#         icon = icon("truck"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$VB_KlsEntradasBrutos <- renderInfoBox({
#       
#       aux1 <- entradas_f()$KilosNetos %>% sum()
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Kilos Brutos",
#         icon = icon("weight-scale"),
#         fill = T,
#         color = "black"
#       )
#     })  ### Arreglar.
#     output$VB_KlsEntradasNetos <- renderInfoBox({
#       
#       aux1 <- entradas_f()$KilosNetos %>% sum()
#       
#       infoBox(
#         value = comma(aux1, accuracy = 1),
#         title = "",
#         subtitle = "Kilos Netos",
#         icon = icon("weight-scale"),
#         fill = T,
#         color = "black"
#       )
#     }) 
#     output$DistribucionCalidad <- renderTable({
#       
#       entradas_f() %>% 
#         mutate(TotalKilos = sum(KilosNetos)) %>% 
#         group_by(Calidad = CalNom) %>%   
#         summarise(Kilos = comma(sum(KilosNetos)),
#                   Porcentaje = percent(sum(KilosNetos)/unique(TotalKilos))) %>% 
#         arrange(desc(Porcentaje))
#       
#     }, spacing = 'xs', width = "100%")
#     
#     output$MapaOrigenes <- renderLeaflet({
#       
#       aux0 <- entradas_f() 
#       
#       aux1 <- aux0 %>% 
#         group_by(PerRazSoc, NomDepPro,MunPro, LatPro, LngPro) %>% 
#         summarise(Lat = max(Lat),
#                   Lng = max(Lng),
#                   Entradas= n(),
#                   Kilos = sum(KilosNetos), 
#                   FactorPromedio = 96.8
#         )
#       
#       labs <- lapply(seq(nrow(aux1)), function(i) {
#         paste0("<b>Departamento: </b>", aux1[i, "NomDepPro"],
#                "<br><b>Municipio: </b>", aux1[i, "MunPro"],
#                "<br><b>Entradas: </b>", aux1[i, "Entradas"],
#                "<br><b>Kilos: </b>", aux1[i, "Kilos"],
#                "<br><b>FactorPromedio: </b>", aux1[i, "FactorPromedio"]
#         ) 
#       })
#       
#       leaflet(options = leafletOptions(minZoom = 5, maxZoom = 10, zoomControl = FALSE)) %>% 
#         setView(lat = max(aux1$Lat), lng = max(aux1$Lng), zoom = 7) %>% 
#         addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
#         addMarkers(data = aux1, lng = ~Lng, lat = ~Lat, label = ~PerRazSoc) %>% 
#         addHeatmap(data = aux1, lng = ~LngPro, lat = ~LatPro, intensity = ~Kilos, max = 0.1, blur = 35, group = "Mapa de Calor") %>% 
#         addCircleMarkers(data = aux1, lng = ~LngPro, lat = ~LatPro, radius = ~rescale(aux1$Kilos, to = c(10,25)), 
#                          label = lapply(labs, htmltools::HTML), group = "Radial", stroke = F) %>% 
#         addLayersControl(
#           baseGroups = c("Mapa de Calor", "Radial"),
#           options = layersControlOptions(collapsed = T)
#         )
#       
#       
#     })
#     output$DetalleMapa <- renderDataTable({
#       
#       aux0 <- entradas_f() 
#       
#       aux1 <- aux0 %>% 
#         group_by(Mun, NomDepPro,MunPro) %>% 
#         summarise(Entradas= n(),
#                   Kilos = sum(KilosNetos), 
#                   FactorPromedio = mean(FactorRendimiento, na.rm=T)
#         )
#       
#       noms <- c("Municipio Cliente", "Depto. Procedencia", "Mpio. Procedencia", "Entradas", "Kilos", "Factor Promedio")
#       
#       datatable(aux1, escape = F, rownames=F, colnames = noms, extensions = "FixedColumns", class = "compact",
#                 options=list(pageLength =6, dom = 'tp', searching= F, autoWidth = F, ordering=T,
#                              list(list(className = 'dt-center', targets = 1)),
#                              scrollX = "10px", autoWidth = F,
#                              columnDefs = list(list(width = '19%', targets = 1))
#                 )) %>%
#         formatRound(4:5, digits = 0) %>% 
#         formatRound(6, digits = 2) %>% 
#         formatStyle(1:ncol(aux1), lineHeight='90%')
#       
#       
#       
#       
#     })
#     
#     output$SerieFactorRendimiento <- renderPlotly({
#       
#       aux0 <- entradas_f() 
#       
#       aux1 <- aux0 %>% 
#         group_by(Fecha = floor_date(RegFchEnt, unit = "month")) %>% 
#         summarise(Valor = weighted.mean(FactorRendimiento, KilosNetos)
#         )
#       
#       plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Valor, type = "scatter", mode="lines+markers",
#               line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Compras",
#               textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1),
#               hoverlabel = list(align = "left"), hoverinfo = "text",
#               hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
#                                  "<br>","Factor de Rendimiento"," :", comma(aux1$Valor, accuracy = 0.01))
#       ) %>% 
#         layout(title = list(text="Factor de Rendimiento",
#                             font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
#                xaxis = list(gridcolor="#CCD1D1",
#                             title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                yaxis = list(tickformat = "s", rangemode = "tozero",
#                             title= list(text= "Factor", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22,
#                              font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>%
#         config(displayModeBar=F)
#       
#     })
#     output$CajaFactorRendimiento <- renderPlotly({
#       
#       aux0 <- entradas_f() 
#       
#       aux1 <- aux0 %>% mutate(Var = FactorRendimiento) %>% select(Var)
#       
#       plot_ly(data = aux1, y = ~Var, type = 'box', box = list(visible = T),
#               meanline = list(visible = T), points = FALSE, name="_") %>% 
#         layout(title = list(text="", 
#                             font=list(family = "Arial, sans-serif",size = 18,color = "black")),
#                xaxis = list(tickformat = "",
#                             title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "black")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ",",  rangemode = "tozero",
#                             title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "black")), 
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07, 
#                              font=list(family = "Arial, sans-serif",size = 14,color = "black"))) %>% 
#         config(displayModeBar=F) 
#       
#     })
#     output$SerieHumedad <- renderPlotly({
#       
#       aux0 <- entradas_f() 
#       
#       aux1 <- aux0 %>% 
#         group_by(Fecha = floor_date(RegFchEnt, unit = "month")) %>% 
#         summarise(Valor = weighted.mean(EvaPorHum, KilosNetos)
#         )
#       
#       plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Valor, type = "scatter", mode="lines+markers",
#               line = list(width = 2, color = "#212F3D"), marker = list(size = 5, color = "#212F3D"), name = "Compras",
#               textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1),
#               hoverlabel = list(align = "left"), hoverinfo = "text",
#               hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
#                                  "<br>","Humedad"," :", comma(aux1$Valor, accuracy = 0.01))
#       ) %>% 
#         layout(title = list(text="Humedad",
#                             font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
#                xaxis = list(gridcolor="#CCD1D1",
#                             title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                yaxis = list(tickformat = "s", rangemode = "tozero",
#                             title= list(text= "Humedad (%)", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22,
#                              font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>%
#         config(displayModeBar=F)
#       
#     })
#     output$CajaHumedad <- renderPlotly({
#       
#       aux0 <- entradas_f() 
#       
#       aux1 <- aux0 %>% mutate(Var = EvaPorHum) %>% select(Var)
#       
#       plot_ly(data = aux1, y = ~Var, type = 'box', box = list(visible = T),
#               meanline = list(visible = T), points = FALSE, name="_") %>% 
#         layout(title = list(text="", 
#                             font=list(family = "Arial, sans-serif",size = 18,color = "black")),
#                xaxis = list(tickformat = "",
#                             title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "black")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ",", rangemode = "tozero",
#                             title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "black")), 
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07, 
#                              font=list(family = "Arial, sans-serif",size = 14,color = "black"))) %>% 
#         config(displayModeBar=F) 
#       
#     })
#     output$SerieGranulometria <- renderPlotly({
#       
#       aux0 <- entradas_f() 
#       
#       aux1 <- aux0 %>% 
#         group_by(Fecha = floor_date(RegFchEnt, unit = "month")) %>% 
#         summarise(PctExcelso = mean(PctExcelso),
#                   PctPasilla = mean(PctPasilla),
#                   PctRipio = mean(PctRipio),
#                   PctMerma = mean(PctMerma)
#         ) %>% 
#         pivot_longer(PctExcelso:PctMerma, names_to = "Resultado", values_to = "Valor") %>% 
#         mutate(Resultado = gsub( "Pct", "", Resultado))
#       
#       plot_ly(data= aux1, x = aux1$Fecha , y= aux1$Valor, type = "scatter", mode="lines+markers", color = aux1$Resultado,
#               line = list(width = 2), marker = list(size = 5), 
#               textposition = 'bottom', text = ~comma(aux1$Valor, accuracy = 1),
#               hoverlabel = list(align = "left"), hoverinfo = "text",
#               hovertext = paste0("<b>", str_to_title(format(aux1$Fecha, "%B %Y")), "</b>",
#                                  "<br>",aux1$Resultado," :", percent(aux1$Valor, accuracy = 0.01))
#       ) %>% 
#         layout(title = list(text="Granulometría",
#                             font=list(family = "Arial, sans-serif",size = 18,color = "#17202A")),
#                xaxis = list(gridcolor="#CCD1D1",
#                             title=list(text="Fecha", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                yaxis = list(tickformat = ".0%", rangemode = "tozero",
#                             title= list(text= "", font= list(family = "Arial, sans-serif",size = 16,color = "#17202A")),
#                             gridcolor="#CCD1D1", tickfont= list(family = "Arial, sans-serif",size = 14,color = "#17202A")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.22,
#                              font=list(family = "Arial, sans-serif",size = 14,color = "#17202A"))) %>%
#         config(displayModeBar=F)
#       
#     })
#     output$CajaGranulometria <- renderPlotly({
#       
#       aux0 <- entradas_f() 
#       
#       aux1 <- aux0 %>% 
#         select(PctExcelso , PctPasilla , PctRipio , PctMerma ) %>% 
#         pivot_longer(PctExcelso:PctMerma, names_to = "Resultado", values_to = "Var") %>% 
#         mutate(Resultado = gsub( "Pct", "", Resultado))
#       
#       plot_ly(data = aux1, x= ~Resultado, y = ~Var, type = 'box', box = list(visible = T), split = ~Resultado,
#               meanline = list(visible = T), points = FALSE) %>% 
#         layout(title = list(text="", 
#                             font=list(family = "Arial, sans-serif",size = 18,color = "black")),
#                xaxis = list(tickformat = "",
#                             title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "black")),
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                yaxis = list(tickformat = ".0%", rangemode = "tozero",
#                             title=list(text="", font= list(family = "Arial, sans-serif",size = 16,color = "black")), 
#                             tickfont= list(family = "Arial, sans-serif",size = 14,color = "black")
#                ),
#                paper_bgcolor='rgba(0,0,0,0)',
#                plot_bgcolor='rgba(0,0,0,0)',
#                legend = list(orientation = 'h', xanchor = "center",  x = 0.5, y = -0.07, 
#                              font=list(family = "Arial, sans-serif",size = 14,color = "black"))) %>% 
#         config(displayModeBar=F) 
#       
#     })
#     
#     ### Comunicacion  -----
#     ### Edicion -----
#     
#     
#     
#     
#   })
# }