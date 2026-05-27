function(input, output, session) {
  
  # Generalidades -----
  output$TextoGeneralidades <- renderUI({
    h5(paste("Indicadores al", format(Sys.Date()-1, "%d de %B del %Y")))
  })
  output$Gen_Ind_Existencias <- renderDataTable({
    req(input$GenSucursal)
    conn_cafe <- dbConnect(odbc::odbc(),
                           Driver = "ODBC Driver 18 for SQL Server",
                           Server = "172.16.19.21",
                           Database = "ContabRacafe",
                           uid = "AppQlikRcaLee",
                           pwd = "Rac@CafQlikS2022*",
                           port = 1433,
                           TrustServerCertificate="yes")
    
    aux1 <- dbGetQuery(conn_cafe, paste0("select * from DSPMAT where ciacod = 10 and SucCod <> 12
                                         and DspFac <> 4 and DspFchPro >='",
                                         format(Sys.Date()-1, "%Y-%d-%m"), "'")) %>%
      left_join(sucs, by = join_by(SucCod)) %>%
      filter(Sucursal == input$GenSucursal) %>%
      mutate(DspFchPro = as.Date(DspFchPro)) %>% 
      filter(DspFchPro == max(DspFchPro)) %>% 
      group_by(Fecha=DspFchPro, Sucursal, Item = ifelse(DspTipCaf == 1 & DspCalCod == 1, "PERGAMINO SECO", "PERGAMINO ESPECIAL")) %>%
      summarise(Sacos = sum(DspEntKls)/70) %>%
      bind_rows((.) %>%
                  group_by(Fecha, Sucursal, Item  = "TOTAL") %>%
                  summarise(Sacos = sum(Sacos))) %>%
      ungroup() %>% 
      select(-Fecha) %>% 
      select(Item, Sacos)
    
    DBI::dbDisconnect(conn_cafe)
    
    v1 <- aux1 %>% select(1) %>% .[[1]]
    cols1 <- ifelse(v1 == 'TOTAL','bold','normal')
    
    datatable(aux1, escape = F, rownames=F, class = "compact", colnames = c("", "Sacos 70kg"),
              selection=list(target='row', mode = "single"), style = "default",
              options = list(pageLength =nrow(aux1), ordering=F, dom="t",
                             language = lang)) %>%
      formatRound("Sacos", digits = 0) %>% 
      formatStyle(1, target = "row", fontWeight = styleEqual(v1, cols1))
    
  })
  output$Gen_Ind_CSN <- renderDataTable({
    req(input$GenSucursal)
    conn_cafe <- dbConnect(odbc::odbc(),
                           Driver = "ODBC Driver 18 for SQL Server",
                           Server = "172.16.19.21",
                           Database = "ContabRacafe",
                           uid = "AppQlikRcaLee",
                           pwd = "Rac@CafQlikS2022*",
                           port = 1433,
                           TrustServerCertificate="yes")
    
    aux1 <- dbGetQuery(conn_cafe, paste0("select * from DSPMAT where ciacod = 10 and SucCod <> 12 
                                                      and DspFac = 4 and DspFchPro >='", 
                                         format(Sys.Date()-1, "%Y-%d-%m"), "'")) %>% 
      mutate(DspFchPro = as.Date(DspFchPro)) %>% 
      filter(DspFchPro == max(DspFchPro)) %>% 
      left_join(sucs, by = join_by(SucCod)) %>% 
      group_by(Fecha=DspFchPro, Sucursal) %>% 
      summarise(`KILOS DE CSN CON ANTICIPO` = comma(sum(DspCsnAK), accuracy = 1),
                `KILOS DE CSN SIN ANTICIPO` = comma(sum(DspCsnKls - DspCsnAK), accuracy = 1),
                `TOTAL KILOS DE CSN` = comma(sum(DspCsnKls), accuracy = 1),
                `SALDO DE ANTICIPOS CSN (COP)` = dollar(sum(DspCsnAV), accuracy = 1)) %>% 
      pivot_longer(`KILOS DE CSN CON ANTICIPO`:`SALDO DE ANTICIPOS CSN (COP)`, names_to = "Item", values_to = "Valor") %>% 
      ungroup() %>%
      select(-Fecha) %>%
      filter(Sucursal == input$GenSucursal) %>%
      select(Item, Valor)
    
    DBI::dbDisconnect(conn_cafe)
    
    v1 <- aux1 %>% select(1) %>% .[[1]]
    cols1 <- ifelse(v1 == 'TOTAL','bold','normal')

    datatable(aux1, escape = F, rownames=F, class = "compact", colnames = c("", "Valor"),
              selection=list(target='row', mode = "single"), style = "default",
              options = list(pageLength =nrow(aux1), ordering=F, dom="t",
                             columnDefs = list(list(className = 'dt-right', targets = 1)),
                             language = lang)) %>% 
      formatStyle(1, target = "row", fontWeight = styleEqual(v1, cols1))
  })
  ofertas_f <- reactive({
    req(input$GenSucursal)
    OFEPEN %>% 
      filter(Sucursal == input$GenSucursal) %>% 
      group_by(RangoDias = str_to_upper(RangoDias)) %>% 
      summarise(NumOfertas = n(),
                KilosOfertas = sum(OPenASldK),
                SaldoAnticipo = sum(OPenAAntV)
      ) %>% 
      arrange(RangoDias) %>% 
      janitor::adorn_totals("row", name = "TOTAL") %>% 
      mutate(RangoDias = factor(RangoDias, 
                                c("DE 0 A 7 DÍAS", "DE 8 A 15 DÍAS", "DE 16 A 30 DÍAS", "DE 31 A 60 DÍAS", "MÁS DE 60 DÍAS", "TOTAL"), 
                                ordered = T)) %>% 
      arrange(RangoDias)
  })
  output$Gen_Ofertas <- renderDataTable({
    
    aux1 <- ofertas_f() %>% 
      AdicionarBotonDetalle
    
    filas <- 1:nrow(aux1)
    colus <- ncol(aux1)-1
    mat <- expand.grid(filas, colus) %>% as.matrix
    v1 <- aux1 %>% select(1) %>% .[[1]]
    cols1 <- ifelse(v1 == 'TOTAL','bold','normal')
    noms <- c("Altura Mora", "Ofertas", "Kilos Pendientes", "Saldo Anticipos", "")
    
    datatable(aux1, escape = F, rownames=F, class = "compact", colnames = noms, style = "default",
              selection = list(target='cell', mode = "single", selectable = mat),
              options = list(pageLength =nrow(aux1), ordering=F, dom="t",
                             columnDefs = list(list(width = "1%", className = 'dt-center', targets =colus)),
                             language = lang)) %>% 
      formatRound(c(2,3), digits = 0) %>% 
      formatCurrency(4, digits = 0) %>% 
      formatStyle(1, target = "row", fontWeight = styleEqual(v1, cols1))
    
  })
  output$DetalleOFEPEN <- renderDataTable({
    req(input$GenSucursal)
    
    seleccion = ofertas_f()[input$Gen_Ofertas_cells_selected[1],1] %>% .[[1]]
    aux1 <- OFEPEN %>%
      filter(Sucursal == input$GenSucursal,
             if(seleccion=="TOTAL")T else RangoDias == seleccion
             ) %>% 
      select(OPenANum, RazonSocial, Asociado, Dias, OPenASldK, OPenAAntV) 
    
    noms <- c("Num Oferta", "Cooperativa", "Asociado", "Días de Altura", "Saldo Kilos", "Saldo Anticipos")
    
    datatable(aux1, escape = F, rownames=F, class = "compact", colnames = noms, style = "default",
              extensions =  c('Buttons', 'FixedColumns', 'FixedHeader'),
              selection = "none",
              options = list(pageLength =nrow(aux1), ordering=T, dom="tB",
                             buttons = list( list(extend = "excel", text = 'Descargar <span class="glyphicon glyphicon-download"> </span>',
                                                  filename=paste0("Descarga", format(Sys.Date(), "%d%m%Y")), className='copyButton')
                                             ),
                             language = lang)) %>% 
      formatRound(c(4,5), digits = 0) %>% 
      formatCurrency(6, digits = 0) 
    

    
    
  })
  observe({
    req(input$Gen_Ofertas_cells_selected)
    if (!is.na(input$Gen_Ofertas_cells_selected[1])) {
      showModalUI("DetalleOFEPEN")
    }
  })
  observeEvent(input$Cerrar_DetalleOFEPEN, {
    hideModalUI("DetalleOFEPEN")
  })
  
  
  # Ofertas -----
  
  # Entradas -----
  ## Interactividad de los Inputs ----
  observe({
    aux1 <- Entradas %>% filter(Sucursal %in% input$EntSucursal)
    
    aux2 <- aux1 %>% filter(if(input$EntTodasFec) T else RegFchEnt >= input$EntFecha[1] & RegFchEnt <= input$EntFecha[2])
    updatePickerInput(session, "EntCalidad", 
                      choices = Unicos(aux2$CalNom),
                      selected =  Unicos(aux2$CalNom),
                      options = pick_opt(Unicos(aux2$CalNom), fem = F))
    
    
    updatePickerInput(session, "EntCooperativa", 
                      choices = Unicos(aux2$RazonSocial),
                      selected =  Unicos(aux2$RazonSocial),
                      options = pick_opt(Unicos(aux2$RazonSocial), fem = F))
    
    
    updatePickerInput(session, "EntAsociado", 
                      choices = Unicos(aux2$Asociado),
                      selected =  Unicos(aux2$Asociado),
                      options = pick_opt(Unicos(aux2$Asociado), fem = F))
    
    
    updatePickerInput(session, "EntDepto", 
                      choices = Unicos(aux2$Departamento),
                      selected =  Unicos(aux2$Departamento),
                      options = pick_opt(Unicos(aux2$Departamento), fem = F))
    
    
    updatePickerInput(session, "EntMunicipio", 
                      choices = Unicos(aux2$Municipio),
                      selected =  Unicos(aux2$Municipio),
                      options = pick_opt(Unicos(aux2$Municipio), fem = F))
    
    
  })
  
  ## Datos Reactivos----
  entradas_f <- reactive({
    req(input$EntSucursal)
    Entradas %>% 
      filter(Sucursal == input$EntSucursal,
             if(input$EntTodasFec) T else RegFchEnt >= input$EntFecha[1] & RegFchEnt <= input$EntFecha[2],
             CalNom %in% input$EntCalidad,
             RazonSocial %in% input$EntCooperativa,
             Asociado %in% input$EntAsociado,
             Departamento %in% input$EntDepto,
             Municipio %in% input$EntMunicipio
             )
    })
  
  ## Resultados ----
  TablaDimension("Calidad", entradas_f, "CalNom")
  Cliente("Proveedor", entradas_f)
  
  TablaDimension("Cooperativa", entradas_f, "RazonSocial")
  TablaDimension("Asociado", entradas_f, "Asociado")
  TablaDimension("Departamento", entradas_f, "Departamento")
  TablaDimension("Municipio", entradas_f, "Municipio")
  
  output$EVAs <- renderDataTable({
    
    aux1 <- Entradas %>% 
      filter(Sucursal == "HUILA",
             RegFchEnt >= PrimerDia(Sys.Date())) %>% 
      select(RegNro, PrecioCarga, KilosNegoc, FactorRendimiento, PctExcelso:PctMerma) %>% 
      summarise(PrecioCarga = dollar(weighted.mean(PrecioCarga, KilosNegoc, na.rm = T), accuracy= 1),
                FactorRendimiento = comma(weighted.mean(FactorRendimiento, KilosNegoc, na.rm = T), accuracy= 0.01),
                PctExcelso = percent(weighted.mean(PctExcelso, KilosNegoc, na.rm = T), accuracy= 0.01),
                PctConsumo = percent(weighted.mean(PctConsumo, KilosNegoc, na.rm = T), accuracy= 0.01),
                PctPasilla = percent(weighted.mean(PctPasilla, KilosNegoc, na.rm = T), accuracy= 0.01),
                PctRipio = percent(weighted.mean(PctRipio, KilosNegoc, na.rm = T), accuracy= 0.01),
                PctMerma = percent(weighted.mean(PctMerma, KilosNegoc, na.rm = T), accuracy= 0.01)) %>% 
      pivot_longer(PrecioCarga:PctMerma) %>% 
      mutate(name = str_to_upper(recode(name, 
                                        "PrecioCarga"="Precio de Carga", 
                                        "FactorRendimiento"="Factor de Rendimiento",
                                        "PctExcelso"="Porcentaje de Excelso",
                                        "PctConsumo"="Porcentaje de Consumo",
                                        "PctPasilla"="Porcentaje de Pasilla",
                                        "PctRipio"="Porcentaje de Ripio",
                                        "PctMerma"="Porcentaje de Merma"))) %>% 
      AdicionarBotonDetalle()
    
    filas <- 1:nrow(aux1)
    colus <- ncol(aux1)-1
    mat <- expand.grid(filas, colus) %>% as.matrix
    noms <- c("Item", "Promedio Ponderado", "")
    
    datatable(aux1, escape = F, rownames=F, colnames = noms, extensions = "FixedColumns",
              selection = list(target='cell', mode = "single", selectable = mat), style = "default",
              options=list(pageLength = nrow(aux1), dom = 't', searching= T,scrollX=0.1,
                           autoWidth = F, ordering= T, autoWidth = F,
                           columnDefs = list(list(width = "70%", targets = 0),
                                             list(width = "29%", className = 'dt-right', targets = 1),
                                             list(width = "1%", className = 'dt-center', targets =colus)
                           ),
                           language = lang))
    
    
  })
  EVA("DetalleEVA", entradas_f)
  observe({
    req(input$EVAs_cells_selected)
    if (!is.na(input$EVAs_cells_selected[1])) {
      showModalUI("DetalleEVA")
    }
  })
  observeEvent(input$Cerrar_DetalleEVA, {
    hideModalUI("DetalleEVA")
  })
  
  
  # Facturas -----
  ## Liquidacion ----
  
  # Consulta Individual -----
  # Registro individual -----
}
