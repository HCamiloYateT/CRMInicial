construir_cosechas <- function(df, varplot = "Kilos", paleta = paleta) {
  
  aux0 <- df %>% filter(OfeEst == "Cumplida")
  
  aux1 <- aux0 %>%
    filter(!is.na(Fecha)) %>%
    mutate(
      FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
      FechaCumplimiento = if_else(
        FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento
      ),
      TOB = pmax(0, as.numeric(difftime(as.Date(Fecha),
                                        as.Date(FechaCumplimiento), units = "days")))
    ) %>%
    select(SucCod, OfeNro, FecOferta, FechaCumplimiento,
           Fecha, KilosOriginal, kilos, AntGirado, DescuentoAnt, TOB) %>%
    mutate(
      # Granularidad dinámica: mes (año vigente), trimestre (2 años ant.), año (resto)
      Vintage = .vintage_label(FechaCumplimiento)
    ) %>%
    filter(!Vintage %in% c("1973", "1753")) %>%
    group_by(SucCod, OfeNro) %>%
    mutate(
      KilosAcum = cumsum(kilos),
      Verif     = KilosOriginal - KilosAcum
    ) %>%
    ungroup()
  
  aux2 <- aux1 %>%
    group_by(Vintage, TOB) %>%
    summarise(
      kilos             = sum(kilos),
      AnticipoGirado    = sum(AntGirado),
      AnticipoDescontado = sum(DescuentoAnt),
      .groups = "drop"
    ) %>%
    left_join(
      aux1 %>%
        select(OfeNro, Vintage, KilosOriginal) %>%
        distinct() %>%
        group_by(Vintage) %>%
        summarise(KilosOriginal = sum(KilosOriginal)),
      by = "Vintage"
    ) %>%
    group_by(Vintage) %>%
    mutate(
      Kilos     = (KilosOriginal - cumsum(kilos)) / KilosOriginal,
      Anticipos = (cumsum(AnticipoGirado) - cumsum(AnticipoDescontado)) /
        cumsum(AnticipoGirado),
      Var       = !!sym(varplot),
      Var       = if_else(Var < 0, 0, Var)
    ) %>%
    complete(TOB = 0:90) %>%
    filter(TOB <= 90) %>%
    mutate(Var = if_else(TOB == 0, 1, Var)) %>%
    fill(Var, .direction = "up")
  
  # Ordenar vintages cronológicamente como factor antes del plot
  # ungroup obligatorio: factor con niveles globales requiere ver todos los grupos a la vez
  aux2 <- aux2 %>% ungroup() %>% mutate(Vintage = .vintage_factor(Vintage))
  ncols   <- nlevels(aux2$Vintage)
  # rep_len cicla la paleta si hay más vintages que colores disponibles
  cols    <- rep_len(paleta, ncols)
  names(cols) <- levels(aux2$Vintage)
  
  plot_ly(
    data        = aux2, x = ~TOB, y = ~Var,
    type        = "scatter", mode = "lines",
    color       = ~Vintage, colors = cols,
    line        = list(width = 2),
    hoverinfo   = "text", hoverlabel = list(align = "left"),
    hovertext   = paste0(
      "<b>", aux2$Vintage, "</b>",
      "<br>Días de cumplimiento: ", comma(aux2$TOB, accuracy = 1),
      "<br>Saldo ", varplot, ": ", percent(aux2$Var, accuracy = 0.1)
    )
  ) %>%
    layout(
      shapes = list(
        vline(30, color = "#D35400"),
        vline(60, color = "red")
      ),
      title = list(
        text = paste("Cosechas (", varplot, ")"),
        font = list(family = "Arial, sans-serif", size = 18, color = "black")
      ),
      xaxis = list(
        title    = list(text = "Días después de oferta"),
        tickfont = list(family = "Arial, sans-serif", size = 14)
      ),
      yaxis = list(
        tickformat = ",.0%",
        title      = list(text = "Porcentaje de Saldo"),
        tickfont   = list(family = "Arial, sans-serif", size = 14)
      ),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.18)
    ) %>%
    config(displayModeBar = FALSE)
}