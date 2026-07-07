# Funciones auxiliares de formateo y layout ----

## Helper: línea vertical para shapes de plotly ----
# Retorna una lista compatible con layout(shapes = list(vline(...)))
vline <- function(x, color = "red", dash = "dash", width = 1) {
  list(
    type = "line",
    x0 = x, x1 = x,
    y0 = 0, y1 = 1, yref = "paper",
    line = list(color = color, dash = dash, width = width)
  )
}

## Helper: formateador JS para eje Y de dygraph ----
# formato: "coma"   → número con separador de miles en es-CO
# formato: "dinero" → igual pero con prefijo "$"
FormatoJS <- function(formato = "coma") {
  switch(
    formato,
    coma   = paste0(
      "function(v,opts,sName,g,row,col){",
      "return v.toLocaleString('es-CO',{maximumFractionDigits:0});}"
    ),
    dinero = paste0(
      "function(v,opts,sName,g,row,col){",
      "return '$'+v.toLocaleString('es-CO',{maximumFractionDigits:0});}"
    ),
    "function(v){return v;}"
  )
}

## Helper: etiqueta de vintage con granularidad dinámica ----
# Año vigente   → "AAAA Mes"  (p.ej. "2026 May")
# Dos años ant. → "AAAA Q#"  (p.ej. "2025 Q1")
# Resto         → "AAAA"     (p.ej. "2023")
.vintage_label <- function(fecha) {
  meses_es      <- c("Ene","Feb","Mar","Abr","May","Jun",
                     "Jul","Ago","Sep","Oct","Nov","Dic")
  anio_actual   <- lubridate::year(Sys.Date())
  anio_trim_min <- anio_actual - 2L
  dplyr::case_when(
    lubridate::year(fecha) == anio_actual   ~
      paste0(lubridate::year(fecha), " ", meses_es[lubridate::month(fecha)]),
    lubridate::year(fecha) >= anio_trim_min ~
      paste0(lubridate::year(fecha), " Q",  lubridate::quarter(fecha)),
    TRUE ~
      as.character(lubridate::year(fecha))
  )
}

## Helper: clave numérica para ordenar etiquetas de vintage cronológicamente ----
# Permite crear factors ordenados sin perder la etiqueta legible
.vintage_orden <- function(label) {
  meses_es <- c("Ene","Feb","Mar","Abr","May","Jun",
                "Jul","Ago","Sep","Oct","Nov","Dic")
  es_mes  <- grepl("^\\d{4} [A-Za-z]{3}$", label)
  es_trim <- grepl("^\\d{4} Q[1-4]$",      label)
  anio    <- as.numeric(substr(label, 1, 4))
  ifelse(
    es_mes,
    anio * 10000L + match(substr(label, 6, 8), meses_es) * 100L,
    ifelse(
      es_trim,
      anio * 100L + as.numeric(substr(label, 7, 7)),
      anio
    )
  )
}

## Helper: convierte vector de etiquetas a factor cronológicamente ordenado ----
.vintage_factor <- function(x) {
  lvls <- unique(x)
  lvls <- lvls[order(.vintage_orden(lvls))]
  factor(x, levels = lvls, ordered = TRUE)
}
