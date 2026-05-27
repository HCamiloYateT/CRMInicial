# Parámetros globales de la aplicación ----

## Paleta de colores para curvas de cosecha (vintages) ----
# Se toman los primeros n colores según la cantidad de vintages disponibles
paleta <- c(
  "#212F3D", "#1A5276", "#117A65", "#784212", "#6C3483",
  "#1F618D", "#B03A2E", "#D68910", "#2C3E50", "#0E6655",
  "#7D6608", "#512E5F", "#1B2631", "#0B5345", "#6E2F1A"
)

## Colores por sucursal para Sankey de ofertas ----
# IMPORTANTE: los nombres deben coincidir exactamente con los valores de Sucursal
# después de aplicar LimpiarNombres() en el ETL. Verificar contra unique(Ofertas$Sucursal).
colores_sucursal <- c(
  "Trilladora 12" = "#212F3D",
  "Bachue"        = "#1A5276",
  "Medellin"      = "#117A65",
  "Popayan"       = "#784212",
  "Armenia"       = "#6C3483",
  "Arenales"      = "#1F618D",
  "Pereira"       = "#B03A2E",
  "Bucaramanga"   = "#D68910",
  "Huila"         = "#2C3E50"
)

## Colores por estado de oferta para Sankey ----
# Valores post-recode del ETL: Cumplida, Pendiente, Anulada, Cancelada, etc.
colores_estado <- c(
  "Cumplida"       = "#1E8449",
  "Pendiente"      = "#D4AC0D",
  "Anulada"        = "#C0392B",
  "Cancelada"      = "#E74C3C",
  "Trasladada"     = "#2E86C1",
  "Cruzada"        = "#5D6D7E",
  "Cobro Juridico" = "#884EA0"
)

## IDs de clientes bloqueados para transacciones ----
# Poblar con los IDs reales según la fuente de datos de riesgo
bloqueados <- c()

## Tabla de usuarios, grupos y accesos por trilladora ----
# Trilladora: "TODAS" o el nombre exacto (post-LimpiarNombres) de la sucursal asignada
tabla_usuarios <- dplyr::tibble(
  usuario    = c("HCYATE",  "ADMIN"),
  grupo      = c("ANALITICA", "ADMIN"),
  Trilladora = c("TODAS",   "TODAS")
)
