Sys.setlocale("LC_TIME", "es_ES.UTF-8")
options(tidyverse.quiet = TRUE)
# Librerias ----
library(shiny)
library(bslib)
library(shinyGizmo)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(tidyverse)
library(DT)
library(plotly)
library(scales)
library(lubridate)
library(openxlsx)
library(rlang)
library(daterangepicker)
library(phosphoricons)
library(DBI)
source("https://raw.githubusercontent.com/AnaliticaRacafe/ShinyApps/main/Funciones.r")

# Funciones ----
Unicos <- function(x){
  res <- if (is.factor(x)) {
    levels(x)
  } else {
    sort(unique(x))
  }
  return(res)
}
Formatod3 <- function(formato){
  # Define el formato para un numero segun su tipo: coma, numero, dinero, porcentaje
  formato <- if(formato == "coma"){
    ",.0"
  } else if(formato == "numero"){
    ",.3"
  } else if(formato == "dinero"){
    "$,"
  } else if(formato == "porcentaje"){
    ",.0%"
  }
  return(formato)
}


# Datos ------
load("data/data.RData")
# load("data/dataOld.RData")

# Modulos ----
source("modules/modules.R", encoding = "UTF-8")

# Tabs ----
source("misc/tabs/generalidades_tab.R", encoding = "UTF-8")
source("misc/tabs/ofertas_tab.R", encoding = "UTF-8")
source("misc/tabs/entradas_tab.R", encoding = "UTF-8")
source("misc/tabs/facturacion_tab.R", encoding = "UTF-8")
source("misc/tabs/liquidacion_tab.R", encoding = "UTF-8")
source("misc/tabs/individual_tab.R", encoding = "UTF-8")
source("misc/tabs/registro_tab.R", encoding = "UTF-8")

# Values ----
lang <- list(url = '//cdn.datatables.net/plug-ins/1.13.1/i18n/es-ES.json')



