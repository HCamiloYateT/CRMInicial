tictoc::tic("CRM Cliente Inicial")
print(paste0("*********** ", Sys.time(), " ***********"))
setwd(here::here())
Sys.setenv(LANG = "es_CO.UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")
options(OutDec = ".", scipen = 999, lubridate.week.start = 1,
        repos = c(CRAN = "https://cloud.r-project.org"))

# 1. Librerias ----
library(racafeCore)
library(racafeBD)
Loadpkg(c("tidyverse", "readxl", "openxlsx2", "tictoc", "here",
          "connectapi", "rsconnect", "rfm"))

# 2. Diccionarios, Catalogos y Niveles ----
# 3. Funciones ----
.limpiar <- function(df) {
  mutate(df, across(where(is.character),
                    ~racafeCore::LimpiarCadena(., rem_numeros = FALSE, rem_caresp = FALSE, rem_acentos = FALSE)))
}
# 4. Fechas ----
ayer <- Sys.Date() - 1
# 5. Datos ---
## Auxiliares ----

# Geografia.
dane <- CargarDatos("ANDIVIPOLA")

# Sucursales.
sucs <- data.frame(SucCod = c(12, 15, 20, 26, 30, 32, 35, 50, 55),
                   Sucursal = c("Trilladora 12","Bachué", "Medellín", "Popayán", "Armenia", 
                                "Arenales", "Pereira", "Bucaramanga", "Huila")) %>% 
  mutate(across(where(is.character), LimpiarNombres),
         Sucursal = factor(Sucursal, levels = Sucursal, ordered = TRUE))

SUCS_IN <- "(15, 20, 26, 30, 35, 50, 55, 32)"

# Calidades.
marcas    <- ConsultaSistema("syscafe", 
                             "SELECT MrcCod, MrcNom 
                              FROM NMARCAS")
linprod   <- ConsultaSistema("syscafe", 
                             "SELECT LinProCod, LinProNom 
                              FROM NTIPPROD 
                              WHERE CiaCod = 10")
calidades <- ConsultaSistema("cafesys", 
                             "SELECT TipCaf, CalCod, CalNom 
                              FROM CALTRN")

# Personas.
personas <- ConsultaSistema("syscafe", 
                            "SELECT PerCod, PerTip, PerTipPer, PerRazSoc, CiuCodSt2, PerDir, PerTel, 
                                    PerFax, PaiCodNac, CiiuCod 
                             FROM NPERSONA") %>% 
  mutate(PerTipPer = case_when(PerTipPer == "N" ~ "NATURAL",
                               PerTipPer == "J" ~ "JURÍDICA")) %>% 
  left_join(dane, by = c("CiuCodSt2" = "CodMpio"))

Conductores <- ConsultaSistema("cafesys",
                               "SELECT CndCed, CndRazSoc as Conductor 
                                FROM CNDTRN")


# Precio de carga.
PrecioCarga <- ConsultaSistema("syscafe", 
                               "SELECT h2.HTFec,
                                      SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE h2.HTKilCom END) as KilTo,
                                      SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 
                                               ELSE ((h2.HTPreCom * (f.FactRen/125.0) - (f.FactRen * ((h.HTPreCon * f.PorCon/100.0) + 
                                                     (h.HTPrePas * f.PorPas/100.0) + (h.HTPreRip * f.PorRip/100.0))) +
                                                     (f.FactBas * ((h.HTPreCon * f.PorCon/100.0) + (h.HTPrePas * f.PorPas/100.0) + 
                                                     (h.HTPreRip * f.PorRip/100.0)))) * (125.0/f.FactBas) * h2.HTKilCom)
                                                     END) as PreTo
                                FROM EXPHOTR2 h2
                                LEFT JOIN EXPFACON f ON h2.FactSec = f.FactSec
                                LEFT JOIN EXPHOTRA h ON h2.HTFec = h.HTFec
                                GROUP BY h2.HTFec
                                HAVING SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE h2.HTKilCom END) > 0") %>% 
  group_by(Fecha = as.Date(HTFec)) %>% 
  summarise(PrecioCarga = mean(PreTo / KilTo, na.rm = TRUE), .groups = "drop") %>% 
  arrange(Fecha)

## Ofertas ----
Anticipos <- ConsultaSistema("cafesys", "SELECT SucCod, OfeNro, OfeAntFch, OfeAntVal as AntAutorizado, OfeAntPag as AntGirado,
                                                OfeAntNNo as AntxGirar, OfeAntRei AntReintegrado, OfeAntFeM as FecModificacion,
                                                OfeAntFeA as FecAnulacion from OANTRN
                                         WHERE  CiaCod = 10 AND 
                                                SucCod in (15, 20, 26, 30, 35, 50, 55, 32)")

Ofertas <- ConsultaSistema("cafesys", paste("SELECT SucCod, OfeNro, OfeFch, PrvNit, AsoCed, TipCaf, CalCod, OfeFchVto,
                                                    OfeNroIni, OfeTotKls, OfeTotKls * OfePCaAco/125 as ValorOfe,
                                                    OfeTrlKls KilosTrasladados, OfeRempNro NumOfeRemp, OfeObsv,
                                                    OfeAntAtr as AnticipoAutorizado, OfeAntTSa SaldoAnticipo, OfeAntDsc as AnticipoDescontado,
                                                    OfeRegNro, OfeOriNum, OfeEst, OfeFchEst, OfeFut, OfeFutfch,
                                                    OfePCaDiaA as PrAutorizado, OfePCaAcoA as PrNegociado,
                                                    OfeNegValA as ValorNegociadosAGC, OfeNegVal as ValorNegociados, OfeAntDsc as AnticipoDesc
                                             FROM ofetrn
                                             WHERE CiaCod = 10 AND 
                                                   SucCod in (15, 20, 26, 30, 35, 50, 55, 32) AND 
                                                   OfeFch >= '2016-01-01'")) %>% 
  left_join(Anticipos %>% 
              group_by(SucCod, OfeNro) %>% 
              summarise(AnticiposGirados = sum(AntGirado), 
                        .groups = "drop"),
            by = c("SucCod", "OfeNro")) %>% 
  left_join(calidades, by = c("TipCaf", "CalCod"))  %>% 
  left_join(sucs, by = "SucCod") %>% 
  mutate(IdClienteInicial = ifelse(AsoCed == 0, PrvNit, AsoCed),
         Cambio    = ifelse(OfeNroIni != 0, FALSE, TRUE),
         OfeNroIni = ifelse(OfeNroIni == 0, OfeNro, OfeNroIni),
         OfeEst    = recode(OfeEst, "AN" = "Anulada",    "CA" = "Cancelada", "CR" = "Cruzada",
                                    "CU" = "Cumplida",   "JU" = "Cobro Jurídico",
                                    "PE" = "Pendiente",  "TR" = "Trasladada")) %>% 
  left_join(personas, by = c("IdClienteInicial" = "PerCod")) 

Traslados <- ConsultaSistema("cafesys", 
                             "SELECT SucCod, OfeNro, TOfFch, TOfKls 
                              FROM TOFTRN 
                              WHERE CiaCod = 10 AND 
                                    SucCod in (15, 20, 26, 30, 35, 50, 55, 32) AND 
                                    TOfFch >= '2016-01-01'")

Facturas <- ConsultaSistema("cafesys", "SELECT SucCod, FCoNro, RegNro, TNeCod, OfeNro, FCoKls1 AS kilos, FCoPKiNeg as PrKlNegociado, 
                                               FCoPKiAutA as PrKlAutorizado, FCoVal as ValorFacturado, FCoAntDs1 as DescuentoAnt, 
                                               FcoOfeNro, FcoOfeOri, FcoFltVlr, FcoAjsHst, FCoTipCaf as TipCaf, FCoCalCod as CalCod 
                                        FROM FCOTRN1 
                                        WHERE CiaCod = 10 AND 
                                              SucCod in (15, 20, 26, 30, 35, 50, 55, 32)")  %>% 
  inner_join(ConsultaSistema("cafesys", "SELECT SucCod, FCoNro, FCoFch, FCoNet as ValorNeto, FCoValPag as ValorPagado, FCoPrvNit, FCoAsoCed 
                                         FROM FCOTRN 
                                         WHERE CiaCod = 10 AND 
                                               SucCod in (15, 20, 26, 30, 35, 50, 55, 32) AND 
                                               FCoFch >= '2016-01-01' and FCoEst = 'AC'"), 
             by = c("SucCod", "FCoNro")) %>% 
  left_join(sucs, by = "SucCod") %>% 
  left_join(calidades, by = c("TipCaf", "CalCod")) %>% 
  mutate(IdClienteInicial = ifelse(FCoAsoCed == 0, FCoPrvNit, FCoAsoCed),
         FacturaFlete     = ifelse(kilos == 0 & FcoFltVlr > 0, TRUE, FALSE)) %>% 
  left_join(personas, by = c("IdClienteInicial" = "PerCod"))

# Entradas ----
Entradas <- ConsultaSistema("cafesys", paste(
  "select SucCod, RegNro, RegFchEnt, PrvNit, AsoCed, TipCaf, CalCod,
   EvaPorHum, EvaGrsMue, EvaGrsAlm, EvaGrsMMa, EvaGrsPas, EvaGrsRip,
   CndCed as Conductor, VehNroPla, RegKlsBru, RegTarEmp, RegTarBru,
   (RegKlsBru - RegTarEmp - RegTarBru) as KilosNetos,
   RegBul, EvaPorFer, EvaPorMan, RegCiuC, RegVerCod,
   RBscTiqNro as Tiqbascula,
   CSNKls, CSNKlsNeg, CSNAntGir, CSNAntDsc, CSNAntAtr, CSNAntUlt, CSNAntTSa
   from REGTRN
   where CiaCod = 10 and SucCod in", SUCS_IN, "and RegFchEnt > '2016-01-01'")) %>%
  left_join(sucs, by = "SucCod") %>%
  mutate(
    IdClienteInicial  = ifelse(AsoCed == 0, PrvNit, AsoCed),
    FactorRendimiento = round(70 / (EvaGrsMMa / EvaGrsMue), 2),
    PctAlmendra = EvaGrsAlm / EvaGrsMue,
    PctExcelso  = EvaGrsMMa / EvaGrsMue,
    PctPasilla  = EvaGrsPas / EvaGrsMue,
    PctRipio    = EvaGrsRip / EvaGrsMue,
    PctMerma    = 1 - (PctExcelso + PctPasilla + PctRipio)) %>%
  left_join(calidades, by = c("TipCaf", "CalCod")) %>%
  left_join(personas, by = c("IdClienteInicial" = "PerCod")) %>%
  left_join(dane %>% rename(NomDepPro = NomDep, MunPro = Mun, LatPro = lat, LngPro = lng) %>% mutate(CodMun = as.numeric(CodMun)),
            by = c("RegCiuC" = "CodMun")) %>%
  .limpiar()

# Liquidacion ----
## Helper: resolver cadena de ofertas iniciales (hasta 4 niveles)
.resolver_oferta_inicial <- function(df) {
  base <- df %>% select(SucCod, OfeNro, OfertaInicial)
  for (i in 1:4) {
    base <- base %>%
      left_join(df %>% select(SucCod, OfeNro, next_ini = OfertaInicial),
                by = c("SucCod", "OfertaInicial" = "OfeNro")) %>%
      mutate(OfertaInicial = ifelse(is.na(next_ini), OfertaInicial, next_ini)) %>%
      select(-next_ini)
  }
  base
}

aux1 <- Ofertas %>%
  mutate(NumOfeRemp = ifelse(NumOfeRemp %in% c(NA, 0), OfeOriNum, NumOfeRemp)) %>%
  select(SucCod, OfeNro, OfertaInicial = NumOfeRemp) %>%
  distinct() %>%
  filter(SucCod == 20)

OfertaInicial <- .resolver_oferta_inicial(aux1)

Iniciales <- Ofertas %>%
  select(SucCod, NumOfeRemp, OfeOriNum, OfeNro) %>%
  left_join(OfertaInicial, by = c("SucCod", "OfeNro")) %>%
  mutate(OfertaInicial = ifelse(OfertaInicial %in% c(NA, 0), NumOfeRemp, OfertaInicial),
         OfertaInicial = ifelse(OfertaInicial %in% c(NA, 0), OfeOriNum, OfertaInicial)) %>%
  select(SucCod, OfertaInicial) %>%
  distinct() %>%
  left_join(Ofertas %>% select(SucCod, OfeNro, OfeFch, OfeTotKls, IdClienteInicial,
                              PerRazSoc, TipCaf, CalCod, CalNom, OfeFut, OfeFutfch),
            by = c("SucCod", "OfertaInicial" = "OfeNro")) %>%
  left_join(
    Anticipos %>%
      group_by(SucCod, OfeNro, OfeAntFch) %>%
      summarise(across(c(AntAutorizado, AntGirado, AntxGirar, AntReintegrado), ~sum(., na.rm = TRUE)),
                .groups = "drop"),
    by = c("SucCod", "OfertaInicial" = "OfeNro", "OfeFch" = "OfeAntFch")) %>%
  replace_na(list(AntAutorizado = 0, AntGirado = 0, AntxGirar = 0, AntReintegrado = 0))

aux_ofe_fact <- bind_rows(
  Facturas %>% mutate(Tipo = "Factura") %>%
    group_by(SucCod, OfeNro, Fecha = FCoFch, Tipo) %>%
    summarise(kilos = sum(kilos), DescuentoAnt = sum(DescuentoAnt), .groups = "drop"),
  Traslados %>% mutate(Tipo = "Traslado") %>%
    group_by(SucCod, OfeNro, Fecha = TOfFch, Tipo) %>%
    summarise(kilos = sum(TOfKls), .groups = "drop"))

Liquidacion <- Ofertas %>%
  select(SucCod, NumOfeRemp, OfeOriNum, OfeNroIni, OfeNro, FecOferta = OfeFch,
         KilosOferta = OfeTotKls, ClienteOferta = IdClienteInicial, PerRazSoc,
         CalNomOferta = CalNom, OfeEst, OfeFut, OfeFutfch) %>%
  mutate(NumOfeRemp = ifelse(NumOfeRemp %in% c(NA, 0), OfeOriNum, NumOfeRemp)) %>%
  left_join(OfertaInicial, by = c("SucCod", "OfeNro")) %>%
  mutate(OfertaInicial = ifelse(is.na(OfertaInicial), NumOfeRemp, OfertaInicial)) %>%
  relocate(OfertaInicial, .after = SucCod) %>%
  left_join(Iniciales %>% select(SucCod, OfertaInicial, FecOriginal = OfeFch,
                                KilosOriginal = OfeTotKls, ClienteOriginal = IdClienteInicial,
                                NomClienteInicial = PerRazSoc, CalNomOriginal = CalNom, AntGirOriginal = AntGirado),
            by = c("SucCod", "OfertaInicial")) %>%
  left_join(aux_ofe_fact, by = c("SucCod", "OfeNro")) %>%
  full_join(Anticipos %>% select(SucCod, OfeNro, Fecha = OfeAntFch, AntGirado),
            by = c("SucCod", "OfeNro", "Fecha")) %>%
  arrange(SucCod, OfeNro, Fecha) %>%
  replace_na(list(kilos = 0, DescuentoAnt = 0, Tipo = "Anticipo", AntGirado = 0)) %>%
  group_by(SucCod, OfeNro) %>%
  fill(OfertaInicial, NumOfeRemp, OfeOriNum, OfeNroIni, FecOferta, KilosOferta,
       ClienteOferta, PerRazSoc, CalNomOferta, OfeEst, FecOriginal, KilosOriginal,
       ClienteOriginal, NomClienteInicial, CalNomOriginal, AntGirOriginal,
       OfeFut, OfeFutfch, .direction = "downup") %>%
  mutate(across(where(is.numeric), as.numeric),
         SaldoKilosOfe       = KilosOferta - cumsum(kilos),
         SaldoKilosPctOfe    = SiError_0(SaldoKilosOfe / KilosOferta),
         SaldoAnticipoOfe    = cumsum(AntGirado) - cumsum(DescuentoAnt),
         SaldoPctAnticipoOfe = SiError_0(SaldoAnticipoOfe / cumsum(AntGirado))) %>%
  left_join(sucs, by = "SucCod")

# RFM Facturacion ----
aux0 <- Facturas %>%
  select(PerRazSoc, Sucursal) %>%
  distinct() %>%
  group_by(customer_id = PerRazSoc) %>%
  summarise(Sucursales = paste(Sucursal, collapse = "|"), .groups = "drop")

aux1 <- Facturas %>%
  group_by(PerRazSoc, FCoFch = as.Date(FCoFch)) %>%
  summarise(Kilos = sum(kilos), .groups = "drop") %>%
  rename(customer_id = PerRazSoc, order_date = FCoFch, revenue = Kilos)

rfm_result <- rfm_table_order(aux1, customer_id, order_date, revenue,
                              analysis_date = max(aux1$order_date))

ResultadosRFM <- rfm_result$rfm %>%
  left_join(aux0, by = "customer_id") %>%
  left_join(
    Liquidacion %>%
      ungroup() %>%
      filter(!is.na(Fecha)) %>%
      select(OfeNro, PerRazSoc, OfeFut, OfeFutfch, FecOferta, Fecha) %>%
      mutate(
        FechaCumplimiento = if_else(OfeFut == 2, OfeFutfch, FecOferta),
        FechaCumplimiento = if_else(FechaCumplimiento == as.Date("1753-01-01"), FecOferta, FechaCumplimiento),
        TOB = pmax(0, as.numeric(difftime(as.Date(Fecha), as.Date(FechaCumplimiento), units = "days")))) %>%
      group_by(PerRazSoc, OfeNro) %>%
      summarise(Morosas = sum(ifelse(max(TOB) > 10, 1, 0)), .groups = "drop") %>%
      group_by(customer_id = PerRazSoc) %>%
      summarise(ScoreCumplimiento = (1 - (sum(Morosas) / n_distinct(OfeNro))) * 1000, .groups = "drop"),
    by = "customer_id") %>%
  mutate(
    Freq  = rescale(transaction_count, to = c(0, 1000)),
    Rece  = rescale(recency_days,      to = c(1000, 0)),
    Mont  = rescale(amount,            to = c(0, 1000)),
    # Scores simulados por quintil de recencia/frecuencia
    ScoreFuga = case_when(
      recency_score == 1 ~ runif(n(), 0,   350),
      recency_score == 2 ~ runif(n(), 350, 500),
      recency_score == 3 ~ runif(n(), 500, 650),
      recency_score == 4 ~ runif(n(), 650, 800),
      recency_score == 5 ~ runif(n(), 800, 1000)),
    ScoreSobrevida = case_when(
      recency_score == 1 ~ runif(n(), 0,   350),
      recency_score == 2 ~ runif(n(), 350, 500),
      recency_score == 3 ~ runif(n(), 500, 650),
      recency_score == 4 ~ runif(n(), 650, 800),
      recency_score == 5 ~ runif(n(), 800, 1000)),
    ScoreRecompra = case_when(
      frequency_score == 1 ~ runif(n(), 0,   350),
      frequency_score == 2 ~ runif(n(), 350, 500),
      frequency_score == 3 ~ runif(n(), 500, 650),
      frequency_score == 4 ~ runif(n(), 650, 800),
      frequency_score == 5 ~ runif(n(), 800, 1000)))

# 6. Exportacion ----
gdata::keep(Ofertas, Facturas, Entradas, Liquidacion, ResultadosRFM, sure = TRUE)
save.image("APP/data/data.RData")

# # 7. Publicacion en Posit Connect ----

# client <- connect(server  = "http://172.16.19.39:3939",
#                   api_key = "HayDGkCmpQqmZB1rSkj2300JMDNpA2el")
# 
# if (!file.exists("APP/manifest.json")) {
#   rsconnect::writeManifest("/home/compartido/APP/5_Trilladoras/CRMClienteInicial/APP/")
# }
# 
# bundle  <- bundle_dir("/home/compartido/APP/5_Trilladoras/CRMClienteInicial/APP/")
# content <- client %>%
#   deploy(bundle) %>%
#   poll_task()
# 
# rm(bundle, client, content)
# gc()
# tictoc::toc()