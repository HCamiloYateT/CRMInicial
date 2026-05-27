tictoc::tic("Total")
print(paste0("***********",Sys.time(), "***********"))
setwd("/home/htamara/5_Trilladoras/CRMClienteInicial/")

source("https://raw.githubusercontent.com/AnaliticaRacafe/ShinyApps/main/Funciones.r")
pkgs <- c("tidyverse", "DBI", "lubridate")
Loadpkg(pkgs)

CargarDatos <- function(tabla){
  
  con <- dbConnect(RMySQL::MySQL(), dbname= "Analitica", host = "localhost",
                   port=3306, user='datos', password='R4c4f3*1', DBMSencoding="UTF-8")
  
  dbGetQuery(con, "SET NAMES 'utf8'") 
  aux1 <- dbGetQuery(con, paste("select *  from", tabla))
  dbDisconnect(con)
  return(aux1)
  
}

# Conexiones a bases de datos -----
conn_cafe <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 18 for SQL Server",
                       Server = "172.16.19.21",
                       Database = "ContabRacafe",
                       uid = "AppQlikRcaLee",
                       pwd = "Rac@CafQlikS2022*",
                       port = 1433,
                       TrustServerCertificate="yes")

conn_sys <- dbConnect(odbc::odbc(),
                      Driver = "ODBC Driver 18 for SQL Server",
                      Server = "172.16.19.21",
                      Database = "Cafesys",
                      uid = "AppQlikRcaLee",
                      pwd = "Rac@CafQlikS2022*",
                      port = 1433,
                      TrustServerCertificate="yes")

# Cargue de datos ----
## Auxiliares ----

dane <- CargarDatos("ANDIVIPOLA") %>% select(CodMpio, NomDep, Mun:lng)

sucs <- dbGetQuery(conn_sys, "select distinct SucCod from NSUCURSA where CiaCod = 10
              and SucCod in (15, 20, 26, 30, 35, 50, 55)") %>%
  mutate(Sucursal = case_when(SucCod == 15 ~ "BACHUÉ",
                              SucCod == 20 ~ "MEDELLÍN",
                              SucCod == 26 ~ "POPAYÁN",
                              SucCod == 30 ~ "ARMENIA",
                              SucCod == 35 ~ "PEREIRA",
                              SucCod == 50 ~ "BUCARAMANGA",
                              SucCod == 55 ~ "HUILA")) %>%
  mutate(Sucursal = factor(Sucursal, ordered = T, 
                           levels = c('BACHUÉ','MEDELLÍN','POPAYÁN', 'ARMENIA',
                                      'PEREIRA','BUCARAMANGA','HUILA'))) 

marcas <- dbGetQuery(conn_cafe, "select MrcCod, MrcNom from NMARCAS") %>%
  mutate_if(is.character, list(~str_to_upper(trimws(.,))))

linprod <- dbGetQuery(conn_cafe, "select LinProCod, LinProNom from NTIPPROD where CiaCod = 10") %>%
  mutate_if(is.character, list(~str_to_upper(trimws(.,))))

calidades <- dbGetQuery(conn_sys, "select  TipCaf, CalCod, CalNom from caltrn")  %>%
  mutate_if(is.character, list(~str_to_upper(trimws(.,))))

personas <- dbGetQuery(conn_cafe, "select PerCod, PerTip, PerTipPer, PerRazSoc,
                     CiuCodSt2, PerDir, PerTel, PerFax, PaiCodNac, CiiuCod
                     FROM NPERSONA") %>%
  mutate(PerTipPer = case_when(PerTipPer == "N" ~ "Natural",
                               PerTipPer == "J" ~ "JurÃ­dica")) %>%
  left_join(dane, by=c("CiuCodSt2"="CodMpio"))  %>%
  mutate_if(is.character, list(~str_to_upper(trimws(.,))))

Conductores <- dbGetQuery(conn_sys, "select CndCed,CndRazSoc as Conductor from CNDTRN ")  %>%
  mutate_if(is.character, list(~str_to_upper(trimws(.,))))

EXPHOTR<-DBI::dbGetQuery(conn_cafe, "select HTFec, HTPreRip, HTPrePas, HTPreCon FROM EXPHOTRA")
EXPFACON <- DBI::dbGetQuery(conn_cafe, "select FactSec, FactRen, PorCon, PorRip, PorPas, FactBas FROM EXPFACON")
EXPHOTR2<-DBI::dbGetQuery(conn_cafe, "select HTFec, HTSecCom, HTSucRef, HTKilCom, HTPreCom, HTComEsp,FactSec FROM EXPHOTR2")

PrecioCarga <- EXPHOTR2 %>% 
  left_join(EXPFACON, by=c("FactSec")) %>% 
  left_join(EXPHOTR, by = c("HTFec")) %>% 
  mutate(PrdCon = HTPreCon*PorCon/100,
         PrdPas = HTPrePas*PorPas/100,
         PrdRip = HTPreRip*PorRip/100,
         FacRecup = PrdCon+PrdPas +PrdRip,
         RecReg = FactRen*FacRecup,
         RecSac = FacRecup*FactBas,
         PrecNormal = (HTPreCom*(FactRen/125)-RecReg+RecSac)*(125/FactBas),
         KilNor = ifelse(HTComEsp =="S", 0, HTKilCom),
         PreComNor = PrecNormal*KilNor) %>% 
  group_by(HTFec) %>% 
  summarise(KilTo = sum(KilNor, na.rm = T),
            PreTo = sum(PreComNor, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(PreCrgHT = PreTo/KilTo) %>% 
  select(RegFchEnt = HTFec, PrecioCarga = PreCrgHT) %>% 
  mutate(RegFchEnt = as.Date(RegFchEnt))


## Ofertas ----

OFEPEN <- dbGetQuery(conn_cafe, paste0("select SucCod, OPenANum, OPenAPrv, OPenAAso, OPenFchPro, OPenAFch, OPenASldK,
                                       OPenAAntV, OpenATip, OPenACal from OFEPEN where ciacod = 10 and SucCod <> 12 
                                       and OPenFchPro >='", format(Sys.Date()-1, "%Y-%d-%m"), "'")) %>% 
  left_join(sucs, by = join_by(SucCod)) %>% 
  left_join(personas %>% select(PerCod, RazonSocial = PerRazSoc), by =c("OPenAPrv"="PerCod")) %>%
  left_join(personas %>% select(PerCod, Asociado = PerRazSoc), by =c("OPenAAso"="PerCod")) %>%
  left_join(calidades, by=c("OpenATip"="TipCaf", "OPenACal"="CalCod")) %>%
  mutate(Dias = as.numeric((as.Date(OPenFchPro)- as.Date(OPenAFch)))+2,
         RangoDias = str_to_upper(case_when(Dias <=7 ~ "De 0 a 7 días",
                                            Dias <=15 ~ "De 8 a 15 días",
                                            Dias <=30 ~ "De 16 a 30 días",
                                            Dias <=60 ~ "De 31 a 60 días",
                                            T ~ "Más de 60 días"
                                            ))
         ) 
## Entradas -----

Entradas <- dbGetQuery(conn_sys, 
                      "select SucCod, RegNro, RBscTiqNro, RegFchEnt, RegHorEnt, RegUsuRes,
                      TipCaf, CalCod, RegPusCsc, PrvNit, AsoCed, RegDueCed, RegCiuC, RegVerCod,
                      CndCed, VehNroPla, RegKlsBru, RegTarBru, RegTarEmp,RegKlsRec, 
                      RegKlsNeg, RegBul, RegPBDApl, RegAjtKls, RegPCaDia, RegPKiDia,
                      EvaPorHum, EvaGrsMue, EvaGrsAlm, EvaGrsMMa, EvaGrsCon, EvaGrsPas, 
                      EvaGrsRip, EvaGrsMue-EvaGrsAlm as GrsMerma 
                      FROM REGTRN 
                      WHERE CiaCod = 10 and SucCod in (15, 20, 26, 30, 35, 50, 55) and 
                      RegFchEnt >='2016-01-01' and CEsCod='AC'") %>%
  left_join(sucs, by = "SucCod") %>% 
  left_join(dbGetQuery(conn_sys, "select PusCsc, PusUso from BSCUSO where ciacod=10"), by = c("RegPusCsc"="PusCsc")) %>%
  left_join(personas %>% select(PerCod, RazonSocial = PerRazSoc), by =c("PrvNit"="PerCod")) %>%
  left_join(personas %>% select(PerCod, Asociado = PerRazSoc), by =c("AsoCed"="PerCod")) %>%
  left_join(Conductores, by =c("CndCed"="CndCed")) %>%
  left_join(PrecioCarga, by = "RegFchEnt") %>% 
  mutate(KilosBrutos = RegKlsBru,
         KilosTara = RegTarBru,
         KilosTaraEmp =RegTarEmp,
         KilosImpurezas = RegKlsRec,
         KilosNetos = RegKlsBru-RegTarBru-RegTarEmp-RegKlsRec,
         KilosBonifDesc = RegAjtKls,
         PctBinfDesc = KilosBonifDesc/KilosNetos,
         KilosNegoc = KilosNetos + KilosBonifDesc,
         FactorUso = PusUso,
         FactorRendimiento = round(70/(EvaGrsMMa / EvaGrsMue), 2),
         PctAlmendra = EvaGrsAlm/EvaGrsMue,
         PctExcelso = EvaGrsMMa/EvaGrsMue,
         PctConsumo = EvaGrsCon/EvaGrsMue,
         PctPasilla = EvaGrsPas/EvaGrsMue,
         PctRipio = EvaGrsRip/EvaGrsMue,
         PctMerma = GrsMerma / EvaGrsMue,
         Proveedor = ifelse(AsoCed == 0, RazonSocial, Asociado)) %>%
  left_join(calidades, by=c("TipCaf", "CalCod")) %>%
  left_join(dane %>% select(CodMpio, Departamento=NomDep, Municipio = Mun, lat,lng), by=c("RegCiuC"="CodMpio")) %>%
  select(Sucursal, SucCod:RegUsuRes, TipCaf:CalCod, CalNom, Proveedor, PrvNit, RazonSocial, AsoCed, 
         Asociado, CndCed, Conductor, VehNroPla, RegCiuC, 
         Departamento, Municipio, lat, lng, KilosBrutos: PctMerma, EvaPorHum, PrecioCarga) %>% 
  mutate_if(is.character, LimpiarNombres)


## Facturas -----
## Cortes ----
## liquidacion ----

# Exportacion APP ----
gdata::keep(OFEPEN, Entradas, sucs, sure = T)
save.image("APP/data/data.RData")
tictoc::toc()
