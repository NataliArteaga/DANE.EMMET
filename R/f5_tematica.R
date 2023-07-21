#' Tematica
#'
#' @description
#'  Esta funcion construye la base tematica. Esta base expone los datos
#'  procesados, de acuerdo a la metodología de la operación, en ese sentido
#'  presenta los datos reales a partir de los nominales, sumado a ellos presenta
#'  la información ponderada y agrega variables de la identificacion de los
#'  dominios por los cuales se publca. Con esto, se puede presentar la base
#'  tematica como la base final de cuadros.
#'  Para construir esta base, se usa como insumo la base Panel con los casos
#'  de imputacion aplicados. Finalmente exporta un archivo de extension .xlsx .
#'
#'
#'
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#' @return CSV file
#' @export
#'
#' @examples tematica(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)
#'
#'
#' @details
#'  Esta funcion crea las variables de la base tematica de la siguiente manera:
#'
#'  VentasReales: Suma de, la división entre ventas en el interior e IPP_PYC y,
#'     la división entre ventas en el exterior e IPP_PYC
#'
#'  TotalPersonas: Suma de las variables que contabilizan el número de
#'     trabajadores en las diferentes categorías.
#'
#'  TotalSueldosNominal: Suma de las variables de sueldos nominales
#'
#'  TotalSueldosReales: División entre TotalSueldosNominal e IPC
#'
#'  SueldosPermanentesNominal: Suma de las variables de sueldos nominales del
#'     personal contratado por la categoria permanente
#'
#'  SueldosPermanentesReales: División entre SueldosPermanentesNominal e IPC
#'
#'  SueldosTemporalesNominal: Suma de las variables de sueldos nominales del
#'     personal contratado por la categoria temporales
#'
#'  SueldosTemporalesReales: División entre SueldosTemporalesNominal e IPC
#'
#'  SueldosAdmonNominal: Suma de las variables de sueldo nominal del
#'     personal administrativo nominales
#'
#'  SueldosAdmonReal: División entre SueldosAdmonNominal e IPC
#'
#'  SueldosAdmonPermReal: División entre AJU_II_PA_PP_SUELD_EP e IPC
#'
#'  SueldosAdmonTempNomin: Suma de las variables de sueldo del
#'     personal administrativos temporales
#'
#'  SueldosAdmonTempReal: División entre SueldosAdmonTempNomin e IPC
#'
#'  SueldosProducNominal: Suma de las variables del personal de
#'     producción nominal
#'
#'  SueldosProducReal: División entre SueldosProducNominal e IPC
#'
#'  SueldosProducPermReal: División entre AJU_II_PP_PP_SUELD_OP e IPC.
#'
#'  SueldosProducTempNomin: Suma de las variables del personal de
#'     producción temporal
#'
#'  SueldosProducTempReal: División entre SueldosProducTempNomin e IPC.
#'
#'  TotalHoras: Suma de varaibles de horas extras y ordinarias
#'
#'  TotalEmpleoPermanente: Suma de varaibles del personal de
#'     administración y producción
#'
#'  TotalEmpleoTemporal: Suma de varaibles del personal temporal
#'
#'  TotalEmpleoAdmon: Suma de varaibles del personal administrativo
#'
#'  TotalEmpleoProduc: Suma de varaibles del personal de producción
#'
#'  EmpleoProducTempor: Suma de varaibles del personal de producción
#'     temporal
#'
#'  TOTAL_VENTAS: Suma de varaibles en el interior y exterior de país
#'
#'  DEFLACTOR_NAL: División entre TOTAL_VENTAS y VentasReales
#'



tematica <- function(directorio,mes,anio){
  # Cargar librerC-as --------------------------------------------------------
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(kableExtra)
  library(lubridate)
  library(formattable)
  library("htmltools")
  library("webshot")
  library(openxlsx)
  library(seasonal)
  library(stringr)

  source("utils.R")


  # Cargar bases y variables ------------------------------------------------

  base_panel2<-read.csv(paste0(directorio,"/results/S4_imputacion/EMMET_PANEL_imputada_",meses[mes],anio,".csv"))

  base_panel2<-base_panel2[1:(dim(base_panel)[1]-2),]

  meses <- tolower(meses)


  # Modificar tipo de variables ---------------------------------------------



  base_panel2 <- as.data.frame(base_panel2)
  base_panel2$MES=as.numeric(base_panel2$MES)
  base_panel2$ANIO=as.numeric(base_panel2$ANIO)
  variablesinte=c("II_PA_PP_NPERS_EP","AJU_II_PA_PP_SUELD_EP","II_PA_TD_NPERS_ET",
                  "AJU_II_PA_TD_SUELD_ET","II_PA_TI_NPERS_ETA","AJU_II_PA_TI_SUELD_ETA","II_PA_AP_AAEP","AJU_II_PA_AP_AAS_AP",
                  "II_PP_PP_NPERS_OP","AJU_II_PP_PP_SUELD_OP","II_PP_TD_NPERS_OT","AJU_II_PP_TD_SUELD_OT","II_PP_TI_NPERS_OTA",
                  "AJU_II_PP_TI_SUELD_OTA","II_PP_AP_APEP","AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T",
                  "AJU_III_PE_PRODUCCION","AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","III_EX_VEXIS")

  for (i in variablesinte) {

    base_panel2[,i] <- as.numeric(base_panel2[,i])
    base_panel2[,i] <- ifelse(is.na(base_panel2[,i]),0,base_panel2[,i])
  }



  base_panel2$II_PA_PP_NPERS_EP<-as.numeric(base_panel2$II_PA_PP_NPERS_EP)
  base_panel2$AJU_III_PE_VENTASEX<-as.numeric(base_panel2$AJU_III_PE_VENTASEX)
  base_panel2$AJU_II_HORAS_HEXTR_T<-as.numeric(base_panel2$AJU_II_HORAS_HEXTR_T)
  base_panel2$PONDERADOR<-gsub(",",".",base_panel2$PONDERADOR)
  base_panel2$PONDERADOR<-as.numeric(base_panel2$PONDERADOR)




  # Funcion -----------------------------------------------------------------


  #Creacion de todas las variables
  base_tematica<-function(base){
    base_tematica<-base %>%
      select(ANIO,MES,NOVEDAD,ID_ESTADO,ID_NUMORD,NOMBREDPTO,II_PA_PP_NPERS_EP,
             AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,AJU_II_PA_TD_SUELD_ET,
             II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,
             AJU_II_PA_AP_AAS_AP,II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,
             II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
             AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,
             AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,TOTAL_HORAS,
             AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,
             TOTAL_VENTAS,NOMBRE_ESTAB,ID_MUNICIPIO,ORDEN_AREA,
             AREA_METROPOLITANA,ORDEN_CIUDAD,CIUDAD,ORDEN_DEPTO,
             INCLUSION_NOMBRE_DEPTO,CLASE_CIIU4,DIVISION,EMMET_CLASE,
             ORDENDOMINDEPTO,AGREG_DOMINIO_REG,DOMINIOEMMET43,DOMINIOEMMET39,
             DESCRIPCIONDOMINIOEMMET39,DOM_PONDERADOR_CIUDADES,PONDERADOR,
             DEFLACTOR,IPP_PYC,IPP_EXP,IPC,CODIGODEPARTAMENTO,NOMBREDEPARTAMENTO) %>%
      mutate(VentasReales=(AJU_III_PE_VENTASIN/IPP_PYC)+(AJU_III_PE_VENTASEX/IPP_EXP),
             II_TOT_TOT_PERS=(II_PA_PP_NPERS_EP+II_PA_TD_NPERS_ET+II_PA_TI_NPERS_ETA+
                                II_PA_AP_AAEP+II_PP_PP_NPERS_OP+II_PP_TD_NPERS_OT+
                                II_PP_TI_NPERS_OTA+II_PP_AP_APEP),
             TotalSueldosNominal=(AJU_II_PA_PP_SUELD_EP+AJU_II_PA_TD_SUELD_ET+
                                    AJU_II_PA_TI_SUELD_ETA+AJU_II_PA_AP_AAS_AP+
                                    AJU_II_PP_PP_SUELD_OP+AJU_II_PP_TD_SUELD_OT+
                                    AJU_II_PP_TI_SUELD_OTA+AJU_II_PP_AP_AAS_PP),
             TotalSueldosReales= (TotalSueldosNominal/IPC),
             SueldosPermanentesNominal=(AJU_II_PA_PP_SUELD_EP+AJU_II_PP_PP_SUELD_OP),
             SueldosPermanentesReales= (SueldosPermanentesNominal/IPC),
             SueldosTemporalesNominal= (AJU_II_PA_TD_SUELD_ET+AJU_II_PA_TI_SUELD_ETA+
                                          AJU_II_PA_AP_AAS_AP+AJU_II_PP_TD_SUELD_OT+
                                          AJU_II_PP_TI_SUELD_OTA+AJU_II_PP_AP_AAS_PP),
             SueldosTemporalesReales= (SueldosTemporalesNominal/IPC),
             SueldosAdmonNominal= (AJU_II_PA_PP_SUELD_EP+AJU_II_PA_TD_SUELD_ET+
                                     AJU_II_PA_TI_SUELD_ETA+AJU_II_PA_AP_AAS_AP),
             SueldosAdmonReal= (SueldosAdmonNominal/IPC),
             SueldosAdmonPermReal= (AJU_II_PA_PP_SUELD_EP/IPC),
             SueldosAdmonTempNomin= (AJU_II_PA_TD_SUELD_ET+AJU_II_PA_TI_SUELD_ETA+
                                       AJU_II_PA_AP_AAS_AP),
             SueldosAdmonTempReal= (SueldosAdmonTempNomin/IPC),
             SueldosProducNominal=(AJU_II_PP_PP_SUELD_OP+AJU_II_PP_TD_SUELD_OT+
                                     AJU_II_PP_TI_SUELD_OTA+AJU_II_PP_AP_AAS_PP),
             SueldosProducReal = (SueldosProducNominal/IPC),
             SueldosProducPermReal= (AJU_II_PP_PP_SUELD_OP/IPC),
             SueldosProducTempNomin=(AJU_II_PP_TD_SUELD_OT+AJU_II_PP_TI_SUELD_OTA+
                                       AJU_II_PP_AP_AAS_PP),
             SueldosProducTempReal = (SueldosProducTempNomin/IPC),
             TotalHoras= (AJU_II_HORAS_HORDI_T+AJU_II_HORAS_HEXTR_T),
             TotalEmpleoPermanente= (II_PA_PP_NPERS_EP+II_PP_PP_NPERS_OP),
             TotalEmpleoTemporal= (II_PA_TD_NPERS_ET+II_PA_TI_NPERS_ETA+II_PA_AP_AAEP+
                                     II_PP_TD_NPERS_OT+II_PP_TI_NPERS_OTA+II_PP_AP_APEP),
             TotalEmpleoAdmon= (II_PA_PP_NPERS_EP+II_PA_TD_NPERS_ET+II_PA_TI_NPERS_ETA+
                                  II_PA_AP_AAEP),
             TotalEmpleoProduc= (II_PP_PP_NPERS_OP+II_PP_TD_NPERS_OT+II_PP_TI_NPERS_OTA+
                                   II_PP_AP_APEP),
             EmpleoProducTempor= (II_PP_TD_NPERS_OT+II_PP_TI_NPERS_OTA+II_PP_AP_APEP),
             TOTAL_VENTAS=(AJU_III_PE_VENTASIN+AJU_III_PE_VENTASEX),
             DEFLACTOR_NAL=(TOTAL_VENTAS/VentasReales)) %>%
      rename(Dominio_39=DOMINIOEMMET39,
             Dominio_43=DOMINIOEMMET43)

    base_tematica<- base_tematica %>%
      mutate(deflactor_nal=case_when(
        is.na(DEFLACTOR_NAL)==TRUE ~ IPP_PYC,
        (VentasReales==0 & TOTAL_VENTAS==0) ~ IPP_PYC,
        TRUE ~ DEFLACTOR_NAL
      ))

    base_tematica<-base_tematica %>%
      mutate(ProduccionReal= (AJU_III_PE_PRODUCCION/deflactor_nal),
             ProduccionNomPond= (AJU_III_PE_PRODUCCION*PONDERADOR),
             ProduccionRealPond= (ProduccionReal*PONDERADOR),
             VentasNominPond= ((AJU_III_PE_VENTASIN+AJU_III_PE_VENTASEX)*PONDERADOR),
             VentasRealesPond= (VentasReales*PONDERADOR))
    base_tematica=subset(base_tematica,select=-DEFLACTOR_NAL)
    return(base_tematica)
  }


  base_tematica<-base_tematica(base_panel2)
  write.csv(base_tematica,paste0(directorio,"/results/S5_tematica/EMMET_PANEL_tematica_",mes,anio,".csv"),row.names=F)

}
