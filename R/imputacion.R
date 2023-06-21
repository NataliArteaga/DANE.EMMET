#' Title
#'
#'  @param mes Definir las tres primeras letras del mes a ejecutar, ej: Nov
#'  @param anio Definir el año a ejecutar, ej: 2022
#'  @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#'  @return CSV file
#'  @export
#'
#'  @examples imputacion_outliers(directorio="/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes="nov",anio=2022)
# Función identificación e imputación de outliers ------------------------------------------



imputacion_outliers <- function(directorio,year,mes) {
  library(dplyr)
  #library(tidyr)
  #library(plotly)
  #library(plyr)
  library(readxl)
  library(readr)
  library(data.table)
  library(VIM)
  meses <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
  month <- mes
  year  <- year

  datos <- fread(paste0(directorio,"/results/S2_estandarizacion/EMMET_PANEL_estandarizado",meses[month],year,".csv"))

  #necesita función identifiacion outliers
  datoscom=datos
  variablesinte=c("II_PA_PP_NPERS_EP","AJU_II_PA_PP_SUELD_EP","II_PA_TD_NPERS_ET",
                  "AJU_II_PA_TD_SUELD_ET","II_PA_TI_NPERS_ETA","AJU_II_PA_TI_SUELD_ETA","II_PA_AP_AAEP","AJU_II_PA_AP_AAS_AP",
                  "II_PP_PP_NPERS_OP","AJU_II_PP_PP_SUELD_OP","II_PP_TD_NPERS_OT","AJU_II_PP_TD_SUELD_OT","II_PP_TI_NPERS_OTA",
                  "AJU_II_PP_TI_SUELD_OTA","II_PP_AP_APEP","AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T",
                  "AJU_III_PE_PRODUCCION","AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","III_EX_VEXIS")
  datos=datos %>% select(ANIO,MES,ID_NUMORD,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
                         AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
                         II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
                         AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
                         AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)

  datos<- as.data.frame(datos)
  datos$MES=as.numeric(datos$MES)
  datos$ANIO=as.numeric(datos$ANIO)
  for (i in variablesinte) {
    datos[,i] <- as.numeric(datos[,i])
    datos[,i] <- ifelse(is.na(datos[,i]),0,datos[,i])
  }

  datos <- subset(datos, !(ANIO == year & MES == mes))

  wowimp=fread(paste0(directorio,"/results/S3_identificacion_alertas/EMMET_PANEL_alertas_",meses[month],year,".csv"))
  for (i in variablesinte) {
    wowimp[wowimp[,paste0(i,"_caso de imputacion")]!="continua",i] <- NA
  }
  wowimp=wowimp %>% select(ANIO,MES,ID_NUMORD,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
                           AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
                           II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
                           AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
                           AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)

  base_imputar=rbind(datos,wowimp)


  # imputacion ---------------------------------------------------------------------

  set.seed(11)
  wowKNN <- base_imputar %>%
    group_by(ID_NUMORD) %>%
    group_modify(~ kNN(.x,numFun = mean,k=4))

  wowKNN=wowKNN%>% filter(MES==mes & ANIO==year) %>%
    arrange(ID_NUMORD) %>%
    as.data.frame()



  tra <- base_imputar  %>% group_by(ID_NUMORD) %>%
    mutate_at(c(variablesinte),.funs=list(rezago1=~lag(.)))#,

  tra <- tra  %>% filter(MES==mes & ANIO==year)



  mes_ant=tra

  mes_ant=as.data.frame(mes_ant)

  cap2=c("II_PA_PP_NPERS_EP","AJU_II_PA_PP_SUELD_EP","II_PA_TD_NPERS_ET",
         "AJU_II_PA_TD_SUELD_ET","II_PA_TI_NPERS_ETA","AJU_II_PA_TI_SUELD_ETA","II_PA_AP_AAEP","AJU_II_PA_AP_AAS_AP",
         "II_PP_PP_NPERS_OP","AJU_II_PP_PP_SUELD_OP","II_PP_TD_NPERS_OT","AJU_II_PP_TD_SUELD_OT","II_PP_TI_NPERS_OTA",
         "AJU_II_PP_TI_SUELD_OTA","II_PP_AP_APEP","AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T")

  for (j in cap2) {

    missing_values <- is.na(mes_ant[, j])

    # Assign values to the selected rows in column j
    mes_ant[missing_values, j] <- round(mes_ant[missing_values, paste0(j,"_rezago1")])

  }

  mes_ant=mes_ant%>% filter(MES==mes & ANIO==year) %>%
    arrange(ID_NUMORD)

  mes_ant=mes_ant %>% select(ANIO,MES,ID_NUMORD,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
                             AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
                             II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
                             AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
                             AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)

  cap3=c("AJU_III_PE_PRODUCCION","AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","III_EX_VEXIS" )

  mes_ant <- mes_ant[, !(names(mes_ant) %in% cap3)]
  mes_ant <- cbind(mes_ant, wowKNN[, cap3])
  mes_ant=arrange(mes_ant,mes_ant$ID_NUMORD)
  novtem=subset(datoscom, (ANIO == year & MES == mes))
  novtem=arrange(novtem,novtem$ID_NUMORD)

  novtem$II_PA_PP_NPERS_EP<-as.numeric(novtem$II_PA_PP_NPERS_EP)
  novtem$AJU_III_PE_VENTASEX<-as.numeric(novtem$AJU_III_PE_VENTASEX)
  novtem$AJU_II_HORAS_HEXTR_T<-as.numeric(novtem$AJU_II_HORAS_HEXTR_T)
  novtem=as.data.frame(novtem)
  datoscom <- subset(datoscom, !(ANIO == year & MES == mes))
  datoscom<- as.data.frame(datoscom)
  datoscom$MES=as.numeric(datoscom$MES)
  datoscom$ANIO=as.numeric(datoscom$ANIO)
  for (i in variablesinte) {
    datoscom[,i] <- as.numeric(datoscom[,i])
    datoscom[,i] <- ifelse(is.na(datoscom[,i]),0,datoscom[,i])
  }

  for (i in variablesinte){
    novtem[,i]<- mes_ant[,i]

  }
  imputa=rbind(datoscom,novtem)
  imputa=as.data.frame(imputa)
  for (i in variablesinte) {
    imputa[,i] <- as.numeric(imputa[,i])
  }
  imputa <- imputa %>% mutate_if(is.integer, as.numeric)
  write.csv(imputa,paste0(directorio,"/results/S4_imputacion/EMMET_PANEL_imputada_",meses[month],year,".csv"),row.names=F)
}




