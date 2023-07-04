#' Identificación de alertas
#'
#' Función para identificar establecimientos que pueden llegar a ser casos de imputación
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#' @return CSV file
#' @export
#'
#' @examples identificacion_outliers(directorio="/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)
#'
identificacion_outliers <- function(directorio,year,mes) {
  ### función detección de outliers
  library(dplyr)
  #library(tidyr)
  library(readxl)
  library(readr)
  library(data.table)
  library(forecast)
  meses <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
  month <- mes


  base_panel <- fread(paste0(directorio,"/results/S2_estandarizacion/EMMET_PANEL_estandarizado",meses[month],year,".csv"))
  variablesinte=c("II_PA_PP_NPERS_EP","AJU_II_PA_PP_SUELD_EP","II_PA_TD_NPERS_ET",
                  "AJU_II_PA_TD_SUELD_ET","II_PA_TI_NPERS_ETA","AJU_II_PA_TI_SUELD_ETA","II_PA_AP_AAEP","AJU_II_PA_AP_AAS_AP",
                  "II_PP_PP_NPERS_OP","AJU_II_PP_PP_SUELD_OP","II_PP_TD_NPERS_OT","AJU_II_PP_TD_SUELD_OT","II_PP_TI_NPERS_OTA",
                  "AJU_II_PP_TI_SUELD_OTA","II_PP_AP_APEP","AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T",
                  "AJU_III_PE_PRODUCCION","AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","III_EX_VEXIS")

  datos<- as.data.frame(base_panel)
  for (i in variablesinte) {

    datos[,i] <- as.numeric(datos[,i])
    datos[,i] <- ifelse(is.na(datos[,i]),0,datos[,i])
  }
  datos$MES=as.numeric(datos$MES)
  datos$ANIO=as.numeric(datos$ANIO)
  tra<- datos %>% select(ANIO,MES,NOVEDAD,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
                         AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
                         II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
                         AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
                         AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)
  tra <- tra  %>% group_by(ID_NUMORD) %>%
    mutate_at(c(variablesinte),.funs=list(rezago1=~lag(.),mes_anio_ant=~ifelse(sum(MES==mes & ANIO==(year-1))>=1,.[MES==mes & ANIO==year-1],NA),
                                          promedio=~mean(.,na.rm = T),devest=~sd(.,na.rm=T),moda_value=~sum(.%in%.[MES==mes & ANIO==year])))#,



  tra <- tra %>% group_by(ID_NUMORD) %>% filter((MES>=mes & ANIO==year-2) |(ANIO==year-1)|(MES<=mes & ANIO==year)) %>%
    mutate_at(c(variablesinte),.funs=list(tsout=~ifelse(sum(.[tsoutliers(.)$index]==.[MES==mes & ANIO==year])>=1,1,0),
                                          serie_constante=~ifelse(sum(.[length(.)-23:length(.)]==.[1])>=24,1,0)))#,

  tra <- tra  %>% filter(ANIO==year & MES==mes) %>%as.data.frame()

  for (i in variablesinte) {
    tra[,paste0(i,"_var_mes_ant")]=(tra[,i]-tra[,paste0(i,"_rezago1")])/tra[,paste0(i,"_rezago1")]
    tra[,paste0(i,"_var_mes_anio_ant")]=(tra[,i]-tra[,paste0(i,"_mes_anio_ant")])/tra[,paste0(i,"_mes_anio_ant")]
    tra[tra[,i]!=0 & tra[,paste0(i,"_rezago1")]==0,paste0(i,"_var_mes_ant")] <- 1
    tra[tra[,i]==0 & tra[,paste0(i,"_rezago1")]==0,paste0(i,"_var_mes_ant")] <- 0
    tra[,paste0(i,"_Li")]=tra[,paste0(i,"_promedio")]-1.96*tra[,paste0(i,"_devest")]
    tra[,paste0(i,"_Ls")]=tra[,paste0(i,"_promedio")]+1.96*tra[,paste0(i,"_devest")]
    tra[,paste0(i,"_caso de imputacion")]=ifelse((tra[,i]==0 & tra[,paste0(i,"_var_mes_ant")]==0),"continua",ifelse(tra$NOVEDAD==5 | (tra[,i]==0 & tra[,paste0(i,"_var_mes_ant")]!=0),"imputación deuda",ifelse(i %in% c("II_PA_PP_NPERS_EP","II_PA_TD_NPERS_ET","II_PA_TI_NPERS_ETA","II_PA_AP_AAEP",
                                                                                                                                                                                                                         "II_PP_PP_NPERS_OP","II_PP_TD_NPERS_OT","II_PP_TI_NPERS_OTA","II_PP_AP_APEP")
                                                                                                                                                                                                                & tra[,i]!=0 & abs(tra[,paste0(i,"_var_mes_ant")])>0.2 &
                                                                                                                                                                                                                  (tra[,i]<=tra[,paste0(i,"_Li")] | tra[,i]>=tra[,paste0(i,"_Ls")]) & tra[,paste0(i,"_moda_value")]<=1 ,
                                                                                                                                                                                                                "imputación caso especial",ifelse(!i %in% c("II_PA_PP_NPERS_EP","II_PA_TD_NPERS_ET","II_PA_TI_NPERS_ETA","II_PA_AP_AAEP",
                                                                                                                                                                                                                                                            "II_PP_PP_NPERS_OP","II_PP_TD_NPERS_OT","II_PP_TI_NPERS_OTA","II_PP_AP_APEP")&
                                                                                                                                                                                                                                                    tra[,paste0(i,"_tsout")]==1  & tra[,paste0(i,"_moda_value")]<=1 & tra[,i]!=0 & abs(tra[,paste0(i,"_var_mes_ant")])>0.2 ,"imputación caso especial","continua"))))
  }


  final <- tra %>%
    select(ANIO,MES,NOVEDAD,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
           AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
           II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
           AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
           AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS,ends_with("imputacion")) %>% arrange(ID_NUMORD) %>% as.data.frame()

  write.csv(final,paste0(directorio,"/results/S3_identificacion_alertas/EMMET_PANEL_alertas_",meses[month],year,".csv"),row.names=F)
}

