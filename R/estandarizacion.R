#' Estandarización
#'
#' Analizar el periodo de tiempo reportado por cada establecimiento, para estandarizar el inicio y fin del periodo reportado
#'
#' @param mes Definir  mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#' @return CSV file
#' @export
#'
#' @examples estandarizacion(directorio="/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)


estandarizacion <- function(directorio,
                            mes,
                            anio){
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)

  # Base Panel --------------------------------------------------------------

  meses <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
  month <- mes
  year  <- anio

  base_panel<-read.csv(paste0(directorio,"/results/S1_integracion/EMMET_PANEL_trabajo_original_",month,year,".csv"), sep = ",")
  base_panel_2<-base_panel

  # Cargar base ------------------------------------------------------------

  #base_panel <- read.csv(paste0("data/",year,"/",month,"/EMMET_PANEL_trabajo_original",month,year,".csv"), sheet = "LOGISTICA")
  #base_panel_2<-base_panel

  # Estandarizar formato de fechas ------------------------------------------


  cols_to_date <- c("II_PA_PP_IEP","II_PA_PP_FEP","II_PA_TD_IET","II_PA_TD_FET",
                    "II_PA_TI_IETA","II_PA_TI_FETA","II_PA_AP_AI_AP","II_PA_AP_AF_AP",
                    "II_PP_PP_IOP","II_PP_PP_FOP","II_PP_TD_IOT","II_PP_TD_FOT",
                    "II_PP_TI_IOTA","II_PP_TI_FOTA","II_PP_AP_AI_PP","II_PP_AP_AF_PP",
                    "II_HORAS_HORDI_D","II_HORAS_HORDI_H",
                    "III_PE_IV","III_PE_FV")
  base_panel[cols_to_date] <- apply(base_panel[cols_to_date], 2, as.numeric)

  # Estandarizar Fechas -----------------------------------------------------

  Fechas<-base_panel %>%
    select(LLAVE,ID_NUMORD,ANIO,MES,NOVEDAD,II_PA_PP_IEP,II_PA_PP_FEP,II_PA_TD_IET,
           II_PA_TD_FET,II_PA_TI_IETA,II_PA_TI_FETA,
           II_PA_AP_AI_AP,II_PA_AP_AF_AP,II_PP_PP_IOP,II_PP_PP_FOP,
           II_PP_TD_IOT,II_PP_TD_FOT,II_PP_TI_IOTA,II_PP_TI_FOTA,
           II_PP_AP_AI_PP,II_PP_AP_AF_PP,II_HORAS_HORDI_D,II_HORAS_HORDI_H,
           III_PE_IV,III_PE_FV) %>%
    arrange(ID_NUMORD,ANIO) %>%
    group_by(ID_NUMORD)



  #Funcion

  funcion_global<-function(){

    Fechas_Mod<-function(base,V_Inicial,V_Final){
      Base_Final<-base%>%
        select(LLAVE,ANIO,NOVEDAD,MES,{{V_Inicial}},{{V_Final}}) %>%
        mutate(
          fecha_ini_ant = round(as.numeric(lag({{V_Inicial}})),0),
          fecha_fin_ant = round(as.numeric(lag({{V_Final}})),0)) %>%
        filter((ANIO==2022 & MES==11 & NOVEDAD==99) &
                 (({{V_Inicial}}==0 & {{V_Final}}==0) |
                    (is.na({{V_Inicial}})==TRUE & is.na({{V_Final}})==TRUE)) ) %>%
        mutate(Variable_Inicial = case_when(
          fecha_ini_ant==0  & fecha_fin_ant==0 ~ 0,
          is.na(fecha_ini_ant)==TRUE  & is.na(fecha_fin_ant)==TRUE ~ 0,
          (is.na(fecha_ini_ant)==TRUE & is.na(fecha_fin_ant)==FALSE) | ( is.na(fecha_ini_ant)==FALSE & is.na(fecha_fin_ant)==TRUE ) ~  0,
          ({{V_Inicial}}==0 | is.na({{V_Inicial}})==TRUE) & ( fecha_ini_ant!=0 | is.na(fecha_ini_ant)==FALSE ) ~  as.numeric(fecha_fin_ant)+1,
          TRUE ~ {{V_Inicial}})) %>%
        mutate(Variable_Final = case_when(
          fecha_ini_ant==0  & fecha_fin_ant==0 ~ 0,
          is.na(fecha_ini_ant)==TRUE  & is.na(fecha_fin_ant)==TRUE ~ 0,
          (is.na(fecha_ini_ant)==TRUE & is.na(fecha_fin_ant)==FALSE) | ( is.na(fecha_ini_ant)==FALSE & is.na(fecha_fin_ant)==TRUE ) ~  0,
          ({{V_Final}}==0 | is.na({{V_Final}})==TRUE) & ( fecha_fin_ant!=0 | is.na(fecha_fin_ant)==FALSE ) ~  as.numeric(fecha_fin_ant)+1+(fecha_fin_ant-fecha_ini_ant),
          TRUE ~ {{V_Final}}))
      return(Base_Final)}

    Remplazo<-function(V_FI,V_FF){
      base_panel_2<-base_panel_2 %>%
        left_join(Fechas_2,by=c("LLAVE"="LLAVE"))
      base_panel_2<-base_panel_2 %>%
        mutate(N_I = case_when(
          is.na(Variable_Inicial)==TRUE ~ as.numeric({{V_FI}}),
          TRUE ~ as.numeric(Variable_Inicial))) %>%
        mutate(N_F = case_when(
          is.na(Variable_Final)==TRUE ~ as.numeric({{V_FF}}),
          TRUE ~ as.numeric(Variable_Final))) %>%
        as.data.frame()

      return(base_panel_2)
    }


    Igualacion<-function(V_FI,V_FF){
      base_panel_2[[V_FI]]<-base_panel_2[,N_I]
      base_panel_2[[V_FF]]<-base_panel_2[,N_F]
      borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
      base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]
    }




    #II_PA_PP__IEP,II_PA_PP__FEP
    Fechas_2    <-Fechas_Mod(Fechas,II_PA_PP_IEP,II_PA_PP_FEP)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PA_PP_IEP,II_PA_PP_FEP)
    base_panel_2$II_PA_PP_IEP<-base_panel_2$N_I
    base_panel_2$II_PA_PP_FEP<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]

    #II_PA_TI__IETA,II_PA_TI__FETA
    Fechas_2    <-Fechas_Mod(Fechas,II_PA_TI_IETA,II_PA_TI_FETA)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PA_TI_IETA,II_PA_TI_FETA)
    base_panel_2$II_PA_TI_IETA<-base_panel_2$N_I
    base_panel_2$II_PA_TI_FETA<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]

    #II_PA_AP__AI_AP,II_PA_AP__AF_AP
    Fechas_2    <-Fechas_Mod(Fechas,II_PA_AP_AI_AP,II_PA_AP_AF_AP)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PA_AP_AI_AP,II_PA_AP_AF_AP)
    base_panel_2$II_PA_AP_AI_AP<-base_panel_2$N_I
    base_panel_2$II_PA_AP_AF_AP<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]

    #II_PP_PP__IOP,II_PP_PP__FOP
    Fechas_2    <-Fechas_Mod(Fechas,II_PP_PP_IOP,II_PP_PP_FOP)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PP_PP_IOP,II_PP_PP_FOP)
    base_panel_2$II_PP_PP_IOP<-base_panel_2$N_I
    base_panel_2$II_PP_PP_FOP<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]

    #II_PP_TD__IOT,II_PP_TD__FOT
    Fechas_2    <-Fechas_Mod(Fechas,II_PP_TD_IOT,II_PP_TD_FOT)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PP_TD_IOT,II_PP_TD_FOT)
    base_panel_2$II_PP_TD_IOT<-base_panel_2$N_I
    base_panel_2$II_PP_TD_FOT<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]

    #II_PP_TI__IOTA,II_PP_TI__FOTA
    Fechas_2    <-Fechas_Mod(Fechas,II_PP_TI_IOTA,II_PP_TI_FOTA)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PP_TI_IOTA,II_PP_TI_FOTA)
    base_panel_2$II_PP_TI_IOTA<-base_panel_2$N_I
    base_panel_2$II_PP_TI_FOTA<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]

    #II_PP_AP__AI_PP,II_PP_AP__AF_PP
    Fechas_2    <-Fechas_Mod(Fechas,II_PP_AP_AI_PP,II_PP_AP_AF_PP)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PP_AP_AI_PP,II_PP_AP_AF_PP)
    base_panel_2$II_PP_AP_AI_PP<-base_panel_2$N_I
    base_panel_2$II_PP_AP_AF_PP<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]

    #II_HORAS__HORDI_D,II_HORAS__HORDI_H
    Fechas_2    <-Fechas_Mod(Fechas,II_HORAS_HORDI_D,II_HORAS_HORDI_H)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_HORAS_HORDI_D,II_HORAS_HORDI_H)
    base_panel_2$II_HORAS_HORDI_D<-base_panel_2$N_I
    base_panel_2$II_HORAS_HORDI_H<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]

    #III_PE__IV,III_PE__FV
    Fechas_2    <-Fechas_Mod(Fechas,III_PE_IV,III_PE_FV)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(III_PE_IV,III_PE_FV)
    base_panel_2$III_PE_IV<-base_panel_2$N_I
    base_panel_2$III_PE_FV<-base_panel_2$N_F
    borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
    base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]



    # Cambiar a fechas --------------------------------------------------------

    cols_to_date <- c("II_PA_PP_IEP","II_PA_PP_FEP","II_PA_TD_IET","II_PA_TD_FET",
                      "II_PA_TI_IETA","II_PA_TI_FETA","II_PA_AP_AI_AP","II_PA_AP_AF_AP",
                      "II_PP_PP_IOP","II_PP_PP_FOP","II_PP_TD_IOT","II_PP_TD_FOT",
                      "II_PP_TI_IOTA","II_PP_TI_FOTA","II_PP_AP_AI_PP","II_PP_AP_AF_PP",
                      "II_HORAS_HORDI_D","II_HORAS_HORDI_H",
                      "III_PE_IV","III_PE_FV")


    for(i in 1:nrow(base_panel_2)){
      for (j in cols_to_date){
        ifelse(is.na(base_panel_2[i,j])==TRUE,0,base_panel_2[i,j])
      }
    }

    # Estandarizar Variables Numericas ----------------------------------------

    cols_to_cero<-c("AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","II_PA_PP_NPERS_EP",
                    "II_PA_TD_NPERS_ET","II_PA_TI_NPERS_ETA","II_PA_AP_AAEP",
                    "II_PP_PP_NPERS_OP","II_PP_TD_NPERS_OT","II_PP_TI_NPERS_OTA",
                    "II_PP_AP_APEP","AJU_II_PA_PP_SUELD_EP","AJU_II_PA_TD_SUELD_ET",
                    "AJU_II_PA_TI_SUELD_ETA","AJU_II_PA_AP_AAS_AP","AJU_II_PP_PP_SUELD_OP",
                    "AJU_II_PP_PP_SUELD_OP","AJU_II_PP_TD_SUELD_O","AJU_II_PP_TI_SUELD_OTA",
                    "AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T",
                    "AJU_III_PE_PRODUCCION","III_EX_VEXIS")

    for(i in 1:nrow(base_panel_2)){
      for (j in cols_to_cero){
        ifelse(is.na(base_panel_2[i,j])==TRUE,0,base_panel_2[i,j])
      }
    }

    base_panel<-base_panel_2

    return(base_panel)
  }

  base_panel<-funcion_global()

  # Exportacion tabla -------------------------------------------------------

  #write.csv(base_panel,"base_pane_estandarizacion.csv")
  write.csv(base_panel,paste0(directorio,"/results/S2_estandarizacion/EMMET_PANEL_estandarizado",month,year,".csv"),row.names=F)
}
