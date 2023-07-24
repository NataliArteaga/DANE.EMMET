#' Estandarizacion
#'
#' @description Esta funcion utiliza como insumo la base de datos integrada, es decir la
#'  base que denominamos: "Base Panel". Tiene  como objetivo analizar el periodo de tiempo
#'  en el que cada empresas reporta la producción, en la encuesta, y con ello estandarizar
#'  las  fechas vacías, tanto de inicio y fin, para que se mantenga el mismo margen de reporte.
#'  Adicionalmente, en las variables numeticas, los datos faltantes se cambian por cero.
#'  A estas mismas variables se es establece un formato numerico para la porterior manipulación.
#'  Finalmente se exporta un archivo de tipo .csv con la información actualizada de las fechas.
#'
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#' @return CSV file
#' @export
#'
#' @examples integracion(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)
#'
#' @details
#'  Esta funcion recorre las siguiente variables de fecha de inicio y fin:
#'
#'  II_PA_PP_IEP, II_PA_PP_FEP.
#'  II_PA_TD_IET, II_PA_TD_FET.
#'  II_PA_TI_IETA, II_PA_TI_FETA.
#'  II_PA_AP_AI_AP, II_PA_AP_AF_AP.
#'  II_PP_PP_IOP, II_PP_PP_FOP.
#'  II_PP_TD_IOT, II_PP_TD_FOT.
#'  II_PP_TI_IOTA, II_PP_TI_FOTA.
#'  II_PP_AP_AI_PP, II_PP_AP_AF_PP.
#'  II_HORAS_HORDI_D, II_HORAS_HORDI_H.
#'  III_PE_IV, III_PE_FV.
#'
#'
#'
#'  La finalidad de la funcion es estandarizar el periodo de las fechas
#'  en que las empresas generan el reporte mensual.
#'
#'  Para ello, se analiza el reporte del mes anretior y se calculan el numero de
#'  días de este. Con ese dato se reemplazan las fehas faltantes del mes presente,
#'  o las reportados como cero.
#'
#'  Con esto se logra una unificación del periodo de repote de las empresas.
#'  En caso dado de no tener reporte del mes anterior se asigna un reporte mensual.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'


estandarizacion <- function(directorio,
                            mes=11,
                            anio=2022){
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)

  source("https://raw.githubusercontent.com/NataliArteaga/DANE.EMMET/main/R/utils.R")

  # Base Panel --------------------------------------------------------------



  base_panel_2<-read.csv(paste0(directorio,"/results/S1_integracion/EMMET_PANEL_trabajo_original_",meses[mes],anio,".csv"), sep = ",")


  # Estandarizar formato de fechas ------------------------------------------



  base_panel_2[cols_to_date] <- apply(base_panel_2[cols_to_date], 2, as.numeric)

  # Estandarizar Fechas -----------------------------------------------------

  Fechas<-base_panel_2 %>%
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


    #Funcion

    #Se actualiza el valor de las fechas iniciales, teniendo en cuenta la
    #información de la fechas del mes anterior, dando una continuidad
    #a la información de las fechas, ependiendo el reporte de la empresa.

    Fechas_Mod<-function(base,V_Inicial,V_Final){
      Base_Final<-base%>%
        select(LLAVE,ANIO,NOVEDAD,MES,{{V_Inicial}},{{V_Final}}) %>%
        mutate(
          fecha_ini_ant = round(as.numeric(lag({{V_Inicial}})),0),
          fecha_fin_ant = round(as.numeric(lag({{V_Final}})),0)) %>%
        filter((ANIO==anio & MES==mes & NOVEDAD==99) &
                 (({{V_Inicial}}==0 & {{V_Final}}==0) |
                    (is.na({{V_Inicial}}) & is.na({{V_Final}})))) %>%
        mutate(Variable_Inicial = case_when(
          fecha_ini_ant==0  & fecha_fin_ant==0 ~ 0,
          is.na(fecha_ini_ant)  & is.na(fecha_fin_ant) ~ 0,
          ( is.na(fecha_ini_ant)==FALSE & is.na(fecha_fin_ant) ) ~  0,
          ({{V_Inicial}}==0 | is.na({{V_Inicial}})) & ( fecha_ini_ant!=0 | is.na(fecha_ini_ant)==FALSE ) ~  as.numeric(fecha_fin_ant)+1,
          TRUE ~ {{V_Inicial}})) %>%
        mutate(Variable_Final = case_when(
          fecha_ini_ant==0  & fecha_fin_ant==0 ~ 0,
          is.na(fecha_ini_ant)==TRUE  & is.na(fecha_fin_ant)==TRUE ~ 0,
          ( is.na(fecha_ini_ant)==FALSE & is.na(fecha_fin_ant)==TRUE ) ~  0,
          ({{V_Final}}==0 | is.na({{V_Final}})==TRUE) & ( fecha_fin_ant!=0 | is.na(fecha_fin_ant)==FALSE ) ~  as.numeric(fecha_fin_ant)+1+(fecha_fin_ant-fecha_ini_ant),
          TRUE ~ {{V_Final}}))
      return(Base_Final)}

    #Función de ayuda para intersectar las bases y actualizar las variables de
    # fecha, asignando el valor de fecha a los campos vaciós

    Remplazo<-function(V_FI,V_FF){
      Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
      base_panel_2<-base_panel_2 %>%
        left_join(Fechas_2,by=c("LLAVE"="LLAVE"))
      base_panel_2<-base_panel_2 %>%
        mutate(N_I = case_when(
          is.na(Variable_Inicial) ~ as.numeric({{V_FI}}),
          TRUE ~ as.numeric(Variable_Inicial))) %>%
        mutate(N_F = case_when(
          is.na(Variable_Final) ~ as.numeric({{V_FF}}),
          TRUE ~ as.numeric(Variable_Final))) %>%
        as.data.frame()
      return(base_panel_2)
    }

    #Función de ayuda que reemplaza y actualiza las variables de fecha para
    # establecer la base final.

    Igualacion<-function(V_FI,V_FF){
      base_panel_2$V_FI<-base_panel_2[,"N_I"]
      base_panel_2$V_FF<-base_panel_2[,"N_F"]
      borrar <- c("Variable_Inicial","Variable_Final","N_I","N_F")
      base_panel_2 <- base_panel_2[ , !(names(base_panel_2) %in% borrar)]
    }


    #Se leen las funciones con las variables de fecha Inicio y fin según correspondan


    #II_PA_PP__IEP,II_PA_PP__FEP
    Fechas_2    <-Fechas_Mod(Fechas,II_PA_PP_IEP,II_PA_PP_FEP)
    base_panel_2<-Remplazo(II_PA_PP_IEP,II_PA_PP_FEP)
    base_panel_2<-Igualacion(II_PA_PP_IEP,II_PA_PP_FEP)

    #II_PA_TI__IETA,II_PA_TI__FETA
    Fechas_2    <-Fechas_Mod(Fechas,II_PA_TI_IETA,II_PA_TI_FETA)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PA_TI_IETA,II_PA_TI_FETA)
    base_panel_2<-Igualacion(II_PA_TI_IETA,II_PA_TI_FETA)

    #II_PA_AP__AI_AP,II_PA_AP__AF_AP
    Fechas_2    <-Fechas_Mod(Fechas,II_PA_AP_AI_AP,II_PA_AP_AF_AP)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PA_AP_AI_AP,II_PA_AP_AF_AP)
    base_panel_2<-Igualacion(II_PA_AP_AI_AP,II_PA_AP_AF_AP)

    #II_PP_PP__IOP,II_PP_PP__FOP
    Fechas_2    <-Fechas_Mod(Fechas,II_PP_PP_IOP,II_PP_PP_FOP)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PP_PP_IOP,II_PP_PP_FOP)
    base_panel_2<-Igualacion(II_PP_PP_IOP,II_PP_PP_FOP)

    #II_PP_TD__IOT,II_PP_TD__FOT
    Fechas_2    <-Fechas_Mod(Fechas,II_PP_TD_IOT,II_PP_TD_FOT)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PP_TD_IOT,II_PP_TD_FOT)
    base_panel_2<-Igualacion(II_PP_TD_IOT,II_PP_TD_FOT)

    #II_PP_TI__IOTA,II_PP_TI__FOTA
    Fechas_2    <-Fechas_Mod(Fechas,II_PP_TI_IOTA,II_PP_TI_FOTA)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PP_TI_IOTA,II_PP_TI_FOTA)
    base_panel_2<-Igualacion(II_PP_TI_IOTA,II_PP_TI_FOTA)

    #II_PP_AP__AI_PP,II_PP_AP__AF_PP
    Fechas_2    <-Fechas_Mod(Fechas,II_PP_AP_AI_PP,II_PP_AP_AF_PP)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_PP_AP_AI_PP,II_PP_AP_AF_PP)
    base_panel_2<-Igualacion(II_PP_AP_AI_PP,II_PP_AP_AF_PP)

    #II_HORAS__HORDI_D,II_HORAS__HORDI_H
    Fechas_2    <-Fechas_Mod(Fechas,II_HORAS_HORDI_D,II_HORAS_HORDI_H)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(II_HORAS_HORDI_D,II_HORAS_HORDI_H)
    base_panel_2<-Igualacion(II_HORAS_HORDI_D,II_HORAS_HORDI_H)

    #III_PE__IV,III_PE__FV
    Fechas_2    <-Fechas_Mod(Fechas,III_PE_IV,III_PE_FV)
    Fechas_2    <-Fechas_2[,c("LLAVE","Variable_Inicial","Variable_Final")]
    base_panel_2<-Remplazo(III_PE_IV,III_PE_FV)
    base_panel_2<-Igualacion(III_PE_IV,III_PE_FV)



    # Cambiar a fechas --------------------------------------------------------


    base_panel_2<-base_panel_2 %>%
      mutate(
        ifelse(is.na(II_PA_PP_IEP),0,II_PA_PP_IEP),
        ifelse(II_PA_PP_FEP,0,II_PA_PP_FEP),
        ifelse(II_PA_TD_IET,0,II_PA_TD_IET),
        ifelse(II_PA_TD_FET,0,II_PA_TD_FET),
        ifelse(II_PA_TI_IETA,0,II_PA_TI_IETA),
        ifelse(II_PA_TI_FETA,0,II_PA_TI_FETA),
        ifelse(II_PA_AP_AI_AP,0,II_PA_AP_AI_AP),
        ifelse(II_PA_AP_AF_AP,0,II_PA_AP_AF_AP),
        ifelse(II_PP_PP_IOP,0,II_PP_PP_IOP),
        ifelse(II_PP_PP_FOP,0,II_PP_PP_FOP),
        ifelse(II_PP_TD_IOT,0,II_PP_TD_IOT),
        ifelse(II_PP_TD_FOT,0,II_PP_TD_FOT),
        ifelse(II_PP_TI_IOTA,0,II_PP_TI_IOTA),
        ifelse(II_PP_TI_FOTA,0,II_PP_TI_FOTA),
        ifelse(II_PP_AP_AI_PP,0,II_PP_AP_AI_PP),
        ifelse(II_HORAS_HORDI_D,0,II_HORAS_HORDI_D),
        ifelse(II_HORAS_HORDI_H,0,II_HORAS_HORDI_H),
        ifelse(III_PE_IV,0,III_PE_IV),
        ifelse(III_PE_FV,0,III_PE_FV)
      )


    # Estandarizar Variables Numericas ----------------------------------------

    base_panel_2<-base_panel_2 %>%
      mutate(
        ifelse(AJU_III_PE_VENTASIN,0,AJU_III_PE_VENTASIN),
        ifelse(AJU_III_PE_VENTASEX,0,AJU_III_PE_VENTASEX),
        ifelse(II_PA_PP_NPERS_EP,0,II_PA_PP_NPERS_EP),
        ifelse(II_PA_TD_NPERS_ET,0,II_PA_TD_NPERS_ET),
        ifelse(II_PA_TI_NPERS_ETA,0,II_PA_TI_NPERS_ETA),
        ifelse(II_PA_AP_AAEP,0,II_PA_AP_AAEP),
        ifelse(II_PP_PP_NPERS_OP,0,II_PP_PP_NPERS_OP),
        ifelse(II_PP_TD_NPERS_OT,0,II_PP_TD_NPERS_OT),
        ifelse(II_PP_TI_NPERS_OTA,0,II_PP_TI_NPERS_OTA),
        ifelse(II_PP_AP_APEP,0,II_PP_AP_APEP),
        ifelse(AJU_II_PA_PP_SUELD_EP,0,AJU_II_PA_PP_SUELD_EP),
        ifelse(AJU_II_PA_TD_SUELD_ET,0,AJU_II_PA_TD_SUELD_ET),
        ifelse(AJU_II_PA_TI_SUELD_ETA,0,AJU_II_PA_TI_SUELD_ETA),
        ifelse(AJU_II_PA_AP_AAS_AP,0,AJU_II_PA_AP_AAS_AP),
        ifelse(AJU_II_PP_PP_SUELD_OP,0,AJU_II_PP_PP_SUELD_OP),
        ifelse(AJU_II_PP_TD_SUELD_OT,0,AJU_II_PP_TD_SUELD_OT),
        ifelse(AJU_II_PP_TI_SUELD_OTA,0,AJU_II_PP_TI_SUELD_OTA),
        ifelse(AJU_II_PP_AP_AAS_PP,0,AJU_II_PP_AP_AAS_PP),
        ifelse(AJU_II_HORAS_HORDI_T,0,AJU_II_HORAS_HORDI_T),
        ifelse(AJU_II_HORAS_HEXTR_T,0,AJU_II_HORAS_HEXTR_T),
        ifelse(AJU_III_PE_PRODUCCION,0,AJU_III_PE_PRODUCCION),
        ifelse(III_EX_VEXIS,0,III_EX_VEXIS)
      )

    return(base_panel_2)
  }

  base_panel<-funcion_global()

  # Exportacion tabla -------------------------------------------------------

  #write.csv(base_panel,"base_pane_estandarizacion.csv")
  write.csv(base_panel,paste0(directorio,"/results/S2_estandarizacion/EMMET_PANEL_estandarizado",meses[mes],anio,".csv"),row.names=F)
}
