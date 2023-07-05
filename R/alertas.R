#' Identificación de alertas
#'
#' Función para identificar establecimientos que pueden llegar a ser casos de imputación.
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada.
#'
#' @return CSV file
#' @export
#'
#' @examples identificacion_outliers(directorio="/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)
#'
identificacion_outliers <- function(directorio,year,mes) {
  ### función detección de outliers



  # librerias ---------------------------------------------------------------

  library(dplyr)
  library(readxl)
  library(readr)
  library(data.table)
  library(forecast)
  library(tsoutliers)
  source("R/utils.R")
  month <- mes
  year  <- year

  #cargar la base de datos
  base_panel <- fread(paste0(directorio,"/results/S2_estandarizacion/EMMET_PANEL_estandarizado",meses[month],year,".csv"))


  #convertir la base en data frame y convertir variables de año y mes en numéricas
  datos<- as.data.frame(base_panel)
  for (i in variablesinte) {

    datos[,i] <- as.numeric(datos[,i])
    datos[,i] <- ifelse(is.na(datos[,i]),0,datos[,i])
  }
  datos$MES=as.numeric(datos$MES)
  datos$ANIO=as.numeric(datos$ANIO)
  # identificación individuos a imputar para las variables del capítulo 2 ----------------------------------------------------


  #crear un data frame con solo las variables de interés
  tra<- datos %>% select(ANIO,MES,NOVEDAD,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
                         AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
                         II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
                         AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
                         AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)

  #Calcular métricas de interés como el promedio, la desviación estándar,etc; en los últimos 2 años
  tra <- tra  %>% group_by(ID_NUMORD) %>%  filter((MES>=mes & ANIO==year-2) |(ANIO==year-1)|(MES<=mes & ANIO==year)) %>%
    mutate_at(c(variablesinte),.funs=list(rezago1=~lag(.),mes_anio_ant=~ifelse(sum(MES==mes & ANIO==(year-1))>=1,.[MES==mes & ANIO==year-1],NA),
                                          promedio=~mean(.,na.rm = T),devest=~sd(.,na.rm=T),moda_value=~sum(.%in%.[MES==mes & ANIO==year])))#,


  #filtrar solo por el periodo actual
  tra <- tra  %>% filter(MES==mes & ANIO==year)

  #crear métricas para la carta de control que servirá para definir la regla de identificación de
  #individuos a imputar
  for (i in variablesinte) {
    tra[,paste0(i,"_var_mes_ant")]=(tra[,i]-tra[,paste0(i,"_rezago1")])/tra[,paste0(i,"_rezago1")]
    tra[,paste0(i,"_var_mes_anio_ant")]=(tra[,i]-tra[,paste0(i,"_mes_anio_ant")])/tra[,paste0(i,"_mes_anio_ant")]
    tra[tra[,i]!=0 & tra[,paste0(i,"_rezago1")]==0,paste0(i,"_var_mes_ant")] <- 1
    tra[tra[,i]==0 & tra[,paste0(i,"_rezago1")]==0,paste0(i,"_var_mes_ant")] <- 0
    tra[,paste0(i,"_Li")]=tra[,paste0(i,"_promedio")]-1.96*tra[,paste0(i,"_devest")]
    tra[,paste0(i,"_Ls")]=tra[,paste0(i,"_promedio")]+1.96*tra[,paste0(i,"_devest")]
    tra[,paste0(i,"_caso_de_imputacion")]=ifelse((tra[,i]==0 & tra[,paste0(i,"_rezago1")]==0),"continua",ifelse(tra$NOVEDAD==5 | (tra[,i]==0 & tra[,paste0(i,"_var_mes_ant")]!=0),"imputacion_deuda",ifelse(tra[,i]!=0 & abs(tra[,paste0(i,"_var_mes_ant")])>0.2 &
                                                                                                                                                                                                              (tra[,i]<tra[,paste0(i,"_Li")] | tra[,i]>tra[,paste0(i,"_Ls")]) & tra[,paste0(i,"_moda_value")]<=1 ,
                                                                                                                                                                                                            "imputacion_caso_especial","continua")))
  }

  # identificación individuos a imputar para las variables del capitulo 3 -----------------------------------------------

  #crear un data frame del mes actual
  novorg=datos %>% filter(ANIO==year & MES==mes) %>% arrange(ID_NUMORD)
  #vector con el id del establecimiento del mes actual
  id_estab=novorg$ID_NUMORD
  #crea un data frame vacio
  datafinal=NULL
  #función para identificar los valores atipicos en las variables del capitulo 3, utilizando la función tso
  # y la carta 24 meses
  #Que realice la función por cada uno de los id
  for (i in id_estab) {
    prueba=data.frame()
    prueba[1,"ID_NUMORD"]=i
    mmm <- datos %>% filter(ID_NUMORD==i) %>%
      as.data.frame()
    #para cada una de las variables en el capítulo 3 aplica la función tso para identificar datos atípicos
    for (j in cap3){
      #Verificar que el porcentaje de datos de ese establecimiento en la variable j sea mayor al 30%
      if ((sum(mmm[!is.na(mmm[,j]),j]==0)/dim(mmm)[1])<0.3){
        prueba[1,paste0(j,"_metodo_de_imputacion")]="tso"
        prueba[1,paste0(j,"_regla_de_imputacion")]=tryCatch(
          expr = {
            # Primera opción, calcula los valores de order y seasonal, los cuales sirven de insumo para
            #la función tso que identifica outliers
            fit <- auto.arima(mmm[,j],allowdrift = F)
            order = arimaorder(fit)
            seasonal=fit$arma[4:6]
            ifelse(sum(tso(as.ts(mmm[,j]), tsmethod = "arima",
                           args.tsmethod = list(order=c(order),seasonal=list(order=seasonal)))$outliers$time==which(mmm$MES==mes & mmm$ANIO==year))>=1,1,0)

          },  # Segunda opción en caso de error, usa tso con el metodo auto.arima
          error = function(e) {
            ifelse(sum(tso(as.ts(mmm[,j]), tsmethod = "auto.arima")$outliers$time==which(mmm$MES==mes & mmm$ANIO==year))>=1,1,0)
          },  # Tercera opción en caso de error, utiliza tso con order y seasonal calculados por defecto
          error = function(e) {

            ifelse(sum(tso(as.ts(mmm[,j]), tsmethod = "arima")$outliers$time==which(mmm$MES==mes & mmm$ANIO==year))>=1,1,0)
          },
          error = function(e) {

            # cuarta opción en caso de error, utiliza tso con los datos escalados
            ifelse(sum(tso(as.ts(scale(mmm[,j])), tsmethod = "arima")$outliers$time==which(mmm$MES==mes & mmm$ANIO==year))>=1,1,0)
          }
        )
      }else{#en dado caso que no se cumpla la condición del 30%, realizar carta control 24 meses
        mmm2 <- mmm %>% filter(ID_NUMORD==i) %>% filter((MES>=mes & ANIO==year-2) |(ANIO==year-1)|(MES<=mes & ANIO==year)) %>%
          as.data.frame()
        prueba[1,paste0(j,"_metodo_de_imputacion")]="IC"
        #calcula el limite superior e inferior de la carta control
        LI <- mean(mmm2[,j],na.rm=T)-1.96*sd(mmm2[,j],na.rm=T)
        LS <- mean(mmm2[,j],na.rm=T)+1.96*sd(mmm2[,j],na.rm=T)

        #marca como 1 si el valor se sale de los limites de control
        prueba[1,paste0(j,"_regla_de_imputacion")]=ifelse((mmm2[which(mmm2$MES==11 & mmm2$ANIO==2022),j]<LI |
                                                             mmm2[which(mmm2$MES==11 & mmm2$ANIO==2022),j]>LS),1,0)
      }}

    datafinal=rbind(datafinal,prueba)
  }
  #crea un solo dataframe con los resultados de la función que identifica datos atipicos
  traic=left_join(tra,datafinal,by= "ID_NUMORD")

  #crear la regla de indentificación de individuos a imputar para las variables del capitulo 3
  for (i in cap3) {
    traic[,paste0(i,"_caso_de_imputacion")]=ifelse((traic[,i]==0 & traic[,paste0(i,"_rezago1")]==0),"continua",ifelse(traic$NOVEDAD==5 | (traic[,i]==0 & traic[,paste0(i,"_var_mes_ant")]!=0),"imputacion_deuda",ifelse(traic[,paste0(i,"_regla_de_imputacion")]==1  & traic[,paste0(i,"_moda_value")]<=1 & traic[,i]!=0 & abs(traic[,paste0(i,"_var_mes_ant")])>0.2 ,"imputacion_caso_especial","continua")))
  }

  #crear un data frame final con las variables que necesitaremos para el proceso de imputación
  final <- traic %>%
    select(ANIO,MES,NOVEDAD,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
           AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
           II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
           AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
           AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS,ends_with("imputacion")) %>% arrange(ID_NUMORD) %>% as.data.frame()




  # Exportar data frame con la identificación de posibles casos de imputación -------------------------------


  write.csv(final,paste0(directorio,"/results/S3_identificacion_alertas/EMMET_PANEL_alertas_",meses[month],year,".csv"),row.names=F)
}

