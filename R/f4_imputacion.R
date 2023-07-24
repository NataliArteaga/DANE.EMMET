#' Imputación outliers
#'
#' Función para realizar la imputación por deuda y por casos especiales.
#'
#' @param mes Definir las tres primeras letras del mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#' @details La metodología de imputación es la siguiente:
#'
#' 1.	Para las variables de capítulo 2 la metodología a usar es la imputación por el mes anterior.
#'
#' 2.	Para las variables de capítulo 3 la metodología a usar para imputación por casos especiales
#'  es el KNN combinado con variación mes anterior. Para la imputación por deuda en variables pertenecientes
#'  a capítulo 3 se realiza un proceso similar.
#'
#' El KNN combinado consta de los siguientes pasos:
#'
#' 1. Realizar un primer KNN imputando los individuos atípicos en las variables de interés (KNN1).
#'
#' 2. Calcular la variación con respecto al mes anterior.
#'
#' 3. Realizar un segundo KNN para la variación con respecto al mes anterior de los
#' establecimientos del mismo dominio (KNN2).
#'
#' 4. Calcular el valor final de la siguiente manera:
#' \deqn{ \hat{y} = KNN_1*(1+KNN_2)}
#' Donde \eqn{ \hat{y}} es el valor resultante con el que se imputara la variable de interés.
#'
#' Para el caso de imputación deuda en capítulo 3 se sigue la siguiente formula:
#' \deqn{ \hat{y} = MA*(1+KNN_2)}
#' Esto es que el valor resultante con el que se imputara la variable de interés es igual al valor
#' del mes anterior, multiplicado por 1 más la variación con respecto al mes anterior de los establecimientos
#' del mismo dominio.
#'
#' Para conocer más sobre la imputación por el método k-Nearest Neighbors ver \code{\link[VIM:kNN]{KNN_VIM}}.
#'
#' @return CSV file
#' @export
#'
#' @examples imputacion_outliers(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)


# Función identificación e imputación de outliers ------------------------------------------



imputacion_outliers <- function(directorio,mes,anio) {


  # librerias ---------------------------------------------------------------


  library(dplyr)
  library(readxl)
  library(readr)
  library(data.table)
  library(VIM)
  source("https://raw.githubusercontent.com/NataliArteaga/DANE.EMMET/main/R/utils.R")

  anio  <- anio

  #cargar base estandarizada
  datos <- fread(paste0(directorio,"/results/S2_estandarizacion/EMMET_PANEL_estandarizado",meses[mes],anio,".csv"))

  #crear una copia de la base de datos
  datoscom=datos
  #crear un data frame con las variables de interes
  datos=datos %>% select(ANIO,MES,NOVEDAD,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
                         AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
                         II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
                         AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
                         AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)
  #convertir la base en data frame y convertir variables de año y mes en numéricas
  datos<- as.data.frame(datos)
  datos$MES=as.numeric(datos$MES)
  datos$ANIO=as.numeric(datos$ANIO)
  for (i in variablesinte) {
    datos[,i] <- as.numeric(datos[,i])
    datos[,i] <- ifelse(is.na(datos[,i]),0,datos[,i])
  }
  #Dejar la base sin los datos del mes actual
  datos <- filter(datos, !(ANIO == anio & MES == mes))
  #cargar la base de alertas
  wowimp=fread(paste0(directorio,"/results/S3_identificacion_alertas/EMMET_PANEL_alertas_",meses[mes],anio,".csv"))
  wowimp=as.data.frame(wowimp)
  # Convertir los datos que son casos de imputación en NA
  for (i in variablesinte) {
    wowimp[wowimp[,paste0(i,"_caso_de_imputacion")]!="continua",i] <- NA
  }
  wowimp=wowimp %>% select(ANIO,MES,NOVEDAD,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
                           AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
                           II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
                           AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
                           AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)
  #Combinar la base sin el mes actual con la base de alertas
  base_imputar=rbind(datos,wowimp)


  # imputacion ---------------------------------------------------------------------

  #crear un data frame con el valor del mes anterior y del año anterior
  tra <- base_imputar  %>% group_by(ID_NUMORD) %>%
    mutate_at(c(variablesinte),.funs=list(rezago1=~lag(.),mes_anio_ant=~ifelse(sum(MES==mes & ANIO==(anio-1))>=1,.[MES==mes & ANIO==anio-1],NA)))#,
  tra <- tra  %>% filter(ANIO==anio & MES==mes)%>%
    as.data.frame()

  #calcular la variación que hubo con respecto al mes anterior
  for (i in variablesinte) {
    tra[,paste0(i,"_var_mes_ant")]=(tra[,i]-tra[,paste0(i,"_rezago1")])/tra[,paste0(i,"_rezago1")]
    tra[tra[,i]==0 & !is.na(tra[,i]) & tra[,paste0(i,"_rezago1")]==0& !is.na(tra[,paste0(i,"_rezago1")]),paste0(i,"_var_mes_ant")] <- 0
    tra[tra[,i]!=0 & !is.na(tra[,i]) & tra[,paste0(i,"_rezago1")]==0 & !is.na(tra[,paste0(i,"_rezago1")]),paste0(i,"_var_mes_ant")] <- 1
  }

  #Convertir algunas variables como factor para la función KNN
  base_imputar$ANIO=as.factor(base_imputar$ANIO)
  base_imputar$MES=as.factor(base_imputar$MES)
  base_imputar$ID_NUMORD=as.factor(base_imputar$ID_NUMORD)
  base_imputar$DOMINIOEMMET39=as.factor(base_imputar$DOMINIOEMMET39)

  # realizar la imputación de las variables de interes con el comando kNN
  set.seed(11)
  wowKNN4md <- base_imputar %>%
    group_by(ID_NUMORD) %>%
    group_modify(~ kNN(.x,numFun = mean,k=4))

  #filtrar por los valores del mes actual y ordenar por "ID_NUMORD"
  wowKNN4md=wowKNN4md%>% filter(MES==mes & ANIO==anio) %>%
    arrange(ID_NUMORD) %>%
    as.data.frame()

  #crea un data frame con las variaciones y convierte algunas variables como anio,mes, id_numord en factor
  traf1=tra %>% select(ANIO,MES,ID_NUMORD,DOMINIOEMMET39,ends_with("mes_ant"))
  traf1$ANIO=as.factor(traf1$ANIO)
  traf1$MES=as.factor(traf1$MES)
  traf1$ID_NUMORD=as.factor(traf1$ID_NUMORD)
  traf1$DOMINIOEMMET39=as.factor(traf1$DOMINIOEMMET39)
  #Vector con los nombres de las variables que contienen los valores de la variación de las variables del capitulo 3
  capf=c("AJU_III_PE_PRODUCCION_var_mes_ant","AJU_III_PE_VENTASIN_var_mes_ant","AJU_III_PE_VENTASEX_var_mes_ant","III_EX_VEXIS_var_mes_ant" )

  #realiza un segundo KNN para imputar la variación del mes anterior
  set.seed(11)
  wowKNNci3 <- traf1 %>%
    group_by(DOMINIOEMMET39) %>%
    group_modify(~ kNN(.x,numFun = median,k=6))

  wowKNNci3=wowKNNci3%>% filter(MES==mes & ANIO==anio) %>%
    arrange(ID_NUMORD) %>%
    as.data.frame()

  #crea un data frame combinando el KNN de la imputación de las variables y de la variación
  wowKNNciu3=cbind(wowKNN4md,wowKNNci3[,capf])


  #crea el valor final multiplicando el valor de la imputación de las variables por 1 más el valor de la
  #variación imputada
  for (i in cap3) {
    filas_completas <- !(complete.cases(wowimp[,paste0(i)]))
    wowKNNciu3[filas_completas,i]<- wowKNNciu3[filas_completas,i]*(1+ wowKNNciu3[filas_completas,paste0(i,"_var_mes_ant")])

  }
  #convierte el id_numord en factor
  tra$ID_NUMORD <- as.factor(tra$ID_NUMORD)


  # imputación por deuda ----------------------------------------------------
  wowKNNciu3= wowKNNciu3 %>%
    left_join(select(tra,c("ID_NUMORD",ends_with("rezago1"))), by = "ID_NUMORD")
  #valor para los establecimientos imputados por deuda multiplicando el valor del mes anterior por
  # 1 más el valor de la variación imputada
  for (i in cap3) {
    filas_completas <- which(wowimp$NOVEDAD==5)
    wowKNNciu3[filas_completas,i]<- wowKNNciu3[filas_completas,paste0(i,"_rezago1")]*(1+ wowKNNciu3[filas_completas,paste0(i,"_var_mes_ant")])

  }


  #crear una base con los valores del mes anterior
  tra <- base_imputar  %>% group_by(ID_NUMORD) %>%
    mutate_at(c(variablesinte),.funs=list(rezago1=~lag(.)))#,

  mes_ant <- tra  %>% filter(MES==mes & ANIO==anio)

  mes_ant=as.data.frame(mes_ant)


  for (j in cap2) {
    #identificar las filas que tienen NA
    missing_values <- is.na(mes_ant[, j])

    #imputar las variables del capítulo 2 por el valor del mes anterior
    mes_ant[missing_values, j] <- round(mes_ant[missing_values, paste0(j,"_rezago1")])

  }

  #filtrar solo por el mes de interés y dejar las variables importantes
  mes_ant=mes_ant%>% filter(MES==mes & ANIO==anio) %>%
    arrange(ID_NUMORD)

  mes_ant=mes_ant %>% select(ANIO,MES,NOVEDAD,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,
                             AJU_II_PA_TD_SUELD_ET,II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
                             II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,II_PP_TI_NPERS_OTA,
                             AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,
                             AJU_III_PE_PRODUCCION,AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)


  #quitar las columnas del capítulo 3 para imputarlos por el valor optenido con los KNN
  mes_ant=arrange(mes_ant,mes_ant$ID_NUMORD)
  mes_ant <- mes_ant[, !(names(mes_ant) %in% cap3)]
  mes_ant <- cbind(mes_ant, wowKNNciu3[, cap3])

  #crear una base con el mes de interes y ordenar por el id_numord
  novtem=filter(datoscom, (ANIO == anio & MES == mes))
  novtem=arrange(novtem,novtem$ID_NUMORD)

  #convertir algunas variables en numericas
  novtem$II_PA_PP_NPERS_EP<-as.numeric(novtem$II_PA_PP_NPERS_EP)
  novtem$AJU_III_PE_VENTASEX<-as.numeric(novtem$AJU_III_PE_VENTASEX)
  novtem$AJU_II_HORAS_HEXTR_T<-as.numeric(novtem$AJU_II_HORAS_HEXTR_T)
  novtem=as.data.frame(novtem)

  #Modificar la base copia para que no tenga el mes de interes
  datoscom <- filter(datoscom, !(ANIO == anio & MES == mes))
  datoscom<- as.data.frame(datoscom)
  datoscom$MES=as.numeric(datoscom$MES)
  datoscom$ANIO=as.numeric(datoscom$ANIO)
  for (i in variablesinte) {
    datoscom[,i] <- as.numeric(datoscom[,i])
    datoscom[,i] <- ifelse(is.na(datoscom[,i]),0,datoscom[,i])
  }

  #pegar las columnas imputadas en el data frame del mes actual
  for (i in variablesinte){
    novtem[,i]<- mes_ant[,i]

  }

  #combinar las base original estandarizada con el mes imputado

  imputa=rbind(datoscom,novtem)
  imputa=as.data.frame(imputa)
  for (i in variablesinte) {
    imputa[,i] <- as.numeric(imputa[,i])
  }
  imputa <- imputa %>% mutate_if(is.integer, as.numeric)



  # Exportar la base imputada -------------------------------

  write.csv(imputa,paste0(directorio,"/results/S4_imputacion/EMMET_PANEL_imputada_",meses[mes],anio,".csv"),row.names=F)
}


