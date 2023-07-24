#'  Integración
#'
#'  Con esta función se integraran los archivos de entrada que son necesarios en el proceso EMMET, con el fin
#'  de trabajar con una base llamada "base panel.csv".
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#' @return CSV file
#' @export
#'
#' @details En esta función se leen las siguientes bases:
#'
#' 1 Base logística: Es la base con todos los datos desde enero de 2018 hasta el mes
#' anterior al indicado en el parámetro mes.
#'
#' 2 Base Original: Es la base con los registros de los 3000 establecimientos para el mes y año
#' indicados en los parámetros.
#'
#' 3 Base paramétrica: Base con las características de cada establecimiento.
#'
#' 4 Base deflactor: base con los datos de IPP e IPC para cada CIIU4.
#'
#' 5 Base divipola: base con los códigos de los departamentos y municipios, con sus nombres estandarizados.
#'
#' El procedimiento para unificar las bases es el siguiente:
#'
#' 1 Se pegan las filas de la base original debajo de la base logística y se nombra base panel.
#'
#' 2 Se agregan las columnas de la base paramétrica a la base panel por medio del ID_NUMORD.
#'
#' 3 Adicionar las columnas de la base deflactor a partir del CIIU4, mes y año.
#'
#' 4 Finalmente se estandarizan los nombres de los departamentos y municipios, uniendo las columnas de la base
#' divipola por el ID_municipio.
#'
#'
#'
#' @examples f1_integracion(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)


f1_integracion <- function(directorio,
                        mes,
                        anio){
  error_dire <- "El valor de 'directorio' debe ser proporcionado.Ej: \"Documents/DANE/Procesos DIMPE /PilotoEMMET\"."

  if(missingArg(directorio)) stop(error_dire,call.=FALSE)

  if (!is.character(directorio)) {
    error_message <- sprintf("El valor de 'directorio' debe ser un caracter: %s no es válido. Ej:\"Documents/DANE/Procesos DIMPE /PilotoEMMET\".", directorio)
    stop(error_message,call. = FALSE)
  }
  error_anio<-"El valor de 'anio' debe ser numerico.Ej: 2022"
  if(missingArg(anio)) stop(error_anio,call.=FALSE)

  if (!is.numeric(anio)) {
    error_message <- sprintf("El valor de 'anio' debe ser numerico: %s no es válido.Ej: 2022", anio)
    stop(error_message,call. = FALSE)
  }
 error_mes<-"El valor de 'mes' debe ser numerico.Ej: 11"
  if(missingArg(mes)) stop(error_mes,call.=FALSE)

  if (!is.numeric(mes)) {
    error_message <- sprintf("El valor de 'mes' debe ser numerico: %s no es válido.Ej: 11", mes)
    stop(error_message,call. = FALSE)
  }

  # librerias ---------------------------------------------------------------


  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
  library(dplyr)
  source("https://raw.githubusercontent.com/NataliArteaga/DANE.EMMET/main/R/utils.R")




  # Cargar bases necesarias y aplicar funcion de formato de nombres

  base_logistica          <- read_csv(paste0(directorio,"/data/",anio,"/",meses[mes-1],"/EMMET_PANEL_imputada_",meses[mes-1],anio,".csv"))

  base_original           <- read_excel(paste0(directorio,"/data/",anio,"/",meses[mes],"/EMMET_original_",meses[mes],anio,".xlsx"))
  colnames(base_original) <- colnames_format(base_original)
  colnames(base_original)[!colnames(base_original)%in%colnames(base_logistica)] <- c("IMP_IMPUTACION","TOTAL_VENTAS","LOGISTICO_NUEVO","OBSERVACION_DE_AJUSTES")
  #cambia en las columnas que contienen la palabra "OBSE" cualquier caracter que no sea alfanumerico por un espacio
  base_original           <-  base_original %>%
    mutate_at(vars(contains("OBSE")),~str_replace_all(.,pattern="[^[:alnum:]]",replacement=" "))

  base_parametrica           <- read_excel(paste0(directorio,"/data/EMMET_parametrica.xlsx"),sheet = "Paramétrica")
  colnames(base_parametrica) <- colnames_format(base_parametrica)
  base_deflactor             <- read_excel(paste0(directorio,"/data/",anio,"/",meses[mes],"/DEFLACTOR_",meses[mes],anio,".xlsx"))
  colnames(base_deflactor)   <- colnames_format(base_deflactor)
  divipola                   <- read_excel(paste0(directorio,"/data/DIVIPOLA.xlsx"))
  colnames(divipola)         <- colnames_format(divipola)

  # Concatenar base Logistica con base Original ----------------------------------------------------

  base_panel <- rbind.data.frame(base_logistica,base_original)

  # Concatenar con Base Parametrica ---------------------------------------------
  base_panel <- base_panel %>%
    left_join(base_parametrica,by=c("ID_NUMORD"="ID_NUMORD"))

  # Concatenar con Base Deflactor ----------------------------------------------------
  base_panel <- base_panel %>%
    left_join(base_deflactor,by=c("EMMET_CLASE"="CIIU4","ANIO"="ANO","MES"="MES"))

  # Arreglos Base Panel (De Base Parametrica) -------------------------------

  #borrar variables que no son necesarias o estan repetidas
  drop <- names(base_panel) %in% c("NOVEDADEMMET","DOMINIO_39","NOMBRE_ESTAB.x")
  base_panel<-base_panel[,!drop]
  colnames(base_panel) <- gsub(".y","",colnames(base_panel),fixed = TRUE)

  # Estandarizacion nombres Departamento y Municipio ------------------------------------------------
  divipola <- divipola[!is.na(divipola$CODIGOMUNICIPIO),-dim(divipola)[2]]

  base_panel <- base_panel %>%
    left_join(divipola,by=c("ID_MUNICIPIO"="CODIGOMUNICIPIO"))

  drop <- names(base_panel) %in% c("NOMBREMPIO","NOMBREDEPTO","DESDEPTO")
  base_panel <- base_panel[,!drop]


  # Exportar bases de Datos integradas -------------------------------------------------

  write.csv(base_panel,paste0(directorio,"/results/S1_integracion/EMMET_PANEL_trabajo_original_",meses[mes],anio,".csv"),row.names=F)

}


