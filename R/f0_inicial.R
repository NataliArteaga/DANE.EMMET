#' Función inicial
#'
#' Funcion que instala las librerias necesarias para todo el proceso y crea las carpetas en donde se guardaran los archivos.
#'

#' @param directorio definir el directorio donde se crearan las carpetas.
#'
#' @details Se revisa si las funciones están instaladas en el entorno, en caso de
#' que no estén instaladas se procederán a instalar, luego se procede a revisar
#' si están creadas las carpetas donde se guardarán los archivos creados con la librería
#' en total se crearán siete carpetas, para cada una de las funciones que genera algún archivo de salida.
#'
#' 1 S1_integracion:\code{\link{f1_integracion}}
#'
#' 2 S2_estandarizacion:\code{\link{f2_estandarizacion}}
#'
#' 3 S3_identificacion_alertas:\code{\link{f3_identificacion_alertas}}
#'
#' 4 S4_imputacion:\code{\link{f4_imputacion}}
#'
#' 5 S5_tematica:\code{\link{f5_tematica}}
#'
#' 6 S6_anexos:\code{\link{f6_anacional}} y \code{\link{f7_aterritorial}}
#'
#' 7 S7_boletin:\code{\link{f8_boletin}}
#'
#' @examples inicial(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET")


f0_inicial<-function(directorio){

#instalar todas las librerias necesarias para el proceso

  # Lista de librerías que deseas instalar o cargar
  librerias <- c("tidyverse", "ggplot2", "dplyr","readr","readxl","stringr","tidyr","lubridate","forecast","tsoutliers","data.table","VIM","scales","kableExtra","formattable","htmltools","webshot","openxlsx","seasonal","xlsx","rmarkdown","roxygen2","plotly")

  # Verificar si las librerías están instaladas
  librerias_faltantes <- librerias[!sapply(librerias, requireNamespace, quietly = TRUE)]

  # Instalar librerías faltantes
  if (length(librerias_faltantes) > 0) {
    install.packages(librerias_faltantes)
  }

  # Cargar todas las librerías
  lapply(librerias, require, character.only = TRUE)



#crear la función que revisa si la carpeta existe, de lo contrario la crea
crearCarpeta <- function(ruta) {

  if (!dir.exists(ruta)) {
    dir.create(ruta)
    mensaje <- paste("Se ha creado la carpeta:", ruta)
    print(mensaje)
  } else {
    mensaje <- paste("La carpeta", ruta, "ya existe.")
    print(mensaje)
  }
}
#crear la carpeta results
ruta=paste0(directorio,"/results")
crearCarpeta(ruta)

#crear la carpeta de integracion
ruta=paste0(directorio,"/results/S1_integracion")
crearCarpeta(ruta)

#crear la carpeta de estandarizacion
ruta=paste0(directorio,"/results/S2_estandarizacion")
crearCarpeta(ruta)

#crear la carpeta de alertas
ruta=paste0(directorio,"/results/S3_identificacion_alertas")
crearCarpeta(ruta)

#crear la carpeta de imputacion
ruta=paste0(directorio,"/results/S4_imputacion")
crearCarpeta(ruta)


#crear la carpeta de tematica
ruta=paste0(directorio,"/results/S5_tematica")
crearCarpeta(ruta)


#crear la carpeta anexos
ruta=paste0(directorio,"/results/S6_anexos")
crearCarpeta(ruta)

#crear la carpeta boletin
ruta=paste0(directorio,"/results/S7_boletin")
crearCarpeta(ruta)

}
