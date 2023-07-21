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
#' 1 S1_integracion:\code{\link{integracion}}
#'
#' 2 S2_estandarizacion:\code{\link{estandarizacion}}
#'
#' 3 S3_identificacion_alertas:\code{\link{identificacion_outliers}}
#'
#' 4 S4_imputacion:\code{\link{imputacion_outliers}}
#'
#' 5 S5_tematica:\code{\link{tematica}}
#'
#' 6 S6_anexos:\code{\link{anacional}} y \code{\link{aterritorial}}
#'
#' 7 S7_boletin:\code{\link{boletin}}
#'
#' @examples inicial(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET")


macro1<-function(directorio,mes,anio){
inicial(directorio)
  print("Se ejecuto la funcion inicial")
integracion(directorio,mes,anio)
print("Se ejecuto la funcion integracion")
estandarizacion(directorio,mes,anio)
print("Se ejecuto la funcion estandarizacion")
identificacion_outliers(directorio,mes,anio)
print("Se ejecuto la funcion identificacion_outliers, por favor verifique el archivo en la
      carpeta results/S3_identificacion_alertas")
print("Una vez listo el archivo continue el proceso llamando la función macro2(directorio,mes,anio) ")
}







guardar_con_confirmacion <- function(datos, archivo) {
  if (file.exists(archivo)) {
    respuesta <- readline(paste("El archivo", archivo, "ya existe. ¿Desea sobreescribirlo? (S/N): "))
    if (toupper(respuesta) != "S") {
      cat("Operación cancelada. El archivo no ha sido sobrescrito.\n")
      return(invisible())
    }
  }
  write.csv(datos, archivo, row.names = FALSE)
  cat("Los datos han sido guardados exitosamente en", archivo, "\n")
}

# Uso de la función
mis_datos <- data.frame(x = 1:11, y = letters[1:11])
archivo_guardado <- "datos.csv"
guardar_con_confirmacion(mis_datos, archivo_guardado)
