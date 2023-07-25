#' Macro 2
#'
#' Con esta función realizaremos la segunda parte del proceso, en donde se correran las 5
#' funciones restantes, para finalmente optener el boletin
#'
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#' @details  Esta funcion tiene como objetivo correr desde la función de imputacion hasta la función
#' que crea el archivo de boletin, con el fin de que el usuario pueda continuar con el proceso
#' luego de haber realizado el proceso de critica al archivo de alertas, para imputar solo los
#' valores necesarios. Esta función le pedira una confirmación de que ya valido el archivo de
#' alertas, marque 1 para que la función corra, marque otro valor para cancelar.
#'
#' Ver:\code{\link{f4_imputacion}}, \code{\link{f5_tematica}}, \code{\link{f6_anacional}},
#' \code{\link{f7_aterritorial}}, \code{\link{f8_boletin}}
#'
#'
#' @examples macro2(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)


macro2<-function(directorio,mes,anio,fecha,guardar){
  respuesta <- readline(paste("Ingrese '1' si ya valido el archivo de alertas y desea continuar
                              con el proceso, otro valor si desea cancelar"))
  if (respuesta != 1) {
    cat("Operación cancelada. Valide el archivo de alertas y luego llame de nuevo la función .\n")
    return(invisible())
  }else{
  f4_imputacion(directorio,mes,anio)
  print("Se ejecuto la funcion imputacion")
  f5_tematica(directorio,mes,anio)
  print("Se ejecuto la funcion construccion base tematica")
  f6_anacional(directorio,mes,anio)
  print("Se ejecuto la funcion construccion anexo nacional")
  f7_aterritorial(directorio,mes,anio)
  print("Se ejecuto la funcion construccion anexo territorial")
  f8_boletin(directorio,mes,anio,tipo="pdf")
  print("Se ejecuto la funcion construccion  boletin")
  print(paste0("por favor consulte ",directorio,"/results encontrara los archivos para la
               publicacion de los resultados EMMET"))
  }
  }

### archivo en excel con los parametros del boletin
