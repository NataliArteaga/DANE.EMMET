#' Macro 1
#'
#' Con esta función realizaremos la primera parte del proceso, en donde se correran las 4
#' primeras funciones.
#'
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#' @details  Esta funcion tiene como objetivo correr desde la función inicial hasta la función
#' de identificacion de alertas, con el fin de que el usuario pueda obtener rapidamente el
#' archivo de salida de la función de alertas y poder realizar el proceso de critica.
#' Recuerde que si el archivo de alertas ya está creado, al correr esta función le pedira que
#' responda S si quiere sobreescribirlo, de lo contrario otro valor para cancelar la función
#'
#' Ver:\code{\link{f0_inicial}},\code{\link{f1_integracion}}, \code{\link{f2_estandarizacion}},
#' \code{\link{f3_identificacion_outliers}}
#'
#'
#' @examples macro1(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)












f8_boletin <- function(directorio, mes, anio, tipo = "pdf") {
  library(rmarkdown)
  library(readxl)
  # Definir la URL de la carpeta boletin en GitHub
  url_boletin <- "https://github.com/NataliArteaga/DANE.EMMET/raw/main/boletin.zip"

  # Descargar y descomprimir la carpeta boletin
  archivo_zip <- file.path(directorio, "boletin.zip")
  download.file(url_boletin, destfile = archivo_zip)
  unzip(archivo_zip, exdir = file.path(directorio, "boletin"))

  # Definir la ruta del archivo boletin.Rmd en la carpeta descargada
  ruta_boletin_rmd <- file.path(directorio, "boletin","boletin", "boletin.Rmd")
  parametros <- as.data.frame(read_excel(paste0(directorio,"/results/S7_boletin/parametros_boletin.xlsx")))
  # Renderizar el archivo Rmd localmente
  rmarkdown::render(ruta_boletin_rmd, paste0(tipo, "_document"),
                    params = list(month = mes,
                                  year = anio,
                                  fecha_publicacion = Sys.Date(),
                                  directorio = directorio,
                                  IC_prod= parametros[1,2],
                                  IC_ven= parametros[2,2],
                                  IC_empl= parametros[3,2],
                                  TNR= parametros[4,2],
                                  TI_prod= parametros[5,2],
                                  TI_ven=parametros[6,2],
                                  TI_empl= parametros[7,2],
                                  ),
                    output_file = paste0("boletin_", Sys.Date()),
                    output_dir = file.path(directorio, "results", "S7_boletin"))
}

