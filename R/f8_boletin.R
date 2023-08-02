#' Boletin
#'
#' @description Está función crea el boletín, un documento exportado en pdf o word, según se desea.
#'
#'  Este es un resumen ejecutivo en el que se presentan los
#'  principales resultados con el uso de cuadros de líneas para presentar
#'  información histórica y graficas de barras para hacer comparaciones de
#'  variaciones entre las distintas actividades o desagregación regional.
#'  Además, se presentan tablas con el resumen por actividad para el total
#'  nacional, departamentos, áreas metropolitanas y ciudades incluyendo
#'  las tres variables principales que permiten hacer un análisis cruzado
#'  entre estas.
#'
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#' @param tipo Definir si se quiere un archivo pdf, word, o html, por defecto esta la opción "pdf", ej: "html
#'

#'
#'
#' @examples f8_boletin(directorio"Documents/DANE/Procesos DIMPE /PilotoEMMET",mes = 11  ,anio =2022,tipo="word")












f8_boletin <- function(directorio, mes, anio, tipo = "pdf") {
  library(rmarkdown)
  library(readxl)
  library(lubridate)

  # Definir la URL de la carpeta boletin en GitHub
  url_boletin <- "https://github.com/NataliArteaga/DANE.EMMET/raw/main/boletin.zip"

  # Descargar y descomprimir la carpeta boletin
  archivo_zip <- file.path(directorio, "boletin.zip")
  download.file(url_boletin, destfile = archivo_zip)
  unzip(archivo_zip, exdir = file.path(directorio, "boletin"))

  # Definir la ruta del archivo boletin.Rmd en la carpeta descargada

  if (tipo == "word") {

    ruta_boletin_rmd <- file.path(directorio, "boletin","boletin", "boletin_versionW.Rmd")
  } else {
    ruta_boletin_rmd <- file.path(directorio, "boletin","boletin", "boletin_versionP.Rmd")
  }
  parametros <- as.data.frame(read_excel(paste0(directorio,"/results/S7_boletin/parametros_boletin.xlsx")))
  meses_b <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")


  # Creamos una función para obtener el nombre del mes en español
  nombre_mes_espanol <- function(fecha) {
    return(meses_b[month(fecha)])
  }

  # Formateamos la fecha en el formato deseado
  fecha_formateada1 <- paste(day(Sys.Date()), "de", nombre_mes_espanol(Sys.Date()), "de", year(Sys.Date()))
  fecha_formateada2 <- paste0(day(Sys.Date()), "de", nombre_mes_espanol(Sys.Date()), "de", year(Sys.Date()))




  # Renderizar el archivo Rmd localmente
  rmarkdown::render(ruta_boletin_rmd, output_format=paste0(tipo, "_document"),
                    params = list(month = mes,
                                  year = anio,
                                  month_b= meses_b[mes],
                                  fecha_publicacion = fecha_formateada1,
                                  directorio = directorio,
                                  IC_prod= parametros[1,2],
                                  IC_ven= parametros[2,2],
                                  IC_empl= parametros[3,2],
                                  TNR= parametros[4,2],
                                  TI_prod= parametros[5,2],
                                  TI_ven=parametros[6,2],
                                  TI_empl= parametros[7,2]
                                  ),
                    output_file = paste0("boletin_", fecha_formateada2),
                    output_dir = file.path(directorio, "results", "S7_boletin"))
}



