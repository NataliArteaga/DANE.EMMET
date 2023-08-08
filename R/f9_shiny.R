#' Shiny alertas
#'
#' @description Está función abre el navegador para abrir la aplicación de alertas, en la cual se puede loguear
#' y se encontraran 4 ventanas
#'
#' 1. identificación de alertas: la cual se puede filtrar por ID_NUMORD, mes y año; se encontrara una tabla con
#' los valores para las variables de capitulo 2 y 3 para los filtros aplicados, grafico de la serie para la variable
#' que se seleccione y finalmente los datos para el establecimiento en esa variable desde 2018 hasta la fecha.
#'
#' 2. Consolidado de alertas: la cual se puede filtrar por capitulo a la que pertenece la variable y el tipo de
#' imputación (imputación deuda o imputación caso especial) en el año y mes de interes, tiene el resgistro de todos
#' los establecimientos y variables en los que se identifico un posible caso de imputación.
#'
#' 3. Agregado variables: la cual se puede filtrar por año, mes, actividad economicama, departamento, área metropolitana
#' y ciudad
#'
#' 4. Comparación: la cual se puede filtrar por comparación (CIUU,DPTO,ciudad y área metropolitana) , año, mes, actividad economica
#' departamento, área metropolitana y ciudad...
#'
#' @param mes Definir el mes a ejecutar, ej: 11
#' @param anio Definir el año a ejecutar, ej: 2022
#' @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#'
#'
#' @examples f9_shiny(directorio"Documents/DANE/Procesos DIMPE /PilotoEMMET",mes = 11  ,anio =2022)





f9_shiny <- function(directorio, mes, anio) {
  library(rmarkdown)
  library(readxl)
  library(lubridate)
  library(shiny)
  source("https://raw.githubusercontent.com/NataliArteaga/DANE.EMMET/main/R/utils.R")
  mes=mes
  anio=anio


  url_boletin <- "https://github.com/NataliArteaga/DANE.EMMET/raw/main/Shiny_alertas.zip"

  # Descargar y descomprimir la carpeta shiny
  archivo_zip <- file.path(directorio, "Shiny_alertas.zip")
  download.file(url_boletin, destfile = archivo_zip)
  unzip(archivo_zip, exdir = file.path(directorio))

  ruta_app_shiny <- file.path(directorio,"Shiny_alertas/app_fun_critica_2.R")

  ruta_archivo_excel<-paste0(directorio,"/results/S5_tematica/EMMET_PANEL_tematica_",meses[mes],anio,".csv")

  #identificar si el archivo de tematica existe
  if (!file.exists(ruta_archivo_excel)) {
    # Si el archivo no está presente, ejecuta la función f4_imputacion y f5_tematica
    f4_imputacion(directorio,mes,anio)
    f5_tematica(directorio,mes,anio)
  } else {
  }

#función que corre la app
  shiny::runApp(
    appDir = ruta_app_shiny,
    launch.browser = TRUE  # Esto abrirá automáticamente el navegador para mostrar la aplicación
  )

  }
