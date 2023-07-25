f8_boletin <- function(directorio, mes, anio, tipo = "pdf") {
  library(rmarkdown)

  # Definir la URL de la carpeta boletin en GitHub
  url_boletin <- "https://github.com/NataliArteaga/DANE.EMMET/raw/main/boletin.zip"

  # Descargar y descomprimir la carpeta boletin
  archivo_zip <- file.path(directorio, "boletin.zip")
  download.file(url_boletin, destfile = archivo_zip)
  unzip(archivo_zip, exdir = file.path(directorio, "boletin"))

  # Definir la ruta del archivo boletin.Rmd en la carpeta descargada
  ruta_boletin_rmd <- file.path(directorio, "boletin","boletin", "boletin.Rmd")

  # Renderizar el archivo Rmd localmente
  rmarkdown::render(ruta_boletin_rmd, paste0(tipo, "_document"),
                    params = list(month = mes,
                                  year = anio,
                                  fecha_publicacion = Sys.Date(),
                                  directorio = directorio),
                    output_file = paste0("boletin_", Sys.Date()),
                    output_dir = file.path(directorio, "results", "S7_boletin"))
}

