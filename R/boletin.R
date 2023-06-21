boletin <- function(directorio,
                    mes,
                    anio,
                    fecha,
                    guardar){
  library(rmarkdown)
  rmarkdown::render("/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /Functions/S4_boletin/boletin.Rmd", "pdf_document",
                    params = list(month= mes,
                                  year= anio,
                                  fecha_publicacion= fecha,
                                  directorio= directorio),
                    output_file = paste0("boletin_",Sys.Date()),
                    output_dir = guardar)
}

