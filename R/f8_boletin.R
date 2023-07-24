f8_boletin <- function(directorio,
                    mes,
                    anio){
  library(rmarkdown)
  rmarkdown::render("/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /Functions/S4_boletin/boletin.Rmd", "pdf_document",
                    params = list(month= mes,
                                  year= anio,
                                  fecha_publicacion= Sys.Date(),
                                  directorio= directorio),
                    output_file = paste0("boletin_",Sys.Date()),
                    output_dir = paste0(directorio,"/results/S7_boletin"))
}

