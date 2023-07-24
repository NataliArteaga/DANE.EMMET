f8_boletin <- function(directorio,
                    mes,
                    anio,
                    tipo="pdf"){
  library(rmarkdown)
  rmarkdown::render(paste0(directorio,"boletin/boletin.Rmd"),paste0(tipo,"_document"),
                    params = list(month= mes,
                                  year= anio,
                                  fecha_publicacion= Sys.Date(),
                                  directorio= directorio),
                    output_file = paste0("boletin_",Sys.Date()),
                    output_dir = paste0(directorio,"/results/S7_boletin"))
}

