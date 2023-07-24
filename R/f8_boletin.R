f8_boletin <- function(directorio,
                    mes,
                    anio,
                    tipo="pdf"){
  library(rmarkdown)
  link="https://raw.githubusercontent.com/NataliArteaga/DANE.EMMET/main/boletin/boletin.Rmd"
  rmarkdown::render(link,paste0(tipo,"_document"),
                    params = list(month= mes,
                                  year= anio,
                                  fecha_publicacion= Sys.Date(),
                                  directorio= directorio),
                    output_file = paste0("boletin_",Sys.Date()),
                    output_dir = paste0(directorio,"/results/S7_boletin"))
}

