#' Title
#'
#'  @param mes Definir las tres primeras letras del mes a ejecutar, ej: Nov
#'  @param anio Definir el año a ejecutar, ej: 2022
#'  @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#'  @return CSV file
#'  @export
#'
#'  @examples integracion(directorio="/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes="nov",anio=2022)

integracion <- function(directorio,
                        mes,
                        anio){
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
  library(dplyr)
  library(stringr)
  meses <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
  month <- mes
  year  <- anio

  # Funcion formato nombres columnas
  colnames_format <- function(base){
    colnames(base) <- toupper(colnames(base))
    colnames(base) <- gsub(" ","",colnames(base))
    colnames(base) <- gsub("__","_",colnames(base))
    colnames(base) <- stringi::stri_trans_general(colnames(base), id = "Latin-ASCII")
    return(colnames(base))
  }

  # Cargar bases

  base_logistica          <- read_csv(paste0(directorio,"/data/",year,"/",meses[which(meses==month)-1],"/EMMET_PANEL_imputada_",meses[which(meses==month)-1],year,".csv"))

  base_original           <- read_excel(paste0(directorio,"/data/",year,"/",month,"/EMMET_original_",month,year,".xlsx"))
  colnames(base_original) <- colnames_format(base_original)
  colnames(base_original)[!colnames(base_original)%in%colnames(base_logistica)] <- c("IMP_IMPUTACION","TOTAL_VENTAS","LOGISTICO_NUEVO","OBSERVACION_DE_AJUSTES")
  base_original           <-  base_original %>%
    mutate_at(vars(contains("OBSE")),~str_replace_all(.,pattern="[^[:alnum:]]",replacement=" "))

  base_parametrica           <- read_excel(paste0(directorio,"/data/EMMET_parametrica.xlsx"),sheet = "Paramétrica")
  colnames(base_parametrica) <- colnames_format(base_parametrica)
  base_deflactor             <- read_excel(paste0(directorio,"/data/",year,"/",month,"/DEFLACTOR_",month,year,".xlsx"))
  colnames(base_deflactor)   <- colnames_format(base_deflactor)
  divipola                   <- read_excel(paste0(directorio,"/data/DIVIPOLA.xlsx"))
  colnames(divipola)         <- colnames_format(divipola)

  # Concatenar Original ----------------------------------------------------

  base_panel <- rbind.data.frame(base_logistica,base_original)

  # Concatenar Base Parametrica ---------------------------------------------
  base_panel <- base_panel %>%
    left_join(base_parametrica,by=c("ID_NUMORD"="ID_NUMORD"))

  # Concatenar Deflactor ----------------------------------------------------
  base_panel <- base_panel %>%
    left_join(base_deflactor,by=c("EMMET_CLASE"="CIIU4","ANIO"="ANO","MES"="MES"))

  # Arreglos Base Panel (De Base Parametrica) -------------------------------
  drop <- names(base_panel) %in% c("NOVEDADEMMET","DOMINIO_39","NOMBRE_ESTAB.x")
  base_panel<-base_panel[,!drop]
  colnames(base_panel) <- gsub(".y","",colnames(base_panel),fixed = TRUE)

  # Estandarizacion Dep, Mun ------------------------------------------------
  divipola <- divipola[!is.na(divipola$CODIGOMUNICIPIO),-dim(divipola)[2]]

  base_panel <- base_panel %>%
    left_join(divipola,by=c("ID_MUNICIPIO"="CODIGOMUNICIPIO"))

  drop <- names(base_panel) %in% c("NOMBREMPIO","NOMBREDEPTO","DESDEPTO")
  base_panel <- base_panel[,!drop]


  # Exportar bases de Datos -------------------------------------------------

  write.csv(base_panel,paste0(directorio,"/results/S1_integracion/EMMET_PANEL_trabajo_original_",month,year,".csv"),row.names=F)

}


