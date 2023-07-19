#' Anexo_Nacional
#'
#'  @param mes Definir las tres primeras letras del mes a ejecutar, ej: 11
#'  @param anio Definir el aC1o a ejecutar, ej: 2022
#'  @param directorio definir el directorio donde se encuentran ubicado los datos de entrada
#'
#'  @return CSV file
#'  @export
#'
#'  @examples anacional(directorio="/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /PilotoEMMET",
#'                        mes=11,anio=2022)
#'
#'  @description Esta funcion crea el archivo de Anexo Nacional. Tiene como argumentos, la base
#'  de datos Temetica, el mes y anio del que desea generar el reporte. EL cuerpo de la funcion
#'  crea cada una de las hojas del reporte, seleccionando las columnas a decuadas de la base tematica
#'  para la construcion de los diferentes calculos desagregados por las variables.
#'  Finalmente exporta un archivo con extension .xlsx en donde podra observar cada una de las
#'  hojas con las que cuenta el anexo.
#'
anacional <- function(directorio="/Users/nataliaarteaga/Documents/DANE/Procesos DIMPE /PilotoEMMET",
                      mes=11,
                      year=2022){
  # Cargar librerC-as --------------------------------------------------------
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(kableExtra)
  library(lubridate)
  library(formattable)
  library("htmltools")
  library("webshot")
  library(openxlsx)
  library(seasonal)
  library(stringr)
  library(xlsx)
  library(data.table)


  # Cargar bases y variables ------------------------------------------------

  meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
             "Agosto","Septiembre","Octubre","Noviembre","Diciembre")

  meses <- tolower(meses)
  data <- fread(paste0(directorio,"/results/S5_tematica/EMMET_PANEL_tematica_nov2022.csv"))
  indices_14<- read_xlsx(paste0(directorio,"/data/Indices_2014.xlsx"))
  indices_01<- read_xlsx(paste0(directorio,"/data/Indices_2001.xlsx"))


  # Archivos de entrada y salida --------------------------------------------

  formato<-paste0(directorio,"/data/",year,"/",substr(mes,1,3),"/anexos_nacional_emmet_",mes,"_formato.xlsx")
  Salida<-paste0(directorio,"/results/S6_anexonal/anexos_nacional_emmet_",mes,"_formato.xlsx")




  # Limpieza de nombres de variable -----------------------------------------

  colnames_format <- function(base){
    colnames(base) <- toupper(colnames(base))
    colnames(base) <- gsub(" ","",colnames(base))
    colnames(base) <- gsub("__","_",colnames(base))
    colnames(base) <- stringi::stri_trans_general(colnames(base), id = "Latin-ASCII")
    return(colnames(base))
  }
  colnames(data) <- colnames_format(data)
  data <-  data %>% mutate_at(vars(contains("OBSE")),~str_replace_all(.,pattern="[^[:alnum:]]",replacement=" "))

  colnames(indices_14)<-colnames_format(indices_14)
  colnames(indices_01)<-colnames_format(indices_01)

  # Se carga el formato de excel --------------------------------------------

  wb <- loadWorkbook(formato)
  sheets <- getSheets(wb)
  names(sheets)


  # Funcion -----------------------------------------------------------------


  contr_sum_an <- function(tabla){
    tabla1 <- tabla %>% summarise(produccion_total = sum(PRODUCCIONREALPOND),
                                  ventas_total=sum(VENTASREALESPOND),
                                  personal_total=sum(TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC))

    return(tabla1)
  }

  contr_fin <- function(periodo,tabla){
    if(periodo==6){
      contribucion <- tabla %>%
        summarise(prod = sum(PRODUCCIONREALPOND),
                  vent=sum(VENTASREALESPOND),
                  per=sum(PERSONAL)) %>%
        group_by(INCLUSION_NOMBRE_DEPTO)  %>%
        summarise(produccion=(prod[2]-prod[1])/contribucion_total$produccion_total,
                  ventas=(vent[2]-vent[1])/contribucion_total$ventas_total,
                  personal=(per[2]-per[1])/contribucion_total$personal_total) %>%
        arrange(produccion)

    }else{
      contribucion <- tabla %>%
        summarise(prod = sum(PRODUCCIONREALPOND),
                  vent=sum(VENTASREALESPOND),
                  per=sum(PERSONAL)) %>%
        group_by(DOMINIO_39,DESCRIPCIONDOMINIOEMMET39) %>%
        summarise(produccion=(prod[2]-prod[1])/contribucion_total$produccion_total,
                  ventas=(vent[2]-vent[1])/contribucion_total$ventas_total,
                  personal=(per[2]-per[1])/contribucion_total$personal_total) %>%
        arrange(produccion)
    }

    return(contribucion)
  }


  tabla_pivot <- function(periodo,tabla){
    if(periodo==5){
      tabla1 <- tabla %>%
        pivot_wider(names_from = c("ANIO2"),
                    values_from = c("produccionNom","produccion","ventasNom","ventas","personas"))
    }else {
      tabla1 <- tabla %>%
        pivot_wider(names_from = c("ANIO"),
                    values_from = c("produccionNom","produccion","ventasNom","ventas","personas"))
    }
    return(tabla1)
  }

  tabla_paste_an <- function(periodo,base){
    if(periodo==11){
      base[paste0("varprodnom_",year)] <- (base[paste0("produccionNom_",year)]-base[paste0("produccionNom_",2019)])/base[paste0("produccionNom_",2019)]
      base[paste0("varprod_",year)] <- (base[paste0("produccion_",year)]-base[paste0("produccion_",2019)])/base[paste0("produccion_",2019)]
      base[paste0("varventasnom_",year)]<- (base[paste0("ventasNom_",year)]-base[paste0("ventasNom_",2019)])/base[paste0("ventasNom_",2019)]
      base[paste0("varventas_",year)]<- (base[paste0("ventas_",year)]-base[paste0("ventas_",2019)])/base[paste0("ventas_",2019)]
      base[paste0("varpersonas_",year)] <- (base[paste0("personas_",year)]-base[paste0("personas_",2019)])/base[paste0("personas_",2019)]
    }else if(periodo==2 | periodo==4 | periodo==6){
      base[paste0("varprod_",year)] <- (base[paste0("produccion_",year)]-base[paste0("produccion_",year-1)])/base[paste0("produccion_",year-1)]
      base[paste0("varventas_",year)]<- (base[paste0("ventas_",year)]-base[paste0("ventas_",year-1)])/base[paste0("ventas_",year-1)]
      base[paste0("varpersonas_",year)] <- (base[paste0("personas_",year)]-base[paste0("personas_",year-1)])/base[paste0("personas_",year-1)]
      base[paste0("varempleo_",year)] <- (base[paste0("empleo_",year)]-base[paste0("empleo_",year-1)])/base[paste0("empleo_",year-1)]
      base[paste0("varemptem_",year)] <- (base[paste0("emptem_",year)]-base[paste0("emptem_",year-1)])/base[paste0("emptem_",year-1)]
      base[paste0("varempleados_",year)] <- (base[paste0("empleados_",year)]-base[paste0("empleados_",year-1)])/base[paste0("empleados_",year-1)]
      base[paste0("varoperarios_",year)] <- (base[paste0("operarios_",year)]-base[paste0("operarios_",year-1)])/base[paste0("operarios_",year-1)]
      base[paste0("varsueldos_",year)] <- (base[paste0("sueldos_",year)]-base[paste0("sueldos_",year-1)])/base[paste0("sueldos_",year-1)]
      base[paste0("varsuelemplper_",year)] <- (base[paste0("suelemplper_",year)]-base[paste0("suelemplper_",year-1)])/base[paste0("suelemplper_",year-1)]
      base[paste0("varsuelempltem_",year)] <- (base[paste0("suelempltem_",year)]-base[paste0("suelempltem_",year-1)])/base[paste0("suelempltem_",year-1)]
      base[paste0("varsueltotemp_",year)] <- (base[paste0("sueltotemp_",year)]-base[paste0("sueltotemp_",year-1)])/base[paste0("sueltotemp_",year-1)]
      base[paste0("varsueltotope_",year)] <- (base[paste0("sueltotope_",year)]-base[paste0("sueltotope_",year-1)])/base[paste0("sueltotope_",year-1)]
      base[paste0("varhoras_",year)] <- (base[paste0("horas_",year)]-base[paste0("horas_",year-1)])/base[paste0("horas_",year-1)]
    }else{
      base[paste0("varprodnom_",year)] <- (base[paste0("produccionNom_",year)]-base[paste0("produccionNom_",year-1)])/base[paste0("produccionNom_",year-1)]
      base[paste0("varprod_",year)] <- (base[paste0("produccion_",year)]-base[paste0("produccion_",year-1)])/base[paste0("produccion_",year-1)]
      base[paste0("varventasnom_",year)]<- (base[paste0("ventasNom_",year)]-base[paste0("ventasNom_",year-1)])/base[paste0("ventasNom_",year-1)]
      base[paste0("varventas_",year)]<- (base[paste0("ventas_",year)]-base[paste0("ventas_",year-1)])/base[paste0("ventas_",year-1)]
      base[paste0("varpersonas_",year)] <- (base[paste0("personas_",year)]-base[paste0("personas_",year-1)])/base[paste0("personas_",year-1)]
    }

    return(base)
  }


  tabla_summarise <- function(periodo,tabla){
    if(periodo==2 | periodo==4 | periodo==6){
      tabla1 <- tabla  %>%
        summarise(produccion=sum(PRODUCCIONREALPOND),
                  ventas = sum(VENTASREALESPOND),
                  personas=sum(TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC),
                  empleo=sum(TOTALEMPLEOPERMANENTE),
                  emptem=sum(TOTALEMPLEOTEMPORAL),
                  empleados=sum(TOTALEMPLEOADMON),
                  operarios=sum(TOTALEMPLEOPRODUC),
                  sueldos=sum(TOTALSUELDOSREALES),
                  suelemplper=sum(SUELDOSPERMANENTESREALES),
                  suelempltem=sum(SUELDOSTEMPORALESREALES),
                  sueltotemp=sum(SUELDOSADMONREAL),
                  sueltotope=sum(SUELDOSPRODUCREAL),
                  horas=sum(TOTALHORAS))

    }else{
      tabla1 <- tabla  %>%
        summarise(produccionNom=sum(PRODUCCIONNOMPOND),
                  produccion=sum(PRODUCCIONREALPOND),
                  ventasNom = sum(VENTASNOMINPOND),
                  ventas = sum(VENTASREALESPOND),
                  personas=sum(TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC))
    }
    return(tabla1)

  }

  tabla_acople<-function(tabla){
    tabla$ANIO <- sapply(strsplit(as.character(tabla$variables), "_"), `[`, 2)
    tabla$variables <- sapply(strsplit(as.character(tabla$variables), "_"), `[`, 1)
    tabla <- tabla %>% filter(gsub("var","",variables)!=variables )

    tabla <- tabla %>% pivot_wider(names_from = variables,values_from = value)

    return(tabla)
  }


  # 1. Var y cont_Anual -----------------------------------------------------

  contribucion_total <- data %>%
    filter(MES==mes & ANIO%in%c(year-1))
  contribucion_total <- contr_sum_an(contribucion_total)

  contribucion <- data %>%
    filter(MES==mes & ANIO%in%c(year,year-1)) %>%
    mutate(PERSONAL=TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC) %>%
    group_by(ANIO,MES,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  contribucion <- contr_fin(1,contribucion)


  tabla1 <- data %>%
    filter(ANIO%in%c(year,year-1) & MES%in%mes) %>%
    group_by(ANIO,MES,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  tabla1 <- tabla_summarise(1,tabla1)


  tabla1 <- tabla_pivot(1,tabla1)

  tabla1 <- tabla_paste_an(1,tabla1)

  tabla1 <- tabla1 %>% pivot_longer(cols = colnames(tabla1)[-c(1:3)],names_to = "variables",values_to = "value" )

  tabla1 <- tabla_acople(tabla1)

  tabla1 <- inner_join(x=tabla1,y=contribucion,by=c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39"))
  tabla1 <- tabla1[,c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39","varprodnom",
                      "varprod","produccion","varventasnom","varventas",
                      "ventas","varpersonas","personal")]

  for( i in c("varprodnom","varprod","produccion","varventasnom",
              "varventas","ventas","varpersonas","personal")){
    tabla1[,i] <-  round(tabla1[,i]*100,1)
  }
  tabla1 <- tabla1 %>% arrange(DOMINIO_39)

  #Exportar

  sheet <- sheets[[2]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 14, startColumn = 1)

  Enunciado<-paste0(meses[mes],"(",year,"/",year-1,")")
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 9, startColumn = 1)

  # 2. Var Anual_Emp_Sueldos_Horas  -----------------------------------------

  tabla1 <- data %>%
    filter(ANIO%in%c(year,year-1) & MES%in%mes) %>%
    group_by(ANIO,MES,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  tabla1 <- tabla_summarise(2,tabla1)

  tabla1 <- tabla1 %>%
    pivot_wider(names_from = c("ANIO"),
                values_from = c("produccion","ventas","personas","empleo","emptem",
                                "empleados","operarios","sueldos","suelemplper","suelempltem","sueltotemp",
                                "sueltotope","horas"))

  tabla1 <- tabla_paste_an(2,tabla1)

  tabla1 <- tabla1 %>% pivot_longer(cols = colnames(tabla1)[-c(1:3)],names_to = "variables",values_to = "value" )


  tabla1 <- tabla_acople(tabla1)

  tabla1 <- inner_join(x=tabla1,y=contribucion,by=c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39"))
  tabla1 <- tabla1[,c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39","varprod",
                      "varventas","varpersonas","varempleo","varemptem",
                      "varempleados","varoperarios","varsueldos","varsuelemplper",
                      "varsuelempltem","varsueltotemp","varsueltotope","varhoras")]

  for( i in c("varprod","varventas","varpersonas","varempleo","varemptem",
              "varempleados","varoperarios","varsueldos","varsuelemplper",
              "varsuelempltem","varsueltotemp","varsueltotope","varhoras")){
    tabla1[,i] <-  round(tabla1[,i]*100,1)
  }

  #Exportar

  sheet <- sheets[[3]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 15, startColumn = 1)

  Enunciado<-paste0(meses[mes],"(",year,"/",year-1,")")
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 9, startColumn = 1)


  # 3. Var y cont_aC1o corrido -----------------------------------------------

  contribucion_total <- data %>%
    filter(MES%in%c(1:mes) & ANIO%in%c(year-1))
  contribucion_total <- contr_sum_an(contribucion_total)

  contribucion <- data %>%
    filter(MES%in%c(1:mes) & ANIO%in%c(year,year-1)) %>%
    mutate(PERSONAL=TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC) %>%
    group_by(ANIO,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  contribucion <- contr_fin(3,contribucion)

  tabla1 <- data %>%
    filter(ANIO%in%c(year,year-1) & MES%in%c(1:mes)) %>%
    group_by(ANIO,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  tabla1 <- tabla_summarise(3,tabla1)

  tabla1 <- tabla_pivot(3,tabla1)

  tabla1 <- tabla_paste_an(3,tabla1)

  tabla1 <- tabla1 %>% pivot_longer(cols = colnames(tabla1)[-c(1:2)],names_to = "variables",values_to = "value" )

  tabla1 <- tabla_acople(tabla1)

  tabla1 <- inner_join(x=tabla1,y=contribucion,by=c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39"))
  tabla1 <- tabla1[,c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39","varprodnom","varprod","produccion",
                      "varventasnom","varventas","ventas","varpersonas","personal")]

  for( i in c("varprodnom","varprod","produccion",
              "varventasnom","varventas","ventas","varpersonas","personal")){
    tabla1[,i] <-  round(tabla1[,i]*100,1)
  }

  #Exportar

  sheet <- sheets[[5]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 14, startColumn = 1)

  Enunciado<-paste0(meses[mes],"(",year,"/",year-1,")")
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 9, startColumn = 1)


  # 4. Var aC1o corr Emp_Sueldos_Hor -----------------------------------------

  tabla1 <- data %>%
    filter(ANIO%in%c(year,year-1) & MES%in%c(1:mes)) %>%
    group_by(ANIO,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  tabla1 <- tabla_summarise(4,tabla1)


  tabla1 <- tabla1 %>%
    pivot_wider(names_from = c("ANIO"),values_from = c("produccion","ventas","personas","empleo","emptem",
                                                       "empleados","operarios","sueldos","suelemplper",
                                                       "suelempltem","sueltotemp","sueltotope","horas"))

  tabla1 <- tabla_paste_an(4,tabla1)

  tabla1 <- tabla1 %>% pivot_longer(cols = colnames(tabla1)[-c(1:3)],names_to = "variables",values_to = "value" )

  tabla1 <- tabla_acople(tabla1)

  tabla1 <- tabla1[,c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39","varprod",
                      "varventas","varpersonas","varempleo","varemptem",
                      "varempleados","varoperarios","varsueldos","varsuelemplper",
                      "varsuelempltem","varsueltotemp","varsueltotope","varhoras")]

  for( i in c("varprod","varventas","varpersonas","varempleo","varemptem",
              "varempleados","varoperarios","varsueldos","varsuelemplper",
              "varsuelempltem","varsueltotemp","varsueltotope","varhoras")){
    tabla1[,i] <-  round(tabla1[,i]*100,1)
  }

  #Exportar
  sheet <- sheets[[6]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 15, startColumn = 1)


  Enunciado<-paste0(meses[1],"-",meses[mes],"(",year,"/",year-1,")")
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 9, startColumn = 1)

  # 5. Var y cont_doce meses ------------------------------------------------

  data$ANIO2 <- as.numeric(ifelse(data$MES%in%c((mes+1):12),data$ANIO+1,data$ANIO))

  contribucion_total <- data %>%
    filter(ANIO2%in%(year-1))
  contribucion_total <- contr_sum_an(contribucion_total)

  contribucion <- data %>%
    filter(ANIO2%in%c(year-1,year)) %>%
    mutate(PERSONAL=TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC) %>%
    group_by(ANIO2,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  contribucion <- contr_fin(5,contribucion)


  tabla1 <- data %>%
    filter(ANIO2%in%c(year,year-1)) %>%
    group_by(ANIO2,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  tabla1 <- tabla_summarise(5,tabla1)

  tabla1 <- tabla_pivot(5,tabla1)

  tabla1 <- tabla_paste_an(5,tabla1)

  tabla1 <- tabla1 %>% pivot_longer(cols = colnames(tabla1)[-c(1:3)],names_to = "variables",values_to = "value" )

  tabla1 <- tabla_acople(tabla1)

  tabla1 <- inner_join(x=tabla1,y=contribucion,by=c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39"))
  tabla1 <- tabla1[,c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39","varprodnom",
                      "varprod","produccion","varventasnom","varventas","ventas",
                      "varpersonas","personal")]

  for( i in c("varprodnom",
              "varprod","produccion","varventasnom","varventas","ventas",
              "varpersonas","personal")){
    tabla1[,i] <-  round(tabla1[,i]*100,1)
  }

  #Exportar

  sheet <- sheets[[7]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 14, startColumn = 1)



  Enunciado<-paste0(meses[12]," ",year-1,"-",meses[mes]," ",year,"/",meses[12]," ",year-2,"-",meses[mes]," ",year-1)
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 9, startColumn = 1)


  # 6. Var doce meses Emp_Sueldos_H -----------------------------------------

  data$ANIO2 <- as.numeric(ifelse(data$MES%in%c((mes+1):12),data$ANIO+1,data$ANIO))

  contribucion_total <- data %>%
    filter(ANIO2%in%(year-1))
  contribucion_total <- contr_sum_an(contribucion_total)

  contribucion <- data %>%
    filter(ANIO2%in%c(year-1,year)) %>%
    mutate(PERSONAL=TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC) %>%
    group_by(ANIO2,INCLUSION_NOMBRE_DEPTO)
  contribucion <- contr_fin(6,contribucion)

  tabla1 <- data %>%
    filter(ANIO2%in%c(year,year-1)) %>%
    group_by(ANIO2,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  tabla1 <- tabla_summarise(6,tabla1)

  tabla1_6 <- data %>%
    filter(ANIO2%in%c(year,year-1)) %>%
    group_by(ANIO2,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39) %>%
    summarise(produccionNom=sum(PRODUCCIONNOMPOND),
              ventasNom = sum(VENTASNOMINPOND))

  tabla1 <- tabla1 %>% left_join(tabla1_6,by=c("ANIO2"="ANIO2","DOMINIO_39"="DOMINIO_39",
                                               "DESCRIPCIONDOMINIOEMMET39"="DESCRIPCIONDOMINIOEMMET39"))

  tabla1 <- tabla1 %>%
    pivot_wider(names_from = c("ANIO2"),
                values_from = c("produccionNom","produccion","ventas","ventasNom",
                                "personas","empleo","emptem","empleados","operarios",
                                "sueldos","suelemplper","suelempltem","sueltotemp",
                                "sueltotope","horas"))


  tabla1 <- tabla_paste_an(6,tabla1)

  tabla1 <- tabla1 %>% pivot_longer(cols = colnames(tabla1)[-c(1:2)],names_to = "variables",values_to = "value" )


  tabla1 <- tabla_acople(tabla1)



  tabla1 <- tabla1[,c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39","varprod",
                      "varventas","varpersonas","varempleo","varemptem",
                      "varempleados","varoperarios","varsueldos","varsuelemplper",
                      "varsuelempltem","varsueltotemp","varsueltotope","varhoras")]

  for( i in c("varprod","varventas","varpersonas","varempleo","varemptem",
              "varempleados","varoperarios","varsueldos","varsuelemplper",
              "varsuelempltem","varsueltotemp","varsueltotope","varhoras")){
    tabla1[,i] <-  round(tabla1[,i]*100,1)
  }


  #Exportar

  sheet <- sheets[[8]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 15, startColumn = 1)


  Enunciado<-paste0(meses[12]," ",year-1,"-",meses[mes]," ",year,"/",meses[12]," ",year-2,"-",meses[mes]," ",year-1)
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 9, startColumn = 1)



  # 7. Indices total por clase  ---------------------------------------------

  contribucion_mensual <- data %>%
    group_by(ANIO,MES,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39) %>%
    summarise(produccionNom_mensual=sum(PRODUCCIONNOMPOND),
              produccion_mensual=sum(PRODUCCIONREALPOND),
              ventasNom_mensual = sum(VENTASNOMINPOND),
              ventas_mensual = sum(VENTASREALESPOND),
              personas_mensual=sum(TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC),
              empleo_mensual=sum(TOTALEMPLEOPERMANENTE),
              emptem_mensual=sum(TOTALEMPLEOTEMPORAL),
              empleados_mensual=sum(TOTALEMPLEOADMON),
              operarios_mensual=sum(TOTALEMPLEOPRODUC),
              sueldos_mensual=sum(TOTALSUELDOSREALES),
              suelemplper_mensual=sum(SUELDOSPERMANENTESREALES),
              suelempltem_mensual=sum(SUELDOSTEMPORALESREALES),
              sueltotemp_mensual=sum(SUELDOSADMONREAL),
              sueltotope_mensual=sum(SUELDOSPRODUCREAL),
              horas_mensual=sum(TOTALHORAS))


  contribucion_base<- contribucion_mensual %>%
    filter(ANIO==2018) %>%
    group_by(ANIO,DOMINIO_39) %>%
    summarise(produccionNom_total=mean(produccionNom_mensual),
              produccion_total=mean(produccion_mensual),
              ventasNom_total = mean(ventasNom_mensual),
              ventas_total = mean(ventas_mensual),
              personas_total=mean(personas_mensual),
              empleo_total=mean(empleo_mensual),
              emptem_total=mean(emptem_mensual),
              empleados_total=mean(empleados_mensual),
              operarios_total=mean(operarios_mensual),
              sueldos_total=mean(sueldos_mensual),
              suelemplper_total=mean(suelemplper_mensual),
              suelempltem_total=mean(suelempltem_mensual),
              sueltotemp_total=mean(sueltotemp_mensual),
              sueltotope_total=mean(sueltotope_mensual),
              horas_total=mean(horas_mensual))


  contribucion_base<-subset(contribucion_base, select = -ANIO)


  contribucion<-contribucion_mensual %>%
    left_join(contribucion_base,by=c("DOMINIO_39"="DOMINIO_39"))

  tabla1<-contribucion %>%
    mutate(produccionNom =round((produccionNom_mensual/produccionNom_total)*100,1),
           produccion    =round((produccion_mensual/produccion_total)*100,1),
           ventasNom     =round((ventasNom_mensual/ventasNom_total)*100,1),
           ventas        =round((ventas_mensual/ventas_total)*100,1),
           personas      =round((personas_mensual/personas_total)*100,1),
           empleo        =round((empleo_mensual/empleo_total)*100,1),
           emptem        =round((emptem_mensual/emptem_total)*100,1),
           empleados     =round((empleados_mensual/empleados_total)*100,1),
           operarios     =round((operarios_mensual/operarios_total)*100,1),
           sueldos       =round((sueldos_mensual/sueldos_total)*100,1),
           suelemplper   =round((suelemplper_mensual/suelemplper_total)*100,1),
           suelempltem   =round((suelempltem_mensual/suelempltem_total)*100,1),
           sueltotemp    =round((sueltotemp_mensual/sueltotemp_total)*100,1),
           sueltotope    =round((sueltotope_mensual/sueltotope_total)*100,1),
           horas         =round((horas_mensual/horas_total)*100,1))  %>%
    select(DOMINIO_39,ANIO,MES,DESCRIPCIONDOMINIOEMMET39,produccionNom,
           produccion,ventasNom,ventas,personas,empleo,emptem,empleados,
           operarios,sueldos,suelemplper,suelempltem,sueltotemp,sueltotope,
           horas)

  contribucion_mensual <- data %>%
    group_by(ANIO,MES) %>%
    summarise(produccionNom_mensual=sum(PRODUCCIONNOMPOND),
              produccion_mensual=sum(PRODUCCIONREALPOND),
              ventasNom_mensual = sum(VENTASNOMINPOND),
              ventas_mensual = sum(VENTASREALESPOND),
              personas_mensual=sum(TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC),
              empleo_mensual=sum(TOTALEMPLEOPERMANENTE),
              emptem_mensual=sum(TOTALEMPLEOTEMPORAL),
              empleados_mensual=sum(TOTALEMPLEOADMON),
              operarios_mensual=sum(TOTALEMPLEOPRODUC),
              sueldos_mensual=sum(TOTALSUELDOSREALES),
              suelemplper_mensual=sum(SUELDOSPERMANENTESREALES),
              suelempltem_mensual=sum(SUELDOSTEMPORALESREALES),
              sueltotemp_mensual=sum(SUELDOSADMONREAL),
              sueltotope_mensual=sum(SUELDOSPRODUCREAL),
              horas_mensual=sum(TOTALHORAS))

  contribucion_base <- contribucion_mensual %>%
    filter(ANIO==2018) %>%
    group_by(ANIO) %>%
    summarise(produccionNom_total=mean(produccionNom_mensual),
              produccion_total=mean(produccion_mensual),
              ventasNom_total= mean(ventasNom_mensual),
              ventas_total= mean(ventas_mensual),
              personas_total=mean(personas_mensual),
              empleo_total=mean(empleo_mensual),
              emptem_total=mean(emptem_mensual),
              empleados_total=mean(empleados_mensual),
              operarios_total=mean(operarios_mensual),
              sueldos_total=mean(sueldos_mensual),
              suelemplper_total=mean(suelemplper_mensual),
              suelempltem_total=mean(suelempltem_mensual),
              sueltotemp_total=mean(sueltotemp_mensual),
              sueltotope_total=mean(sueltotope_mensual),
              horas_total=mean(horas_mensual))

  contribucion_base<-subset(contribucion_base, select = -ANIO)

  contribucion_mensual<-contribucion_mensual %>%
    mutate(produccionNom_total=contribucion_base$produccionNom_total,
           produccion_total=contribucion_base$produccion_total,
           ventasNom_total=contribucion_base$ventasNom_total,
           ventas_total=contribucion_base$ventas_total,
           personas_total=contribucion_base$personas_total,
           empleo_total=contribucion_base$empleo_total,
           emptem_total=contribucion_base$emptem_total,
           empleados_total=contribucion_base$empleados_total,
           operarios_total=contribucion_base$operarios_total,
           sueldos_total=contribucion_base$sueldos_total,
           suelemplper_total=contribucion_base$suelemplper_total,
           suelempltem_total=contribucion_base$suelempltem_total,
           sueltotemp_total=contribucion_base$sueltotemp_total,
           sueltotope_total=contribucion_base$sueltotope_total,
           horas_total=contribucion_base$horas_total)


  tabla1_1<-contribucion_mensual %>%
    mutate(produccionNom =round((produccionNom_mensual/produccionNom_total)*100,1),
           produccion    =round((produccion_mensual/produccion_total)*100,1),
           ventasNom     =round((ventasNom_mensual/ventasNom_total)*100,1),
           ventas        =round((ventas_mensual/ventas_total)*100,1),
           personas      =round((personas_mensual/personas_total)*100,1),
           empleo        =round((empleo_mensual/empleo_total)*100,1),
           emptem        =round((emptem_mensual/emptem_total)*100,1),
           empleados     =round((empleados_mensual/empleados_total)*100,1),
           operarios     =round((operarios_mensual/operarios_total)*100,1),
           sueldos       =round((sueldos_mensual/sueldos_total)*100,1),
           suelemplper   =round((suelemplper_mensual/suelemplper_total)*100,1),
           suelempltem   =round((suelempltem_mensual/suelempltem_total)*100,1),
           sueltotemp    =round((sueltotemp_mensual/sueltotemp_total)*100,1),
           sueltotope    =round((sueltotope_mensual/sueltotope_total)*100,1),
           horas         =round((horas_mensual/horas_total)*100,1))  %>%
    select(ANIO,MES,produccionNom,produccion,ventasNom,ventas,personas,empleo,emptem,empleados,
           operarios,sueldos,suelemplper,suelempltem,sueltotemp,sueltotope,
           horas)
  tabla1_1["DOMINIO_39"]<-"T_IND"
  tabla1_1["DESCRIPCIONDOMINIOEMMET39"]<-"Total Industria"
  tabla1$DOMINIO_39<-as.character(tabla1$DOMINIO_39)
  tabla1<-rbind(tabla1,tabla1_1)
  tabla1_7<-tabla1 %>% arrange(DOMINIO_39,ANIO,MES)

  #Exportar

  sheet <- sheets[[9]]
  addDataFrame(data.frame(tabla1_7), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 12, startColumn = 1)


  Enunciado<-paste0(meses[1]," ",year-4,"-",meses[mes]," ",year)
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE, row.names=FALSE,
               startRow = 7, startColumn = 1)


  # 9. Enlace legal hasta 2014 ----------------------------------------------

  N<-as.data.frame(matrix(NA,ncol = ncol(indices_14),nrow = 12),colnames=FALSE)
  colnames(N)<-colnames(indices_14)

  in_prue<-NULL
  for(i in seq(from=60, to = nrow(indices_14), by =60)){
    if(i==60){
      in_prue<-rbind(indices_14[1:i,],N)
    }
    else{
      in_prue<-rbind(in_prue,indices_14[(i-59):i,],N)
    }

  }

  indices_lag<-rbind(in_prue[13:nrow(in_prue),5:ncol(in_prue)],N[,5:ncol(N)])
  colnames(indices_lag) <- paste("R", colnames(indices_lag), sep = "")
  indices_lag<-cbind(in_prue,indices_lag)
  indices_lag<-indices_lag[-c((nrow(indices_lag)-11):nrow(indices_lag)),]

  variacion<-within(indices_lag,{
    VHORASTOTALESTRABAJADAS=HORASTOTALESTRABAJADAS/RHORASTOTALESTRABAJADAS
    VSUELDOTOTALOPERARIOS  =SUELDOTOTALOPERARIOS/RSUELDOTOTALOPERARIOS
    VSUELDOTOTALEMPLEADOS  =SUELDOTOTALEMPLEADOS/RSUELDOTOTALEMPLEADOS
    VSUELDOEMPLEOTEMPORAL  =SUELDOEMPLEOTEMPORAL/RSUELDOEMPLEOTEMPORAL
    VSUELDOEMPLEOPERMANENTE=SUELDOEMPLEOPERMANENTE/RSUELDOEMPLEOPERMANENTE
    VTOTALSUELDOS      =TOTALSUELDOS/RTOTALSUELDOS
    VTOTALOPERARIOS    =TOTALOPERARIOS/RTOTALOPERARIOS
    VTOTALEMPLEADOS    =TOTALEMPLEADOS/RTOTALEMPLEADOS
    VPERSONALTEMPORAL  =PERSONALTEMPORAL/RPERSONALTEMPORAL
    VPERSONALPERMANENTE=PERSONALPERMANENTE/RPERSONALPERMANENTE
    VEMPLEOTOTAL       =EMPLEOTOTAL/REMPLEOTOTAL
    VVENTASREALES      =VENTASREALES/RVENTASREALES
    VVENTASNOMINALES   =VENTASNOMINALES/RVENTASNOMINALES
    VPRODUCCIONREAL    =PRODUCCIONREAL/RPRODUCCIONREAL
    VPRODUCCIONNOMINAL =PRODUCCIONNOMINAL/RPRODUCCIONNOMINAL
  })

  variacion <- variacion[!is.na(variacion$DOMINIOS),]

  tabla2<-tabla1_7 %>%
    filter(ANIO==2018)
  tabla2<-as.data.frame(tabla2)
  colnames(tabla2)<-paste("I", colnames(variacion[,1:ncol(tabla2)]), sep = "")
  tabla2 <- select(tabla2, -"ICLASESINDUSTRIALES" )
  tabla2$IANO<-as.numeric(tabla2$IANO)

  varia<-variacion %>% left_join(tabla2,by=c("DOMINIOS"="IDOMINIOS",
                                             "ANO"="IANO","MES"="IMES"))


  varia<-varia %>%
    arrange(DOMINIOS,desc(ANO),desc(MES))

  vector<-unique(varia$DOMINIOS)
  variac<-NULL
  for (k in vector) {
    vari<-varia %>%
      filter(DOMINIOS==k)
    for (j in 50:ncol(varia)) {
      for(i in 1:48){
        vari[(i+12),j]=(vari[(i+12),(j-15)]*vari[i,j])
      }
    }
    variac<-rbind(variac,vari)
  }


  cols_i <- grep("^I", names(variac), value = TRUE)
  variac<- select(variac,c(DOMINIOS,ANO,MES,CLASESINDUSTRIALES,cols_i))
  colnames(variac)<-gsub("I","",names(variac))
  tabla1<-tabla1 %>% filter(ANIO!=2018)
  colnames(tabla1)<-colnames(variac)
  tabla1<-rbind(variac,tabla1)
  tabla1<-tabla1 %>% arrange(DOMNOS,ANO,MES)

  #Exportar

  sheet <- sheets[[12]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE,
               row.names=FALSE, startRow = 14, startColumn = 1)


  Enunciado<-paste0(meses[mes],"(",year,"/",year-1,")")
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE,
               row.names=FALSE, startRow = 9, startColumn = 1)



  # 10. Enlace legal Hasta 2001 ---------------------------------------------

  data2<-data %>%
    filter(CLASE_CIIU4!=1922) %>%
    mutate(DOMINIO_39_ENLACE=
             ifelse(DOMINIO_39 %in% c(1050,1090), 1050,
                    ifelse(DOMINIO_39 %in% c(1089,1030,1082),1089,
                           ifelse(DOMINIO_39 %in% c(1300,1400),1300,
                                  ifelse(DOMINIO_39 %in% c(2020,2023,2100),2020,DOMINIO_39))))
    ) %>%
    mutate(DESCRIPCIONDOMINIOEMMET39_ENLACE=
             ifelse(
               DOMINIO_39_ENLACE==1050, "ElaboraciC3n de productos de molinerC-a",
               ifelse(DOMINIO_39_ENLACE==1089 , "Resto de alimentos",
                      ifelse(DOMINIO_39_ENLACE==1400 , "ConfecciC3n de prendas de vestir",
                             ifelse(DOMINIO_39_ENLACE==2020 , "FabricaciC3n de otros productos quC-micos",
                                    ifelse(DOMINIO_39_ENLACE==1900 , "RefinaciC3n de petrC3leo",DESCRIPCIONDOMINIOEMMET39))))))



  contribucion_mensual <- data2 %>%
    group_by(ANIO,MES,DOMINIO_39_ENLACE,DESCRIPCIONDOMINIOEMMET39_ENLACE) %>%
    summarise(produccionNom_mensual=sum(PRODUCCIONNOMPOND),
              produccion_mensual=sum(PRODUCCIONREALPOND),
              ventasNom_mensual = sum(VENTASNOMINPOND),
              ventas_mensual = sum(VENTASREALESPOND),
              personas_mensual=sum(TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC),
              personal_admon=sum(TOTALEMPLEOADMON),
              personal_operario=sum(TOTALEMPLEOPRODUC))

  contribucion_base<- contribucion_mensual  %>%
    filter(ANIO==2018) %>%
    group_by(ANIO,DOMINIO_39_ENLACE) %>%
    summarise(produccionNom_total=mean(produccionNom_mensual),
              produccion_total=mean(produccion_mensual),
              ventasNom_total = mean(ventasNom_mensual),
              ventas_total = mean(ventas_mensual),
              personas_total=mean(personas_mensual),
              personal_admon_total=mean(personal_admon),
              personal_operario_total=mean(personal_operario))

  contribucion_base<-subset(contribucion_base, select = -ANIO)

  contribucion<-contribucion_mensual %>%
    left_join(contribucion_base,by=c("DOMINIO_39_ENLACE"="DOMINIO_39_ENLACE"))

  tabla1<-contribucion %>%
    mutate(produccionNom =round((produccionNom_mensual/produccionNom_total)*100,1),
           produccion    =round((produccion_mensual/produccion_total)*100,1),
           ventasNom     =round((ventasNom_mensual/ventasNom_total)*100,1),
           ventas        =round((ventas_mensual/ventas_total)*100,1),
           personas      =round((personas_mensual/personas_total)*100,1),
           admon         =round((personal_admon/personal_admon_total)*100,1),
           operarios     =round((personal_operario/personal_operario_total)*100,1))  %>%
    select(DOMINIO_39_ENLACE,ANIO,MES,DESCRIPCIONDOMINIOEMMET39_ENLACE,produccionNom,
           produccion,ventasNom,ventas,personas,admon,operarios)


  contribucion_mensual <- data2 %>%
    group_by(ANIO,MES) %>%
    summarise(produccionNom_mensual=sum(PRODUCCIONNOMPOND),
              produccion_mensual=sum(PRODUCCIONREALPOND),
              ventasNom_mensual = sum(VENTASNOMINPOND),
              ventas_mensual = sum(VENTASREALESPOND),
              personas_mensual=sum(TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC),
              personal_admon=sum(TOTALEMPLEOADMON),
              personal_operario=sum(TOTALEMPLEOPRODUC))

  contribucion_base <- contribucion_mensual %>%
    filter(ANIO==2018) %>%
    group_by(ANIO) %>%
    summarise(produccionNom_total=mean(produccionNom_mensual),
              produccion_total=mean(produccion_mensual),
              ventasNom_total= mean(ventasNom_mensual),
              ventas_total= mean(ventas_mensual),
              personas_total=mean(personas_mensual),
              personal_admon_total=mean(personal_admon),
              personal_operario_total=mean(personal_operario))


  contribucion_mensual<-contribucion_mensual %>%
    mutate(produccionNom_total=contribucion_base$produccionNom_total,
           produccion_total=contribucion_base$produccion_total,
           ventasNom_total=contribucion_base$ventasNom_total,
           ventas_total=contribucion_base$ventas_total,
           personas_total=contribucion_base$personas_total,
           personal_admon_total=contribucion_base$personal_admon_total,
           personal_operario_total=contribucion_base$personal_operario_total)



  tabla1_1<-contribucion_mensual %>%
    mutate(produccionNom =round((produccionNom_mensual/produccionNom_total)*100,1),
           produccion    =round((produccion_mensual/produccion_total)*100,1),
           ventasNom     =round((ventasNom_mensual/ventasNom_total)*100,1),
           ventas        =round((ventas_mensual/ventas_total)*100,1),
           personas      =round((personas_mensual/personas_total)*100,1),
           admon         =round((personal_admon/personal_admon_total)*100,1),
           operarios     =round((personal_operario/personal_operario_total)*100,1))  %>%
    select(ANIO,MES,produccionNom,
           produccion,ventasNom,ventas,personas,admon,operarios)
  tabla1_1["DOMINIO_39_ENLACE"]<-"T_IND"
  tabla1_1["DESCRIPCIONDOMINIOEMMET39_ENLACE"]<-"Total Industria"
  tabla1$DOMINIO_39_ENLACE<-as.character(tabla1$DOMINIO_39_ENLACE)
  tabla1<-rbind(tabla1,tabla1_1)

  N<-as.data.frame(matrix(NA,ncol = ncol(indices_01),nrow = 12),colnames=FALSE)
  colnames(N)<-colnames(indices_01)

  in_prue<-NULL
  for(i in seq(from=216, to = nrow(indices_01), by =216)){
    if(i==216){
      in_prue<-rbind(indices_01[1:i,],N)
    }
    else{
      in_prue<-rbind(in_prue,indices_01[(i-215):i,],N)
    }

  }

  indices_lag<-rbind(in_prue[13:nrow(in_prue),5:ncol(in_prue)],N[,5:ncol(N)])
  colnames(indices_lag) <- paste("R", colnames(indices_lag), sep = "")
  indices_lag<-cbind(in_prue,indices_lag)
  indices_lag<-indices_lag[-c((nrow(indices_lag)-11):nrow(indices_lag)),]

  variacion<-within(indices_lag,{
    VPERSONALDEPRODUCCION  =PERSONALDEPRODUCCION/RPERSONALDEPRODUCCION
    VPERSONALDEADMINISTRACION=PERSONALDEADMINISTRACION/RPERSONALDEADMINISTRACION
    VEMPLEOTOTAL       =EMPLEOTOTAL/REMPLEOTOTAL
    VVENTASREALES      =VENTASREALES/RVENTASREALES
    VVENTASNOMINALES   =VENTASNOMINALES/RVENTASNOMINALES
    VPRODUCCIONREAL    =PRODUCCIONREAL/RPRODUCCIONREAL
    VPRODUCCIONNOMINAL =PRODUCCIONNOMINAL/RPRODUCCIONNOMINAL
  })

  variacion <- variacion[!is.na(variacion$DOMINIOS),]

  tabla2<-tabla1_7 %>%
    filter(ANIO==2018)
  tabla2<-as.data.frame(tabla2[,1:ncol(indices_01)])
  colnames(tabla2)<-paste("I", colnames(variacion[,1:ncol(tabla2)]), sep = "")
  tabla2 <- select(tabla2, -"ICLASESINDUSTRIALES" )
  varia<-variacion %>% left_join(tabla2,by=c("DOMINIOS"="IDOMINIOS",
                                             "ANO"="IANO","MES"="IMES"))

  varia<-varia %>%
    arrange(DOMINIOS,desc(ANO),desc(MES))

  vector<-unique(varia$DOMINIOS)
  variac<-NULL
  for (k in vector) {
    vari<-varia %>%
      filter(DOMINIOS==k)
    for (j in 26:ncol(varia)) {
      for(i in 1:204){
        vari[(i+12),j]=(vari[(i+12),(j-7)]*vari[i,j])
      }
    }
    variac<-rbind(variac,vari)
  }


  cols_i <- grep("^I", names(variac), value = TRUE)
  variac<- select(variac,c(DOMINIOS,ANO,MES,CLASESINDUSTRIALES,cols_i))
  colnames(variac)<-gsub("I","",names(variac))
  tabla1<-tabla1 %>% filter(ANIO!=2018)
  tabla1<-tabla1[,1:ncol(variac)]
  colnames(tabla1)<-colnames(variac)
  tabla1<-rbind(variac,tabla1)
  tabla1<-tabla1 %>% arrange(DOMNOS,ANO,MES)



  #Exportar
  names(sheets)
  sheet <- sheets[[13]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE,
               row.names=FALSE, startRow = 14, startColumn = 1)


  Enunciado<-paste0(meses[mes],"(",year,"/",year-1,")")
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE,
               row.names=FALSE, startRow = 9, startColumn = 1)




  # 8. Desestacionalizacion -------------------------------------------------

  CALENDAR.FN <- function(From_year,To_year){
    festivos<-read_xlsx(paste0("D:/Documentos/CAMILA/DANE/Desestacionalizacion/festivos.xlsx"))
    days <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
    #days <- c("Domingo","Lunes","Martes","Miercoles","Jueves","Viernes","Sabado")
    calendar   <- data.frame(dates=seq(as.POSIXct(paste0(From_year,"-01-01"),tz="GMT"),as.POSIXct(paste0(To_year,"-12-31"),tz="GMT"),"days"))
    calendar$year  <- year(calendar$dates)
    calendar$month <- month(calendar$dates)
    calendar$day   <- day(calendar$dates)
    calendar$wday  <- days[wday(calendar$dates)]

    # Incluir los dC-as festivos que no se mueven cuC!ndo caen en fin de semana + antes de 2011
    add_holidays <- c(paste0(From_year:To_year,"-01-01"),
                      paste0(From_year:To_year,"-05-01"),
                      paste0(From_year:To_year,"-07-20"),
                      paste0(From_year:To_year,"-08-07"),
                      paste0(From_year:To_year,"-12-08"),
                      paste0(From_year:To_year,"-12-25"))

    holidays <- c(as.POSIXct(add_holidays,format="%Y-%m-%d",tz="GMT"),as.POSIXct(festivos$FESTIVOS,format="%Y-%m-%d",tz="GMT"))
    holidays <- sort(holidays[!duplicated(holidays)])

    # Incluir domingo de ramos y domingo de resurrecciC3n
    holidays <- c(holidays,holidays[diff(holidays)==1]-ddays(4),holidays[diff(holidays)==1]+ddays(3))
    holidays <- sort(holidays[!duplicated(holidays)])

    calendar$holiday <- ifelse(as.POSIXct(calendar$dates,format="%Y-%m-%d",tz="GMT")%in%holidays,1,0)

    calendar_group <- calendar %>% group_by(year,month, wday) %>% summarise(total=n(),holidays=sum(holiday))
    calendar_group$total_available_days <- calendar_group$total-calendar_group$holidays
    calendar_pivot <- calendar_group %>% select(c("year","month","wday","total_available_days")) %>% pivot_wider(id_cols = c("year","month"),names_from = c("wday"),values_from = c("total_available_days"))

    calendar_final <- calendar_pivot
    for(i in days[-1]){
      calendar_final[,paste0(i)] <- calendar_final[,i]-calendar_final$Sunday
    }
    calendar_final <- calendar_final[,c("year","month",days[-1])]
    return(calendar_final)
  }
  calendar<-as.data.frame(CALENDAR.FN(2001,year))


  # Produccion Real ---------------------------------------------------------

  produccionreal<-tabla1[,c("DOMNOS","ANO","MES","PRODUCCONREAL")]
  produccionreal<-produccionreal %>% filter(DOMNOS=="T_IND")

  produccionreal<-produccionreal %>%
    right_join(calendar,by=c("ANO"="year","MES"="month"))
  produccionreal<-produccionreal[,c("PRODUCCONREAL","Monday","Tuesday","Wednesday",
                                    "Thursday","Friday","Saturday")]



  produccionreal_ts <- ts(produccionreal$PRODUCCONREAL, start = c(2001,1),frequency = 12)
  calendar_ts<-ts(produccionreal[,2:ncol(produccionreal)],start = c(2001,1),frequency = 12)
  colnames(calendar_ts)<-NULL


  produccionreal_desest <-seas(
    x = produccionreal_ts,
    #arima.model = " ",
    series.span = paste0(" 2001.1,",year,".",mes," "),
    #" 2001.1,2022.11",
    #series.span = "2001.1,2022.11",
    #series.modelspan = "2001.1,2022.11",
    transform.function = "auto",
    xreg = calendar_ts,
    regression.variables = c("easter[2]", "lpyear", "AO2016.Jul", "AO2016.Aug",
                             "AO2020.Mar", "TC2020.Apr", "AO2021.May"),
    regression.aictest ="easter",
    outlier.types = "all",
    outlier.lsrun = 3,
    automdl.savelog = "amd",
    forecast.maxlead = 1,
    #forecast.exclude = 12,
    #forecast.end = "2022.12",
    x11.seasonalma = "S3X5",
    history.estimates = c("fcst","aic","sadj","sadjchng", "trend","trendchng"),
    na.action = na.omit
  )


  tabla_prod<-as.data.frame(produccionreal_desest$data)
  tabla_prod<-tabla_prod$final
  deses<-tabla1[,c("DOMNOS","ANO","MES","PRODUCCONREAL")]
  deses<-deses %>% filter(DOMNOS=="T_IND")
  deses<-cbind(deses,tabla_prod)

  # Ventas Reales -----------------------------------------------------------


  ventasreales<-tabla1[,c("DOMNOS","ANO","MES","VENTASREALES")]
  ventasreales<-ventasreales %>% filter(DOMNOS=="T_IND")

  ventasreales<-ventasreales %>%
    right_join(calendar,by=c("ANO"="year","MES"="month"))
  ventasreales<-ventasreales[,c("VENTASREALES","Monday","Tuesday","Wednesday",
                                "Thursday","Friday","Saturday")]



  ventasreales_ts <- ts(ventasreales$VENTASREALES, start = c(2001,1),frequency = 12)
  calendar_ts<-ts(ventasreales[,2:ncol(ventasreales)],start = c(2001,1),frequency = 12)
  colnames(calendar_ts)<-NULL


  ventasreales_desest <-seas(
    x = ventasreales_ts,
    #arima.model = " ",
    series.span = paste0(" 2001.1,",year,".",mes," "),
    #series.span = "2001.1,2022.11",
    #series.modelspan = "2001.1,2022.11",
    transform.function = "auto",
    xreg = calendar_ts,
    regression.variables = c("easter[2]", "lpyear", "AO2016.Jul", "AO2016.Aug",
                             "AO2020.Mar", "TC2020.Apr", "AO2021.May"),
    regression.aictest ="easter",
    outlier.types = "all",
    outlier.lsrun = 3,
    automdl.savelog = "amd",
    forecast.maxlead = 1,
    #forecast.exclude = 12,
    #forecast.end = "2022.12",
    x11.seasonalma = "S3X5",
    history.estimates = c("fcst","aic","sadj","sadjchng", "trend","trendchng"),
    na.action = na.omit
  )


  tabla_vent<-as.data.frame(ventasreales_desest$data)
  tabla_vent<-tabla_vent$final
  deses<-cbind(deses,tabla_vent)



  # Empleo Total ------------------------------------------------------------

  empleototal<-tabla1[,c("DOMNOS","ANO","MES","EMPLEOTOTAL")]
  empleototal<-empleototal %>% filter(DOMNOS=="T_IND")

  empleototal<-empleototal %>%
    right_join(calendar,by=c("ANO"="year","MES"="month"))
  empleototal<-empleototal[,c("EMPLEOTOTAL","Monday","Tuesday","Wednesday",
                              "Thursday","Friday","Saturday")]



  empleototal_ts <- ts(empleototal$EMPLEOTOTAL, start = c(2001,1),frequency = 12)
  calendar_ts<-ts(empleototal[,2:ncol(empleototal)],start = c(2001,1),frequency = 12)
  colnames(calendar_ts)<-NULL


  empleototal_desest <-seas(
    x = empleototal_ts,
    #arima.model = " ",
    series.span = paste0(" 2001.1,",year,".",mes," "),
    #series.span = "2001.1,2022.11",
    #series.modelspan = "2001.1,2022.11",
    transform.function = "auto",
    xreg = calendar_ts,
    regression.variables = c("easter[2]", "lpyear", "AO2016.Jul", "AO2016.Aug",
                             "AO2020.Mar", "TC2020.Apr", "AO2021.May"),
    regression.aictest ="easter",
    outlier.types = "all",
    outlier.lsrun = 3,
    automdl.savelog = "amd",
    forecast.maxlead = 1,
    #forecast.exclude = 12,
    #forecast.end = "2022.12",
    x11.seasonalma = "S3X5",
    history.estimates = c("fcst","aic","sadj","sadjchng", "trend","trendchng"),
    na.action = na.omit
  )

  tabla_empl<-as.data.frame(empleototal_desest$data)
  tabla_empl<-tabla_empl$final
  deses<-cbind(deses,tabla_empl)


  #Exportar
  names(sheets)
  sheet <- sheets[[11]]
  addDataFrame(data.frame(deses), sheet, col.names=FALSE,
               row.names=FALSE, startRow = 14, startColumn = 1)


  Enunciado<-paste0(meses[mes],"(",year,"/",year-1,")")
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE,
               row.names=FALSE, startRow = 9, startColumn = 1)


  # 11. Var y cont_Trienal --------------------------------------------------


  contribucion_total <- data %>%
    filter(MES==mes & ANIO%in%c(2019))
  contribucion_total <- contr_sum_an(contribucion_total)

  contribucion <- data %>%
    filter(MES==mes & ANIO%in%c(year,2019)) %>%
    mutate(PERSONAL=TOTALEMPLEOPERMANENTE+TOTALEMPLEOTEMPORAL+TOTALEMPLEOADMON+TOTALEMPLEOPRODUC) %>%
    group_by(ANIO,MES,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  contribucion <- contr_fin(11,contribucion)


  tabla1 <- data %>%
    filter(ANIO%in%c(year,2019) & MES%in%mes) %>%
    group_by(ANIO,DOMINIO_39,DESCRIPCIONDOMINIOEMMET39)
  tabla1 <- tabla_summarise(11,tabla1)

  tabla1 <- tabla_pivot(11,tabla1)

  tabla1 <- tabla_paste_an(11,tabla1)


  tabla1 <- tabla1 %>% pivot_longer(cols = colnames(tabla1)[-c(1:5)],names_to = "variables",values_to = "value" )

  tabla1 <- tabla_acople(tabla1)

  tabla1 <- tabla1 %>%
    left_join(contribucion,by=c("DOMINIO_39"="DOMINIO_39",
                                "DESCRIPCIONDOMINIOEMMET39"="DESCRIPCIONDOMINIOEMMET39"))
  tabla1 <- tabla1[,c("DOMINIO_39","DESCRIPCIONDOMINIOEMMET39","varprodnom",
                      "varprod","produccion","varventasnom","varventas","ventas",
                      "varpersonas","personal")]

  for( i in c("varprodnom",
              "varprod","produccion","varventasnom","varventas","ventas",
              "varpersonas","personal")){
    tabla1[,i] <-  round(tabla1[,i]*100,1)
  }


  #Exportar
  names(sheets)
  sheet <- sheets[[14]]
  addDataFrame(data.frame(tabla1), sheet, col.names=FALSE,
               row.names=FALSE, startRow = 14, startColumn = 1)


  Enunciado<-paste0(meses[mes],"(",year,"/",year-1,")")
  addDataFrame(data.frame(Enunciado), sheet, col.names=FALSE,
               row.names=FALSE, startRow = 9, startColumn = 1)

  # Guardar archivo de salida -----------------------------------------------

  saveWorkbook(wb, Salida)


}
