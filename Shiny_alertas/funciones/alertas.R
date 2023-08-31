
# librerias ---------------------------------------------------------------
require(dplyr)
require(tidyverse)
require(zoo)
require(xts)
require(lubridate)
require(TSstudio)
require(tseries)
require(forecast)


# codigo ------------------------------------------------------------------



capitulo3=c("Ajuste_Producción","Ajuste_Venta_productos_elaborados_país",
            "Ajuste_Venta_productos_elaborados_exterior","Valor_existencias_precio_costo" )
variablesinte=c("II_PA_PP_NPERS_EP","AJU_II_PA_PP_SUELD_EP","II_PA_TD_NPERS_ET",
                "AJU_II_PA_TD_SUELD_ET","II_PA_TI_NPERS_ETA","AJU_II_PA_TI_SUELD_ETA",
                "II_PA_AP_AAEP","AJU_II_PA_AP_AAS_AP","II_PP_PP_NPERS_OP","AJU_II_PP_PP_SUELD_OP",
                "II_PP_TD_NPERS_OT","AJU_II_PP_TD_SUELD_OT","II_PP_TI_NPERS_OTA","AJU_II_PP_TI_SUELD_OTA",
                "II_PP_AP_APEP","AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T",
                "AJU_III_PE_PRODUCCION","AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","III_EX_VEXIS")


for (i in variablesinte) {

  base_panel[,i] <- as.numeric(base_panel[,i])
  base_panel[,i] <- ifelse(is.na(base_panel[,i]),0,base_panel[,i])
}
base_variables_encuesta <- base_panel %>%
  select(ANIO,MES,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,
         II_PA_PP_NPERS_EP,AJU_II_PA_PP_SUELD_EP,II_PA_TD_NPERS_ET,AJU_II_PA_TD_SUELD_ET,
         II_PA_TI_NPERS_ETA,AJU_II_PA_TI_SUELD_ETA,II_PA_AP_AAEP,AJU_II_PA_AP_AAS_AP,
         II_PP_PP_NPERS_OP,AJU_II_PP_PP_SUELD_OP,II_PP_TD_NPERS_OT,AJU_II_PP_TD_SUELD_OT,
         II_PP_TI_NPERS_OTA,AJU_II_PP_TI_SUELD_OTA,II_PP_AP_APEP,AJU_II_PP_AP_AAS_PP,
         AJU_II_HORAS_HORDI_T,AJU_II_HORAS_HEXTR_T,AJU_III_PE_PRODUCCION,
         AJU_III_PE_VENTASIN,AJU_III_PE_VENTASEX,III_EX_VEXIS)

base_variables_encuesta=as.data.frame(base_variables_encuesta)

des <- base_variables_encuesta %>%
  filter(ANIO==anio & MES%in%c(mes-1,mes))  %>%
  pivot_longer(cols=colnames(base_variables_encuesta)[8:length(colnames(base_variables_encuesta))],names_to = "Variables",values_to ="Valores") %>%
  pivot_wider(names_from = "MES",values_from = "Valores")
des=as.data.frame(des)
colnames(des)[c(8,9)] <- c(meses_c[mes-1],meses_c[mes])
des$variacion <- round(((abs(des[,paste0(meses_c[mes])]-des[,paste0(meses_c[mes-1])])/des[,paste0(meses_c[mes-1])])*100),2)
des$variacion[des[, paste0(meses_c[mes])] != 0 & des[, paste0(meses_c[mes - 1])] == 0] <- 100
des$variacion[des[, paste0(meses_c[mes])] == 0 & des[, paste0(meses_c[mes - 1])] == 0] <- 0


varinteres=paste0(variablesinte,"_caso_de_imputacion")

des2 <- alertas %>% filter(ANIO==anio & MES==mes) %>%
  pivot_longer(cols=varinteres,names_to = "Variables",values_to ="Caso_imputacion")  %>%
  select(ANIO,NOMBREDEPARTAMENTO,NOMBREMUNICIPIO,ID_NUMORD,NOMBRE_ESTAB,DOMINIOEMMET39,Variables,Caso_imputacion)
des2$Variables <- sub("\\_caso_de_imputacion", "", des2$Variables)
des2=as.data.frame(des2)
df_merge <- merge(des, des2, by = c("ANIO","NOMBREDEPARTAMENTO","NOMBREMUNICIPIO",
                                    "ID_NUMORD","NOMBRE_ESTAB","DOMINIOEMMET39" ,"Variables"))

# Seleccionar todas las columnas de la tabla B y la columna valores 2 de la tabla A
desf <- df_merge[, c(names(des), "Caso_imputacion")]
desf[, paste0(meses_c[mes - 1])]=format(round(desf[, paste0(meses_c[mes - 1])], 1), big.mark=".",decimal.mark = ",")
desf[, paste0(meses_c[mes])]=format(round(desf[, paste0(meses_c[mes])], 1), big.mark=".",decimal.mark = ",")
desf$variacion=format(round(desf$variacion, 1), big.mark=".",decimal.mark = ",")
desf=arrange(desf,ID_NUMORD)
# Renombrar variables -----------------------------------------------------

variablesinte=c("II_PA_PP_NPERS_EP","AJU_II_PA_PP_SUELD_EP","II_PA_TD_NPERS_ET",
                "AJU_II_PA_TD_SUELD_ET","II_PA_TI_NPERS_ETA","AJU_II_PA_TI_SUELD_ETA",
                "II_PA_AP_AAEP","AJU_II_PA_AP_AAS_AP","II_PP_PP_NPERS_OP",
                "AJU_II_PP_PP_SUELD_OP","II_PP_TD_NPERS_OT","AJU_II_PP_TD_SUELD_OT",
                "II_PP_TI_NPERS_OTA","AJU_II_PP_TI_SUELD_OTA","II_PP_AP_APEP",
                "AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T",
                "AJU_III_PE_PRODUCCION","AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","III_EX_VEXIS")
cambio=c("Empleados_permanentes","Ajuste_sueldos_Empleados_Permanentes",
         "Empleados_temporales", "Ajuste_sueldos_Empleados_Temporales",
         "Empleados_temporales_agencias","Ajuste_sueldos_Empleados_Temporales_Agencia",
         "Aprendices_pasantes","Ajuste_apoyo_sostenimiento_Aprendices_Pasantes",
         "Obreros_permanentes","Ajuste_sueldos_Operarios_Permanentes",
         "Obreros_temporales","Ajuste_sueldos_Operarios_Temporales",
         "Obreros_temporales_agencias","Ajuste_sueldos_Operarios_Temporales_Agencia",
         "Aprendices_pasantes_producción","Ajuste_apoyo_sostenimiento_aprendices_pasantes_producción",
         "Ajuste_Horas_ordinarias_trabajadas","Ajuste_Horas_extras_trabajadas",
         "Ajuste_Producción","Ajuste_Venta_productos_elaborados_país",
         "Ajuste_Venta_productos_elaborados_exterior","Valor_existencias_precio_costo")
desf$Variables <- factor(desf$Variables, levels= variablesinte, labels = cambio)
colnames(base_variables_encuesta)[8:length(colnames(base_variables_encuesta))]=c("Empleados_permanentes","Ajuste_sueldos_Empleados_Permanentes",
                                                                                 "Empleados_temporales", "Ajuste_sueldos_Empleados_Temporales",
                                                                                 "Empleados_temporales_agencias","Ajuste_sueldos_Empleados_Temporales_Agencia",
                                                                                 "Aprendices_pasantes","Ajuste_apoyo_sostenimiento_Aprendices_Pasantes",
                                                                                 "Obreros_permanentes","Ajuste_sueldos_Operarios_Permanentes",
                                                                                 "Obreros_temporales","Ajuste_sueldos_Operarios_Temporales",
                                                                                 "Obreros_temporales_agencias","Ajuste_sueldos_Operarios_Temporales_Agencia",
                                                                                 "Aprendices_pasantes_producción","Ajuste_apoyo_sostenimiento_aprendices_pasantes_producción",
                                                                                 "Ajuste_Horas_ordinarias_trabajadas","Ajuste_Horas_extras_trabajadas",
                                                                                 "Ajuste_Producción","Ajuste_Venta_productos_elaborados_país",
                                                                                 "Ajuste_Venta_productos_elaborados_exterior","Valor_existencias_precio_costo")
desf["MES"] <- mes
desf["LLAVE"] <- as.numeric(paste0(desf$ANIO,desf$MES,desf$ID_NUMORD))
desf$OBSERVACIONES <- NA
desf$EDITADO <- NA
