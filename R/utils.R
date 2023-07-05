#utils


#crea un vector con los meses
meses <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")

# Función formato nombres columnas
colnames_format <- function(base){
  colnames(base) <- toupper(colnames(base))
  colnames(base) <- gsub(" ","",colnames(base))
  colnames(base) <- gsub("__","_",colnames(base))
  colnames(base) <- stringi::stri_trans_general(colnames(base), id = "Latin-ASCII")
  return(colnames(base))
}

#crea un vector con las variables fecha
cols_to_date <- c("II_PA_PP_IEP","II_PA_PP_FEP","II_PA_TD_IET","II_PA_TD_FET",
                  "II_PA_TI_IETA","II_PA_TI_FETA","II_PA_AP_AI_AP","II_PA_AP_AF_AP",
                  "II_PP_PP_IOP","II_PP_PP_FOP","II_PP_TD_IOT","II_PP_TD_FOT",
                  "II_PP_TI_IOTA","II_PP_TI_FOTA","II_PP_AP_AI_PP","II_PP_AP_AF_PP",
                  "II_HORAS_HORDI_D","II_HORAS_HORDI_H",
                  "III_PE_IV","III_PE_FV")


#crea un vector con las variables de interes
variablesinte=c("II_PA_PP_NPERS_EP","AJU_II_PA_PP_SUELD_EP","II_PA_TD_NPERS_ET",
                "AJU_II_PA_TD_SUELD_ET","II_PA_TI_NPERS_ETA","AJU_II_PA_TI_SUELD_ETA","II_PA_AP_AAEP","AJU_II_PA_AP_AAS_AP",
                "II_PP_PP_NPERS_OP","AJU_II_PP_PP_SUELD_OP","II_PP_TD_NPERS_OT","AJU_II_PP_TD_SUELD_OT","II_PP_TI_NPERS_OTA",
                "AJU_II_PP_TI_SUELD_OTA","II_PP_AP_APEP","AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T",
                "AJU_III_PE_PRODUCCION","AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","III_EX_VEXIS")




#crea un vector con las variables del capitulo 2
cap2=c("II_PA_PP_NPERS_EP","AJU_II_PA_PP_SUELD_EP","II_PA_TD_NPERS_ET",
       "AJU_II_PA_TD_SUELD_ET","II_PA_TI_NPERS_ETA","AJU_II_PA_TI_SUELD_ETA","II_PA_AP_AAEP","AJU_II_PA_AP_AAS_AP",
       "II_PP_PP_NPERS_OP","AJU_II_PP_PP_SUELD_OP","II_PP_TD_NPERS_OT","AJU_II_PP_TD_SUELD_OT","II_PP_TI_NPERS_OTA",
       "AJU_II_PP_TI_SUELD_OTA","II_PP_AP_APEP","AJU_II_PP_AP_AAS_PP","AJU_II_HORAS_HORDI_T","AJU_II_HORAS_HEXTR_T")

#crea un vector con las variables del capitulo 3
cap3=c("AJU_III_PE_PRODUCCION","AJU_III_PE_VENTASIN","AJU_III_PE_VENTASEX","III_EX_VEXIS" )
