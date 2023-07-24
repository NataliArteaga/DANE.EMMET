
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DANE.EMMET

<!-- badges: start -->
<!-- badges: end -->

Este paquete tiene como objetivo optimizar los tiempos del proceso que
se realiza para la encuesta mensual manufacturera con enfoque
territorial (EMMET). La cual sigue el siguiente flujo: ![Flujo del
proceso](imagenes/flujo.jfif)

## Instalación

Para instalar la libreria de “DANE.EMMET” se realiza con el comando

``` r
#instalar la libreria
remotes::install_github("NataliArteaga/DANE.EMMET")
#cargar la libreria
library(DANE.EMMET)
```

## Instalación Java

## Uso

Esta libreria se puede usar de 2 maneras

1.  Funciones individuales: la librera consta de las siguientes
    funciones individuales -inicial(directorio)
    -integracion(directorio,mes,anio)
    -estandarizacion(directorio,mes,anio)
    -identificacion_outliers(directorio,mes,anio)
    -imputacion_outliers(directorio,mes,anio)
    -anacional(directorio,mes,anio) -aterritorial(directorio,mes,anio)
    -boletin(directorio,mes,anio)

Las cuales se puden correr una a una para asi poder revisar los archivos
de salida de cada función.

2.  Funciones macro: la libreria cuenta con dos macro funciones

-macro1(directorio,mes,anio): la cual corre las funciones
inicial,integracion,estandarizacion e identificacion_outliers, para que
asi el usuario pueda verificar y modificar las alertas, y luego
continuar con el proceso.

-macro2(directorio,mes,anio): una vez modificado el archivo de alertas
podemos continuar con el proceso, con esta macro funcion se corren las
funciones imputacion_outliers, anacional,aterritorial y boletin.

## Ejemplos

1.  Ejemplo de usar las funciones de manera individual, se deben correr
    en orden ya que las funciones cargan la salida de la función
    respectivamente anterior

``` r
#cargar la libreria
library(DANE.EMMET)
#correr funcion inicial
inicial(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET")
# correr funcion integracion
integracion(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022)
# correr funcion estandarizacion
estandarizacion(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022)
# correr funcion identificacion_outliers
identificacion_outliers(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022)
# correr funcion imputacion_outliers
imputacion_outliers(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022)
# correr funcion anacional
anacional(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022)
# correr funcion aterritorial
aterritorial(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022)
# correr funcion boletin
boletin(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022)
```

Se puede revisar el archivo de salida despues de cada función

2.  Ejemplo de como usar las macro funciones

``` r
#cargar la libreria
library(DANE.EMMET)
#correr funcion macro1
macro1(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022)
#verificar y/o modificar el archivo de alertas para continuar con el proceso

#correr funcion macro2
macro2(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",mes=11,anio=2022,fecha="24/12/2022",guardar="Documents/DANE/Procesos DIMPE /PilotoEMMET/results/boletin")
```
