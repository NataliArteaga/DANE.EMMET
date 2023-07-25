
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

Esta función requiere que su computador cuente con Java, pues se usa la
librería xlsx, y ésta con la ayuda de Java sobre-escribe algunos
archivos de excel, y genera varias hojas dentro del mismo y, con ellos,
genera el archivo final de los anexos.

La instalación de Java se puede realizar para Windows, Linux y Mac.

Para Windows:

1.  Entre el sitio web oficial de Java en <https://www.oracle.com/java/>
    y haga clic en “Descargar” o “Get Java”.

2.  En la página de descargas, asegúrase de seleccionar la versión
    correcta de Java para Windows, ya sea de 32 o 64 bits, dependiendo
    de su sistema operativo.

3.  Haga clic en el enlace de descarga correspondiente y acepte los
    términos y condiciones de Oracle.

4.  Una vez que se complete la descarga, haga doble clic en el archivo
    de instalación y siga las instrucciones del asistente de
    instalación.

5.  Durante la instalación, es posible que se le solicite elegir una
    ubicación para instalar Java. Por lo general, se recomienda utilizar
    la ubicación predeterminada.

6.  Una vez finalizada la instalación, verifique que Java se haya
    instalado correctamente abriendo la línea de comandos y ejecutando
    el comando “java -version”. Debería ver la versión de Java instalada
    en su sistema.

Para Mac:

1.  En su Mac, abra un navegador web e ingrese al sitio web oficial de
    Java en <https://www.oracle.com/java/> y haga clic en “Descargar” o
    “Get Java”.

2.  En la página de descargas, asegúrase de seleccionar la versión
    correcta de Java para macOS.

3.  Haga clic en el enlace de descarga correspondiente y acepte los
    términos y condiciones de Oracle.

4.  Una vez que se complete la descarga, haga doble clic en el archivo
    de instalación y siga las instrucciones del asistente de
    instalación.

5.  Durante la instalación, es posible que se le solicite ingresar la
    contraseña de administrador de su Mac.

6.  Una vez finalizada la instalación, verifique que Java se haya
    instalado correctamente abriendo la Terminal y ejecutando el comando
    “java -version”. Debería ver la versión de Java instalada en su
    sistema.

Dependiendo de las versiones específicas de Java y de los sistemas
operativos que estés utilizando, algunos pasos o detalles podrían
variar.

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
