% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f8_cregiones.R
\name{f8_cregiones}
\alias{f8_cregiones}
\title{Cuadros desagregaion regiones}
\usage{
f8_cregiones(directorio, ani, mes)
}
\arguments{
\item{directorio}{definir el directorio donde se encuentran ubicado los datos de entrada}

\item{mes}{Definir el mes a ejecutar, ej: 11}

\item{anio}{Definir el año a ejecutar, ej: 2022}
}
\value{
CSV file
}
\description{
Esta funcion crea un archivo de excel que contiene las variaciones
y contribuciones anueales desagregadas por cada región. Se crea
una hoja por cada desagregación en donde se consigna las variaciones y contribuciones de las variables
de produccion, ventas, emppleados, sueldos y horas.
}
\details{
Esta funcion se hace con la finalidad de crear cuadros informativos
con datos desagregada según el interés, para facilitar el análisis del comportamiento
de cada región.
}
\examples{
f8_cregiones(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",
                       mes=11,anio=2022)

}
