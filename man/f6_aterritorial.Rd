% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f6_anexo_territorial.R
\name{f6_aterritorial}
\alias{f6_aterritorial}
\title{Anexo territorial}
\usage{
f6_aterritorial(directorio, mes, anio)
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
Esta funcion crea el archivo de Anexo Territorial.
 Tiene como insumo, la base de datos Tematica. El cuerpo de la función
 crea cada una de las hojas del reporte.

 Los cuadros de salida muestran información complementaria a la registrada
 en el boletín de prensa con el fin de brindar la información a un nivel más
 desagregado tanto total nacional como a nivel de departamental, áreas
 metropolitanas y principales ciudades del país.

 Los resultados se muestran con variaciones anuales, año corrido y doce meses,
 junto con sus respectivas contribuciones, según dominios, por las principales
 variables que se recolectan en el proceso: Producción (real y nominal),
 ventas (real y nominal) y empleo.
 De igual manera se presentan los resultados de sueldos causados y horas
 totales trabajadas para los dominios nacionales.
}
\details{
En los anexos territoriales se calculan las contribuciones y variaciones
 en tres diferentes periodos.

 Es importante resaltar que el equipo en el que se ejecutará la función debe
 tener intalado Java. Ver \href{https://github.com/NataliArteaga/DANE.EMMET#readme}{README}

 A continuación se muestran los  periodos
 y las formulas para realizar el calculo de estos:

 Contribucion anual:

 \deqn{
 CA_{trj} = \frac{(V_{trj} - V_{(t-12)rj)}}{\sum_{1}^n V_{(t-12)rj)}} *100
 }



 Donde:

 t: Mes de referencia de la publicación de la Operación Estadística

 \eqn{V_{trj}}: Valor en el periodo t para el territorio r en el dominio j.

 \eqn{V_{(t-12)rj}}: Valor en el periodo t-12 o en el año anterior, en el
 territorio r en el dominio j.

 \eqn{\sum_{1}^n V_{(t-12)rj)}}: Sumatoria de los valores en el periodo t-12,
 en el territorio r y en el dominio j

 Esta contribucion anual se interpreta como el aporte del domino j en el territorio
 r a la variación anual del mes de referencia en el domino j en el territorio r

 Contribucion anio corrido:

 \deqn{
 CAC_{Trj} = \frac{\sum_{i}^T(V_{trj} - \sum_{b}^{T-12} V_{trj)}}{\sum_{b}^{T-12} V_{trj)}} *100
 }


 Donde:
 t: Mes variando de enero a diciembre

 T: Mes de referencia.

 i: Siempre es el mes de enero.

 b=i-12: Corresponde a enero del año anterior

 \eqn{V_{trj}}: Valor de la variable en el periodo t en el territorio r en el dominio j

 Esta contribucion de anio corrido se interpreta como el aporte del domino j
 en el territorio r a la variación año corrido del mes de referencia en el
 domino j en el territorio r.


 Contribucion anio acumulado:

 \deqn{
 CAA_{Trj} = \frac{\sum_{a+1}^T(V_{trj} - \sum_{b+1}^{a} V_{trj)}}{\sum_{b+1}^{a} V_{trj)}} *100
 }

 Donde:

 t: Mes variando de enero a diciembre

 T: Mes de referencia.

 a=T-12

 b=a-12: Corresponde al mes a del año anterior

 \eqn{V_{trj}}: Valor de la variable en el periodo t en el territorio r en el
 dominio j

 Nota: cuando las variables que denotan meses (a, b) son negativas representan
 el mes del año inmediatamente anterior.

 Esta contribucion de anio acumulado se interpreta como el aporte del domino
 j en el territorio r a la variación acumulada anual del mes de referencia
 en el domino j en el territorio r.


 Variación anual:

 Es la relación del índice o valor (para producción y ventas, categoría de
 contratación, sueldos, horas) en el mes de referencia (ti) con el índice o
 valor absoluto del mismo mes en el año anterior (t i-12), menos 1 por 100.

 \deqn{
 VA = \frac{\text{índice o valor del mes de referencia}}
 {\text{índice o valor del mismo mes del año anterior}} -1 *100
 }

 Se interpreta como el crecimiento o disminución porcentual, dependiendo de
 si el resultado es negativo o positivo, de la variable correspondiente en
 el mes de referencia, en relación al mismo mes del año anterior


 Variación Año Corrido:

 \deqn{
 VAC = \frac{\sum \text{índice o valor de enero al mes de referencia del año actual}}
 {\sum \text{índice o valor de enero al mes de referencia del mismo mes del año anterior}} -1 *100
 }

 Se interpreta como el crecimiento o disminución porcentual, dependiendo de
 si el resultado es negativo o positivo, de la variable correspondiente en lo
 corrido del año hasta el mes de referencia, en relación al mismo periodo del
 año anterior


 Variación Acumulado Anual:

 \deqn{
 VAA = \frac{\sum \text{índice o valor desde} a_{+1} \text{enero al mes de referencia}}
 {\sum \text{índice o valor en el año anterior desde} a_{+1} \text{ hasta el mes de referencia}} -1 *100
 }

 Donde:

 t=mes de referencia

 a=t-12

 Se interpreta como el crecimiento o disminución porcentual, dependiendo de
 si el resultado es negativo o positivo, de la variable correspondiente en l
 os últimos 12 meses hasta el mes de referencia, en relación al mismo periodo
 del año anterior.
 Contribuciones porcentuales: aporte en puntos porcentuales de las variaciones
 individuales a la variación de un agregado.

 La función escribe, en formato excel, las hojas:

 1. Var y Cont Anual Dpto:
 Se caclcula la contribución y variación anual (%)
 del valor de la producción, ventas, y empleo, según departamento

 2. Var y Cont Anual Desagreg Dp:
 Se caclcula la contribución y variación anual (%)
  del valor de la producción, ventas, y empleo, según clase industrial
  por departamento

 3.Var y Cont Anual Áreas metrop:
 Se caclcula la contribución y variación anual (%)
 del valor de la producción, ventas, y empleo, según área metropolitana

 4. Var y Cont Anual Ciudades:
 Se caclcula la contribución y variación anual (%)
 del valor de la producción, ventas, y empleo, según ciudad

 5. Var y Cont Año corrido Dpto:
 Se caclcula la contribución y variación aaño corrido (%)
 del valor de la producción, ventas, y empleo, según departamento

 6.Var y Cont Año corri Desag Dp:
 Se caclcula la contribución y variación año corrido (%)
 del valor de la producción, ventas, y empleo, según clase
 industrial por departamento

 7.Var y Cont Año corrido Áreas met:
 Se caclcula la contribución y variación a año corrido (%)
 el valor de la producción, ventas, y empleo, según área metropolitana

 8. Var y Cont Año corrid Ciudad:
 Se caclcula la contribución y variación año corrido (%)
 del valor de la producción, ventas, y empleo, según ciudad

 9. Var y Cont doce meses Dpto:
 Se caclcula la contribución y variación doce meses (%)
 del valor de la producción, ventas, y empleo, según departamento

 10.Var y Cont doce meses Desa:
 Se caclcula la contribución y variación  doce meses (%)
 del valor de la producción, ventas, y empleo, según clase
 industrial por departamento

 11.Var y Cont docemeses Áreas:
 Se caclcula la contribución y variación doce meses (%)
 del valor de la producción, ventas, y empleo, según área metropolitana

 12. Var y Cont docemeses Ciu:
 Se caclcula la contribución y variación ddoce meses (%)
 del valor de la producción, ventas, y empleo, según ciudad

 13. Índices Departamentos:
 Se caclcula los índices, de producción nominal y real,
 ventas nominal y real, empleo según departamento y clase industrial

 14. Índices Áreas Metropolitana:
 Se caclcula los índices de producción nominal y real, ventas nominal
 y real, empleo según área metropolitana

 15. Índices Ciudades:
 Se caclcula los índices, de producción nominal y real, ventas nominal
 y real, empleo según ciudades

 16. Var y Cont Trienal Dpto:
 Se caclcula la contribución y variación  trienal (%), es decir; usando
 como año base los datos del año 2019, ddel valor de la producción, ventas,
 y empleo, según departamento

 17. Var y Cont Trienal Desa:
 Se caclcula la contribución y variación  trienal (%), es decir usando
 como año base los dtos del año 2019, del valor de la producción, ventas,
 y empleo, según clase industrial por departamento

 18.Var y Cont Trienal Áreas:
 Se caclcula la contribución y variación  trienal (%), es decir usando
 como año base los dtos del año 2019, agrupando los datos por
 áreas metropolitanas de la produccion, ventas y personal

 19. Var y Cont Trienal Ciuda:
 Se caclcula la contribución y y variación  trienal (%), es decir usando
 como año base los dtos del año 2019, del valor de la producción, ventas,
 y empleo, según ciudad.


 Finalmente se exporta una archivo excel, que contiene la informacion de las
 19 hojas.
}
\examples{
f6_aterritorial(directorio="Documents/DANE/Procesos DIMPE /PilotoEMMET",
                       mes=11,anio=2022)

}
