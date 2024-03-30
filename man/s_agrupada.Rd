\name{s_agrupada}
\alias{s_agrupada}
\title{Tabla de frecuencias de series univariadas numéricas.}
\usage{
s_agrupada(x, decimales = 1, li = NULL, ls = NULL, a = NULL, derecha = TRUE, clipboard = FALSE)
}
\description{
Devuelve la tabla de frecuencias de una serie de datos numérica, univariada, de tipo *integer* o *numeric*. En el caso de datos de tipo *numeric*, es posible especificar los límites y la amplitud de los intervalos de clase. Por defecto, construye los intervalos de clase cerrados por derecha, con la regla de Sturges.

Si se desea, es posible exportar los resultados utilizando el argumento "clipboard = TRUE".
}
\arguments{
x: vector numérico de tipo *integer* o *numeric*; en este último caso, el vector debe tener una longitud mayor a 1, y permitir el cálculo de medidas de variabilidad.

li: límite inferior del primer intervalo de clase, para datos de tipo *numeric*.

ls: límite superior del intervalo de clase superior, para datos de tipo *numeric*.

a: amplitud del intervalo de clase, para datos de tipo *numeric*.

derecha: si los intervalos de clase son cerrados por derecha, para datos de tipo *numeric*.

decimales: el número de posiciones decimales de los resultados.

clipboard: lógico; si se desea exportar los resultados con el portapapeles.
}
\examples{

x = runif(20, 0, 10)|>as.integer()
s_agrupada(x, decimales = 2)

x = rnorm(100)
s_agrupada(x, li = -4, ls = 4, a = 1)

}
