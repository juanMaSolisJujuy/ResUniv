\name{s_simple}
\alias{s_simple}
\title{Medidas de resumen univariadas en series numéricas simples.}
\usage{
s_simple(x, decimales = 1, clipboard = FALSE)
}
\description{
Devuelve medidas de tendencia central, medidas de dispersión, medidas de forma e intervalos de confianza (si corresponde) de las series los datos numéricos ingresados, discretos o continuos. Si se desea, es posible exportar los resultados utilizando el argumento "clipboard = TRUE".
}
\arguments{
x: vector numérico de tipo *integer* o *numeric*.

decimales: el número de posiciones decimales de los resultados.

clipboard: lógico; si se desea exportar los resultados con el portapapeles.
}
\examples{

x = runif(20, 10, 20)
s_simple(x, decimales = 2, clipboard = TRUE) # devuelve las medidas de resumen y copia en el 'clipboard' para su exportación.

}
