\name{reflect_x}
\alias{reflect_x}
\title{Reflect Matrix Horizontally (X-axis)}
\description{
  Reflect a matrix horizontally (X-axis).
}
\usage{
  reflect_x(x)
}
\arguments{
  \item{x}{A matrix to be reflected horizontally.}
}
\details{
  This function reflects a matrix \code{x} horizontally (X-axis), reversing the order of columns.
}
\value{
  A matrix resulting from the horizontal reflection of \code{x}.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  matrix1 <- matrix(1:9, ncol = 3)
  result_matrix <- reflect_x(matrix1)
  print(result_matrix)
}
