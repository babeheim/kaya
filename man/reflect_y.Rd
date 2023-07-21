\name{reflect_y}
\alias{reflect_y}
\title{Reflect Matrix Vertically (Y-axis)}
\description{
  Reflect a matrix vertically (Y-axis).
}
\usage{
  reflect_y(x)
}
\arguments{
  \item{x}{A matrix to be reflected vertically.}
}
\details{
  This function reflects a matrix \code{x} vertically (Y-axis), reversing the order of rows.
}
\value{
  A matrix resulting from the vertical reflection of \code{x}.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  matrix1 <- matrix(1:9, ncol = 3)
  result_matrix <- reflect_y(matrix1)
  print(result_matrix)
}
