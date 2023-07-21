\name{reflect_45}
\alias{reflect_45}
\title{Reflect Matrix Diagonally at 45 degrees}
\description{
  Reflect a matrix diagonally at 45 degrees.
}
\usage{
  reflect_45(x)
}
\arguments{
  \item{x}{A matrix to be reflected diagonally at 45 degrees.}
}
\details{
  This function reflects a matrix \code{x} diagonally at 45 degrees, swapping rows and columns.
}
\value{
  A matrix resulting from the diagonal reflection of \code{x}.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  matrix1 <- matrix(1:9, ncol = 3)
  result_matrix <- reflect_45(matrix1)
  print(result_matrix)
}
