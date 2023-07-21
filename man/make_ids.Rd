\name{make_ids}
\alias{make_ids}
\title{Generate Unique IDs}
\description{
  Generate unique IDs based on random characters.
}
\usage{
  make_ids(n, reserved = "", seed = NA, nchars = NA)
}
\arguments{
  \item{n}{The number of unique IDs to generate.}
  \item{reserved}{A character vector containing reserved IDs that should not be used.}
  \item{seed}{An optional numeric value to set the random seed for reproducibility.}
  \item{nchars}{An optional integer specifying the number of characters in each ID.}
}
\details{
  This function generates a specified number of unique IDs based on random characters. It avoids using reserved IDs and ensures no duplication in the output.
}
\value{
  A character vector containing the generated unique IDs.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  ids <- make_ids(5, reserved = c("ABC123", "XYZ456"), seed = 42, nchars = 6)
  print(ids)
}
