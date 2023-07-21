\name{validate_sgfs}
\alias{validate_sgfs}
\title{Validate SGF Files}
\description{
  Validate SGF files to check if they are correctly formatted.
}
\usage{
  validate_sgfs(files)
}
\arguments{
  \item{files}{A character vector containing file paths of SGF files to be validated.}
}
\details{
  This function validates SGF files to check if they are correctly formatted. It checks the file existence, SGF codes, and move coordinates for validity.
}
\value{
  A character vector indicating the validation results for each input SGF file.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  sgf_files <- c("game1.sgf", "game2.sgf", "game3.sgf")
  validation_results <- validate_sgfs(sgf_files)
  print(validation_results)
}
