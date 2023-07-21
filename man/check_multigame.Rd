\name{check_multigame}
\alias{check_multigame}
\title{Check Multiple SGF Files for Validity}
\description{
  Check the validity of multiple SGF files and return a vector of messages indicating whether each file is valid or not.
}
\usage{
  check_multigame(files)
}
\arguments{
  \item{files}{A character vector containing the file paths of the SGF files to be checked.}
}
\details{
  This function checks the validity of multiple SGF files and returns a vector of messages indicating whether each file is valid or not. It checks if the files exist and if they are valid SGF files. It also validates the coordinates and SGF codes in each file to ensure they are in the correct format.
}
\value{
  A character vector containing messages for each SGF file, indicating whether it is valid or not.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  files <- c("game1.sgf", "game2.sgf", "game3.sgf")
  check_results <- check_multigame(files)
  print(check_results)
}
