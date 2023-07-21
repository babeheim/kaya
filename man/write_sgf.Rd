\name{write_sgf}
\alias{write_sgf}
\title{Write SGF File}
\description{
  Write game data to an SGF (Smart Game Format) file.
}
\usage{
  write_sgf(game_list, file)
}
\arguments{
  \item{game_list}{A list containing game data to be written to the SGF file.}
  \item{file}{A character string containing the file path of the SGF file to be written.}
}
\details{
  This function writes game data from a list to an SGF file. The \code{game_list} should contain tags and values representing the SGF properties.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  game_data <- list(
    SZ = "19",
    AB = c("dd", "dp", "qd"),
    AW = c("pd", "pp"),
    moves = data.frame(
      number = c(1, 2),
      row = c(1, 2),
      column = c(1, 2),
      color = c("black", "white"),
      group_id = c("A", "B")
    )
  )
  write_sgf(game_data, "game1.sgf")
}
