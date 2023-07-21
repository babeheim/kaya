\name{parse_sgf}
\alias{parse_sgf}
\title{Parse SGF File}
\description{
  Parse an SGF (Smart Game Format) file and return a list representing the game data.
}
\usage{
  parse_sgf(file_path)
}
\arguments{
  \item{file_path}{A character string containing the file path of the SGF file to be parsed.}
}
\details{
  This function parses an SGF file and returns a list representing the game data. The parsed game data includes information about the board size, moves, colors, and other SGF properties.
}
\value{
  A list representing the parsed game data from the SGF file.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  game_data <- parse_sgf("example_game.sgf")
  print(game_data)
}
