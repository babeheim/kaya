\name{id_direct_connections}
\alias{id_direct_connections}
\title{Identify Direct Connections in Game Moves}
\description{
  Identify direct connections between stones in game moves.
}
\usage{
  id_direct_connections(moves)
}
\arguments{
  \item{moves}{A data frame containing game move information with columns: row and column.}
}
\details{
  This function identifies direct connections between stones in game moves based on their locations.
}
\value{
  A logical matrix representing direct connections between stones.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  game_moves <- data.frame(
    row = c(1, 1, 2, 2),
    column = c(1, 2, 1, 2)
  )
  connections <- id_direct_connections(game_moves)
  print(connections)
}
