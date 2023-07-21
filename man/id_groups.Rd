\name{id_groups}
\alias{id_groups}
\title{Identify Groups in Game Moves}
\description{
  Identify groups of stones in game moves.
}
\usage{
  id_groups(moves)
}
\arguments{
  \item{moves}{A data frame containing game move information with columns: row, column, and group_id.}
}
\details{
  This function identifies groups of stones in game moves based on their connections.
}
\value{
  A numeric vector representing the group IDs of each stone.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  game_moves <- data.frame(
    row = c(1, 1, 2, 2),
    column = c(1, 2, 1, 2),
    group_id = c("A", "B", "A", "C")
  )
  group_ids <- id_groups(game_moves)
  print(group_ids)
}
