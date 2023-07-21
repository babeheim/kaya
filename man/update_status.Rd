\name{update_status}
\alias{update_status}
\title{Update Status of Game Moves}
\description{
  Update the status of game moves, including group identities and liberties.
}
\usage{
  update_status(game_moves, viz = FALSE)
}
\arguments{
  \item{game_moves}{A data frame containing game move information with columns: number, row, column, color, and group_id.}
  \item{viz}{A logical value indicating whether to visualize the game moves.}
}
\details{
  This function updates the status of game moves, including group identities and liberties. It checks for collisions, updates group identities, removes enemy groups with 0 liberties, and checks for suicides.
}
\value{
  A character vector representing the group identities of game moves after updating.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  game_moves <- data.frame(
    number = c(1, 2, 3, 4),
    row = c(1, 2, 2, 1),
    column = c(1, 2, 1, 2),
    color = c("black", "white", "black", "white"),
    group_id = c("A", "B", "C", "D")
  )
  updated_status <- update_status(game_moves)
  print(updated_status)
}
