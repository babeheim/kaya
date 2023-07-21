\name{count_liberties}
\alias{count_liberties}
\title{Count Liberties of Stones}
\description{
  Count the liberties of stones in a game.
}
\usage{
  count_liberties(moves, board_size = 19)
}
\arguments{
  \item{moves}{A data frame containing game move information with columns: row, column, and group_id.}
  \item{board_size}{An integer specifying the board size. Default is 19.}
}
\details{
  This function counts the liberties of stones in a game based on the given move information and board size.
}
\value{
  A numeric vector representing the liberties of each stone.
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
  liberties <- count_liberties(game_moves)
  print(liberties)
}
