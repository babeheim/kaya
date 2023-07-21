\name{plot_game}
\alias{plot_game}
\title{Plot Game Moves on Board}
\description{
  Plot game moves on a board to visualize the game's progression.
}
\usage{
  plot_game(game_object, number = FALSE, stop = NA, ...)
}
\arguments{
  \item{game_object}{An object containing game data to be plotted.}
  \item{number}{A logical value indicating whether to display move numbers on the plot. Default is \code{FALSE}.}
  \item{stop}{An optional integer specifying the last move to plot. Default is \code{NA} (plot all moves).}
  \item{...}{Additional graphical parameters passed to the \code{plot} function.}
}
\details{
  This function plots the game moves on a board, providing a visual representation of the game's progression.
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
      number = c(1, 2, 3),
      row = c(1, 2, 3),
      column = c(1, 2, 3),
      color = c("black", "white", "black"),
      group_id = c("A", "B", "A")
    )
  )
  plot_game(game_data, number = TRUE, stop = 2)
}
