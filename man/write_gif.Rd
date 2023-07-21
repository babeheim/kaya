\name{write_gif}
\alias{write_gif}
\title{Write Animated GIF}
\description{
  Write an animated GIF from game data using a series of PNG files.
}
\usage{
  write_gif(game_object, file, number = FALSE, delay = 50, n_loops = 0, start = NA, stop = NA)
}
\arguments{
  \item{game_object}{An object containing game data to be animated.}
  \item{file}{A character string containing the file path of the animated GIF to be written.}
  \item{number}{A logical value indicating whether to display move numbers on the animation. Default is \code{FALSE}.}
  \item{delay}{An integer specifying the delay between frames in milliseconds. Default is 50.}
  \item{n_loops}{An integer specifying the number of times the animation should loop. Use 0 for infinite looping. Default is 0.}
  \item{start}{An optional integer specifying the starting move for the animation. Default is \code{NA} (start from the first move).}
  \item{stop}{An optional integer specifying the ending move for the animation. Default is \code{NA} (end at the last move).}
}
\details{
  This function creates an animated GIF from game data using a series of PNG files. It allows customization of the animation with options for move numbers, frame delay, and looping.
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
  write_gif(game_data, "game_animation.gif")
}
