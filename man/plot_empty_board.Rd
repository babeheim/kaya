\name{plot_empty_board}
\alias{plot_empty_board}
\title{Plot Empty Go Board}
\description{
  Plot an empty Go board with optional star points.
}
\usage{
  plot_empty_board(board_size = 19, goban.color = "darkgoldenrod1", line.color = "black")
}
\arguments{
  \item{board_size}{An integer specifying the size of the Go board. Default is 19.}
  \item{goban.color}{A character string indicating the color of the Go board. Default is "darkgoldenrod1".}
  \item{line.color}{A character string indicating the color of the board lines. Default is "black".}
}
\details{
  This function plots an empty Go board with optional star points based on the board size.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  plot_empty_board(board_size = 13, goban.color = "lightgrey", line.color = "red")
}
