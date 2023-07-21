\name{lapply_pb}
\alias{lapply_pb}
\title{Parallel lapply with Progress Bar}
\description{
  A wrapper around \code{\link[base]{lapply}} that shows a progress bar during execution.
}
\usage{
  lapply_pb(X, FUN, ...)
}
\arguments{
  \item{X}{A list or vector on which to apply the function \code{FUN}.}
  \item{FUN}{The function to be applied to each element of \code{X}.}
  \item{\dots}{Additional arguments to be passed to \code{FUN}.}
}
\details{
  This function provides a wrapper around the \code{lapply} function, allowing for parallel execution while displaying a progress bar.
}
\value{
  A list containing the results of applying the function \code{FUN} to each element of \code{X}.
}
\author{
  Bret Beheim \\ \email{bret_beheim@eva.mpg.de}
}
\examples{
  my_list <- list(a = 1:3, b = 4:6, c = 7:9)
  result <- lapply_pb(my_list, sum)
  print(result)
}
