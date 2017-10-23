
\name{read_sgf}
\alias{read_sgf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read a Smart Game Format (SGF) file}
\description{
  Reads a plaintext file containing Go game information in the Smart Game Format (SGF).
}
\usage{
read_sgf( file, rotate=TRUE )
}
\arguments{
    \item{file}{the path to a plaintext file containing one or more chess game records in SGF format}
    \item{rotate}{whether to transform the game moves into a "standard orientation", see notes}
}
\details{
    The read_sgf function is a wrapper for a readLines call to the file location, followed by the parse_sgf function which does the work.

    The resulting output will be a named list, containing all the metadata as well as a block of move stones, and a block of setup stones, if specified.

    By default, the games will be transformed into "standard orientation" by rotate=TRUE. In the SGF format, there is no systematic way to record which sides of the Goban the two players were sitting, so two different game records may contain the exact same moves, but rotated.

    To identify duplicated games that contain the same information, we standardize by rotating all game moves about one of the horizonal, vertical, or diagonal axes of the board until the first game move not on a diagonal falls into the upper right quadrant, below the diagonal. If rotate=FALSE, or every game move is on a diagonal, read_sgf will store the moves exactly as they appear in the SGF file.

    This will may cause the comments to be slightly wrong, if they use sgf notation inside the comment (e.g. "better to play at 'aa'").
}
%- maybe also 'usage' for other objects documented here.
\references{}
\author{Bret Beheim}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }