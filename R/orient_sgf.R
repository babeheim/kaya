
reflect_45 <- function(x) t(x)

reflect_y <- function(x) x[,ncol(x):1]

reflect_x <- function(x) x[nrow(x):1,]

reflect_315 <- function(x) {
  x |> reflect_y() |> reflect_x() |> reflect_45()
}

rotate_n90 <- function(x) {
  x |> reflect_315() |> reflect_y()
}

rotate_90 <- function(x) {
  x |> reflect_x() |> reflect_45()
}

rotate_180 <- function(x) {
  x |> rotate_90() |> rotate_90()
}


apply_orientations <- function(board_matrix) {

  out <- cbind(
    as.vector(board_matrix),
    as.vector(reflect_315(board_matrix)),
    as.vector(rotate_90(board_matrix)),
    as.vector(reflect_y(board_matrix)),
    as.vector(rotate_180(board_matrix)),
    as.vector(reflect_45(board_matrix)),
    as.vector(rotate_n90(board_matrix)),
    as.vector(reflect_x(board_matrix))
  )

  return(out)

}

generate_board_sectors <- function(board_size) {

  cols <- rep(1:board_size, each = board_size)
  rows <- rep(1:board_size, times = board_size)

  board_sectors <- matrix(NA, ncol = board_size, nrow = board_size)

  mid <- (board_size + 1) / 2

  # the center point has no sector
  tar <- which(cols == mid & rows == mid)
  board_sectors[tar] <- 0

  tar <- which(cols > mid & rows < mid & ((board_size + 1) - rows) < cols)
  board_sectors[tar] <- 1
  tar <- which(cols > mid & rows < mid & ((board_size + 1) - rows) > cols)
  board_sectors[tar] <- 2
  tar <- which(cols < mid & rows < mid & rows < cols)
  board_sectors[tar] <- 3
  tar <- which(cols < mid & rows < mid & rows > cols)
  board_sectors[tar] <- 4
  tar <- which(cols < mid & rows > mid & rows < ((board_size + 1) - cols))
  board_sectors[tar] <- 5
  tar <- which(cols < mid & rows > mid & rows > ((board_size + 1) - cols))
  board_sectors[tar] <- 6
  tar <- which(cols > mid & rows > mid & rows > cols)
  board_sectors[tar] <- 7
  tar <- which(cols > mid & rows > mid & rows < cols)
  board_sectors[tar] <- 8

  # diagonals
  tar <- which(cols > mid & rows > mid & rows == cols)
  board_sectors[tar] <- 7
  tar <- which(cols < mid & rows > mid & ((board_size + 1) - rows) == cols)
  board_sectors[tar] <- 5
  tar <- which(cols < mid & rows < mid & rows == cols)
  board_sectors[tar] <- 3
  tar <- which(cols > mid & rows < mid & ((board_size + 1) - rows) == cols)
  board_sectors[tar] <- 1

  # central rows and columns
  tar <- which(cols == mid & rows != mid & rows > cols)
  board_sectors[tar] <- 7
  tar <- which(cols != mid & rows == mid & rows > cols)
  board_sectors[tar] <- 5
  tar <- which(cols == mid & rows != mid & rows < cols)
  board_sectors[tar] <- 3
  tar <- which(cols != mid & rows == mid & rows < cols)
  board_sectors[tar] <- 1

  return(board_sectors)

}

# # spot-check
# generate_board_sectors(3)
# generate_board_sectors(9)
# generate_board_sectors(19)


orient_sgf <- function (game_coord_sgf, board_size = 19) {

  if (!all(is.na(nchar(game_coord_sgf)) | nchar(game_coord_sgf) == 2)) {
    stop("input is not a vector of sgf coordinates")
  }

  # we only consider odd-numbered boards for orientation
  stopifnot(board_size %% 2 == 1)

  board_liberties <- matrix(1:(board_size^2), ncol = board_size)

  board_orientations <- apply_orientations(board_liberties)

  board_sectors <- generate_board_sectors(board_size)

  sector_orientations <- apply_orientations(board_sectors)

  sgf_addresses <- c(letters, LETTERS)
  board_coord_sgf <- paste0(
    rep(sgf_addresses[1:board_size], each = board_size),
    rep(sgf_addresses[1:board_size], times = board_size)
  )

  if (!all(is.na(game_coord_sgf) | game_coord_sgf %in% board_coord_sgf)) warning("not all game moves on the board")

  link <- match(game_coord_sgf, board_coord_sgf)
  game_orientations <- board_orientations[link, , drop = FALSE]
  game_sector_orientations <- sector_orientations[link, , drop = FALSE]
  # drop = FALSE necessary to guarantee its still a matrix

  # determine best orientation: so long as there are > 1 remaining orientations, keep filtering to the *lowest* sector option for each 'next_move'
  remaining <- 1:8
  next_move <- 1
  while (length(remaining) > 1 & next_move <= length(game_coord_sgf)) {
    if (!all(is.na(game_sector_orientations[next_move,]))) {
      keep <- which(game_sector_orientations[next_move, remaining] == min(game_sector_orientations[next_move, remaining]))
      remaining <- remaining[keep]
    }
    next_move <- next_move + 1
  }
  # take the first of the remaining options (if more than one)
  remaining <- remaining[1]

  # ##########
  # diagnostic plotter
  # board_cols <- rep(1:board_size, each = board_size)
  # board_rows <- (-1) * rep(1:board_size, times = board_size)

  # par(mfrow = c(2, 4))

  # for (i in 1:8) {
  #   plot(NULL, xlim = c(1, board_size), ylim = -c(board_size, 1), xlab = "", ylab = "", frame.plot = FALSE, axes = FALSE)
  #   abline(h = -(1:board_size), col = gray(0.8))
  #   abline(v = (1:board_size), col = gray(0.8))
  #   text(board_cols[game_orientations[,i]], board_rows[game_orientations[,i]], labels = 1:nrow(game_orientations), col = ifelse(i == remaining, "red", "black"))
  # }
  # ##########

  game_coord_sgf <- board_coord_sgf[game_orientations[,remaining]]

  return(game_coord_sgf)

}
