
strip_comments <- function(move_nodes) {
  out <- lapply(move_nodes, function(z) {
    keep <- which(names(z) %in% c("W", "B"))
    z[keep]
  })
  return(out)
}

simplify_move_list <- function(move_list) {
  is_node <- unlist(lapply(move_list, function(z) !is.null(names(z))))
  output <- move_list[is_node]
  output <- lapply(output, node_stripper)
  if (any(!is_node)) {
    first_branch <- simplify_move_list(move_list[[which(!is_node)[1]]])
    output <- c(output, first_branch)
  }
  return(output)
}

node_stripper <- function(game_node) {
  keep <- which(names(game_node) %in% c("AB", "AW", "W", "B"))
  output <- game_node[keep]
  return(output)
}


# takes list of SGF nodes and produces standard, useable R object for the game

simplify_game <- function(game_list, rotate = TRUE) {
  has_meta <- !all(names(game_list[[1]]) %in% c("B", "W"))
  has_moves <- any(unlist(lapply(game_list, function(z) any(names(z) %in% c("B", "W", "AB", "AW")))))
  if (!has_meta & !has_moves) stop("not a valid game!")
  if (has_meta) {
    out <- game_list[[1]]
    drop <- which(names(out) %in% c("AB", "AW"))
    if (length(drop) > 0) out <- out[-drop]
  } else {
    out <- list()
  }
  if (!hasName(out, "SZ")) out$SZ <- "19"
  board_dimensions <- strsplit(out$SZ, ":")[[1]]
  if (length(board_dimensions) == 1) {
    board_cols <- as.numeric(board_dimensions)
    board_rows <- as.numeric(board_dimensions)
    # we don't try to rotate even-numbered boards
    if (board_cols %% 2 == 0) rotate <- FALSE
  } else if (length(board_dimensions) == 2) {
    # the FF[4] sgf specs allow rectangular boards
    board_cols <- as.numeric(board_dimensions[1])
    board_rows <- as.numeric(board_dimensions[2])
    # there is no way to rotate them in this case
    rotate <- FALSE
  } else {
    stop ("invalid board size")
  }
  if (board_rows > 52 | board_cols > 52 | is.na(board_rows) | is.na(board_cols)) stop("invalid board size")
  if (has_moves) {
    game_moves <- simplify_move_list(game_list)
    game_moves <- unlist(game_moves)
    color <- names(game_moves)
    color <- substr(color, 1, 2)
    number <- rep(NA, length(game_moves))
    game_moves <- as.character(game_moves)
    # handicap stones are all 'move 0'
    number[which(color %in% c("AB", "AW"))] <- 0
    number[which(color %in% c("B", "W"))] <- 1:sum(color %in% c("B", "W"))
    stopifnot(!any(is.na(number)))
    out$n_moves <- max(number)
    out$moves <- data.frame(number = number, coord_sgf = game_moves, color = color, stringsAsFactors = FALSE)
    # if has_moves > 0, nrow(moves) should always be >0 right?
    stopifnot(nrow(out$moves) > 0)
    out$moves$color <- gsub("AB", "black", out$moves$color)
    out$moves$color <- gsub("B", "black", out$moves$color)
    out$moves$color <- gsub("AW", "white", out$moves$color)
    out$moves$color <- gsub("W", "white", out$moves$color)
    # does this ever come up? if not remove
    out$moves$coord_sgf[which(out$moves$coord_sgf == "")] <- NA

    if (rotate) {
      out$moves$coord_sgf_original <- out$moves$coord_sgf
      out$moves$coord_sgf <- orient_sgf(out$moves$coord_sgf, board_size = board_cols)
      out$hash_id <- substr(digest::sha1(out$moves$coord_sgf), 1, 19)
      out$kaya_notes <- "rotated game moves to standard orientation"
    }

    board_liberties <- matrix(1:(board_rows * board_cols), ncol = board_cols)

    sgf_addresses <- c(letters, LETTERS)
    board_coord_sgf <- paste0(
      rep(sgf_addresses[1:board_cols], each = board_rows),
      rep(sgf_addresses[1:board_rows], times = board_cols)
    )

    game_liberties <- board_liberties[match(out$moves$coord_sgf, board_coord_sgf)]
    out$moves$row <- (game_liberties - 1) %% board_cols + 1
    out$moves$column <- ((game_liberties - 1) %/% board_rows) + 1

  } else {
    out$n_moves <- 0
    out$moves <- data.frame(number = integer(), color = character(),
      coord_sgf = character())
  }
  return(out)
}
