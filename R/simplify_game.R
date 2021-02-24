
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

simplify_game <- function(game_list, rotate = TRUE) {
  meta <- list()
  moves <- data.frame(number = integer(), color = character(),
      coord_sgf = character())
  has_meta <- !all(names(game_list[[1]]) %in% c("B", "W"))
  if (has_meta) {
    meta <- game_list[[1]]
    drop <- which(names(meta) %in% c("AB", "AW"))
    if(length(drop) > 0) meta <- meta[-drop]
  }
  # ignore AB and AW
  has_moves <- any(unlist(lapply(game_list, function(z) any(names(z) %in% c("B", "W")))))
  if (has_moves) {
    game_moves <- simplify_move_list(game_list)
    game_moves <- unlist(game_moves)
    color <- names(game_moves)
    color <- substr(color, 1, 2)
    number <- rep(NA, length(game_moves))
    number[color %in% c("AB", "AW")] <- 0
    number[color %in% c("B", "W")] <- 1:sum(color %in% c("B", "W"))
    moves <- data.frame(number = number, coord_sgf = game_moves, color = color, stringsAsFactors = FALSE)
    if (nrow(moves) > 0) {
      moves$coord_sgf[is.na(moves$coord_sgf)] <- "tt"
      trans_coord_sgf <- moves$coord_sgf
      if (rotate == TRUE) trans_coord_sgf <- orient_sgf(moves$coord_sgf)
      # do i need to subtract from 20?
      moves$column <- match(substr(trans_coord_sgf, 1, 1), letters[1:19])
      moves$row <- match(substr(trans_coord_sgf, 2, 2), letters[1:19])
      moves$color <- gsub("AB", "black", moves$color)
      moves$color <- gsub("B", "black", moves$color)
      moves$color <- gsub("AW", "white", moves$color)
      moves$color <- gsub("W", "white", moves$color)
      moves <- moves[, c("number", "color", "coord_sgf", "column", "row")]
      meta$hash_id <- substr(digest::sha1(moves[, c("column", "row")]), 1, 19)
      meta$n_moves <- max(moves$number)
    }
  }
  if (rotate == TRUE) {
    meta$kaya_notes <- "rotated game moves to standard orientation"
  }
  if(!has_meta & !has_moves) stop("not a valid game!")
  output <- meta
  if(nrow(moves) != 0) output$moves <- moves
  if(nrow(moves) == 0) output$n_moves <- 0
  return(output)
}
