simplify_game <- function(game_list, rotate = TRUE) {
  meta <- game_list$nodes[[1]] # assuming there's a metadata node....
  moves <- data.frame(number = integer(), color = character(),
      coord_sgf = character())
  # organize setup stones, if any 
  if ("AB" %in% names(meta)){
    ab_coord_sgf <- meta$AB
    ab_number <- rep(0, length(ab_coord_sgf))
    ab_color <- rep("black", length(ab_coord_sgf))
    ab_setup_moves <- data.frame(number = ab_number, color = ab_color,
      coord_sgf = ab_coord_sgf, stringsAsFactors = FALSE)
    moves <- rbind(ab_setup_moves, moves)
  }
  if ("AW" %in% names(meta)){
    aw_coord_sgf <- meta$AW
    aw_number <- rep(0, length(aw_coord_sgf))
    aw_color <- rep("white", length(aw_coord_sgf))
    aw_comment <- rep("", length(aw_coord_sgf))
    aw_setup_moves <- data.frame(number = aw_number, color = aw_color,
      coord_sgf = aw_coord_sgf, stringsAsFactors = FALSE)
    moves <- rbind(aw_setup_moves, moves)
  }
  # add moves on from the current nodes and also from nodes on each first branch all the way down
  game_moves <- simplify_move_nodes(game_list)
  game_moves$number <- 1:nrow(game_moves)
  moves <- rbind(moves, game_moves)
  hash_id <- NA
  n_moves <- 0
  if (nrow(moves) > 0) {
    trans_coord_sgf <- moves$coord_sgf
    if (rotate == TRUE) trans_coord_sgf <- orient_sgf(moves$coord_sgf)
    # do i need to subtract from 20?
    moves$column <- match(substr(trans_coord_sgf, 1, 1), letters)
    moves$row <- match(substr(trans_coord_sgf, 2, 2), letters)
    moves <- moves[, c("color", "coord_sgf", "number", "column", "row")]
    hash_id <- substr(digest::sha1(
      moves[, c("column", "row")]), 1, 19)
  }
  if (rotate == TRUE) {
    meta$kaya_notes <- "rotated game moves to standard orientation"
  }
  output <- meta
  output$hash_id <- hash_id
  output$n_moves <- nrow(moves)
  output$moves <- moves
  return(output)
}
