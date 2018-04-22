
simplify_game <- function(game_list, rotate = TRUE) {

  meta <- list()
  moves <- data.frame(number = integer(), color = character(),
      coord_sgf = character())

  has_meta <- !all(names(game_list$nodes[[1]]) %in% c("B", "W"))
  if (has_meta) {
    meta <- game_list$nodes[[1]]
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
  }

  has_moves <- any(unlist(lapply(game_list$nodes, function(z) any(names(z) %in% c("B", "W")))))
  if (has_moves) {
    game_moves <- simplify_move_nodes(game_list)
    game_moves$number <- 1:nrow(game_moves)
    moves <- rbind(moves, game_moves)
    hash_id <- NA
    n_moves <- 0
    if (nrow(moves) > 0) {
      moves$coord_sgf[is.na(moves$coord_sgf)] <- "tt"
      trans_coord_sgf <- moves$coord_sgf
      if (rotate == TRUE) trans_coord_sgf <- orient_sgf(moves$coord_sgf)
      # do i need to subtract from 20?
      moves$column <- match(substr(trans_coord_sgf, 1, 1), letters[1:19])
      moves$row <- match(substr(trans_coord_sgf, 2, 2), letters[1:19])
      moves <- moves[, c("color", "coord_sgf", "number", "column", "row")]
      meta$hash_id <- substr(digest::sha1(moves[, c("column", "row")]), 1, 19)
      meta$n_moves <- nrow(moves)
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
