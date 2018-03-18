


parse_sgf <- function(sgf_lines, rotate = TRUE){
  sgf_lines <- paste(sgf_lines, collapse = "\n")
  sgf_lines <- gsub("\n", "", sgf_lines)
  # if multiple games, and nothing in-between them
  sgf_lines <- gsub("\\)\\(;", "\\)~tb~\\(;", sgf_lines)
  sgf_lines <- strsplit(sgf_lines, "~tb~")[[1]]
  n_games <- length(sgf_lines)
  output <- list()
  if (n_games == 1){
    game_start <- regexpr("\\(;", sgf_lines)[1]
    game_stop <- max(gregexpr(")", sgf_lines)[[1]])
    sgf_lines <- substr(sgf_lines, game_start + 2, game_stop - 1)
    sgf_lines <- gsub("\\];", "\\]~tb~;", sgf_lines)
    sgf_lines <- strsplit(sgf_lines, "~tb~;")[[1]]
    metadata <- sgf_lines[1]
    metadata <- gsub("\\]", "\\]~tb~", metadata)
    metadata <- gsub("\\]~tb~\\[", "\\]\\[", metadata)
    metadata <- strsplit(metadata, "~tb~")[[1]]
    hash_id <- NA
    n_moves <- 0
    moves <- data.frame(number = integer(), color = character(),
      column = integer(), row = integer())
    if (length(sgf_lines) > 1){
      move_stuff <- sgf_lines[2:length(sgf_lines)]
      comment <- rep("", length(move_stuff))
      comment_moves <- grep("C\\[", move_stuff)
      if (length(comment_moves) > 0){
        move_stuff <- stringi::stri_trans_general(move_stuff, "latin-ascii")
        # convert non-ASCII to closest ascii
        move_stuff <- gsub("[\x01-\x1F]", "", move_stuff)
        # takes care of non-printing ASCII
        move_stuff <- iconv(move_stuff, "latin1", "ASCII", sub = "")
        # strip out non-ASCII entirely
        # slow!
        comment <- substr(move_stuff, 6, nchar(move_stuff))
        comment[comment_moves] <- sapply(comment[comment_moves],
          function(z) as.character(extract_sgf_tag(z)))
        comment <- as.character(comment)
      }
      moves <- substr(move_stuff, 1, 5) # strip out comments, if any
      color <- substr(moves, 1, 1)
      color[color == "B"] <- "black"
      color[color == "W"] <- "white"
      coord_sgf <- sapply(moves, function(z)
       as.character(extract_sgf_tag(z)))
      coord_sgf <- as.character(coord_sgf)
      if (rotate == TRUE) coord_sgf <- orient_sgf(coord_sgf)
      move_cols <- match(substr(coord_sgf, 1, 1), letters)
      move_rows <- match(substr(coord_sgf, 2, 2), letters)
      # minus 20? assumes board is 19x19
      number <- 1:length(move_cols)
      moves <- data.frame(number, color, column = move_cols,
       row = move_rows, stringsAsFactors = FALSE)
      n_moves <- nrow(moves)
      # hash must be a function of colors and moves only!
      hash_id <- substr(digest::sha1(moves), 1, 19)
      moves$comments <- comment
    }
    meta <- list()
    for (i in 1:length(metadata)){
      # if is tag
      tag <- extract_sgf_tag(metadata[i])
      meta <- c(meta, tag)
    }
    # extract setup stones and add them to moves as "move 0"
    if ("AB" %in% names(meta)){
      coord_sgf <- meta$AB
      if (rotate == TRUE) coord_sgf <- orient_sgf(coord_sgf)
      setup_move_cols <- match(substr(coord_sgf, 1, 1), letters)
      setup_move_rows <- match(substr(coord_sgf, 2, 2), letters)
       # minus 20? assumes board is 19x19
      number <- rep(0, length(setup_move_cols))
      color <- rep("black", length(setup_move_cols))
      comments <- rep("", length(setup_move_cols))
      setup_moves_black <- data.frame(number, color, column = setup_move_cols,
       row = setup_move_rows, comments, stringsAsFactors = FALSE)
      moves <- rbind(setup_moves_black, moves)
    }
    if ("AW" %in% names(meta)){
      coord_sgf <- meta$AW
      if (rotate == TRUE) coord_sgf <- orient_sgf(coord_sgf)
      setup_move_cols <- match(substr(coord_sgf, 1, 1), letters)
      setup_move_rows <- match(substr(coord_sgf, 2, 2), letters)
       # minus 20? assumes board is 19x19
      number <- rep(0, length(setup_move_cols))
      color <- rep("white", length(setup_move_cols))
      comments <- rep("", length(setup_move_cols))
      setup_moves_white <- data.frame(number, color, column = setup_move_cols,
       row = setup_move_rows, comments, stringsAsFactors = FALSE)
      moves <- rbind(setup_moves_white, moves)
    }
    if (rotate == TRUE){
      meta$kaya_notes <- "rotated game moves to standard orientation"
    }
    output <- meta
    output$hash_id <- hash_id
    output$n_moves <- n_moves
    output$moves <- moves
  }
  if (n_games > 1){
    output <- lapply_pb(sgf_lines, parse_sgf)
  }
  return(output)
}
