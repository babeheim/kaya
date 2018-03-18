


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

    # seperate and process metadata
    metadata <- sgf_lines[1]
    metadata <- gsub("\\]", "\\]~tb~", metadata)
    metadata <- gsub("\\]~tb~\\[", "\\]\\[", metadata)
    metadata <- strsplit(metadata, "~tb~")[[1]]
    meta <- list()
    for (i in 1:length(metadata)){
      # if is tag
      tag <- extract_sgf_tag(metadata[i])
      meta <- c(meta, tag)
    }

    moves <- data.frame(number = integer(), color = character(),
      coord_sgf = character(), comment = character())
    
    # organize setup stones, if any 
    if ("AB" %in% names(meta)){
      ab_coord_sgf <- meta$AB
       # minus 20? assumes board is 19x19
      ab_number <- rep(0, length(ab_coord_sgf))
      ab_color <- rep("black", length(ab_coord_sgf))
      ab_comment <- rep("", length(ab_coord_sgf))
      ab_setup_moves <- data.frame(number = ab_number, color = ab_color,
        coord_sgf = ab_coord_sgf, comment = ab_comment, 
        stringsAsFactors = FALSE)
      moves <- rbind(ab_setup_moves, moves)
    }
    if ("AW" %in% names(meta)){
      aw_coord_sgf <- meta$AW
       # minus 20? assumes board is 19x19
      aw_number <- rep(0, length(aw_coord_sgf))
      aw_color <- rep("white", length(aw_coord_sgf))
      aw_comment <- rep("", length(aw_coord_sgf))
      aw_setup_moves <- data.frame(number = aw_number, color = aw_color,
        coord_sgf = aw_coord_sgf, comment = aw_comment, 
        stringsAsFactors = FALSE)
      moves <- rbind(aw_setup_moves, moves)
    }
    # seperate and process game moves, if present
    if (length(sgf_lines) > 1){
      move_stuff <- sgf_lines[2:length(sgf_lines)]
      comment <- rep("", length(move_stuff))
      moves_with_comment <- grep("C\\[", move_stuff)
      if (length(moves_with_comment) > 0){
        move_stuff <- stringi::stri_trans_general(move_stuff, "latin-ascii")
        # convert non-ASCII to closest ascii
        move_stuff <- gsub("[\x01-\x1F]", "", move_stuff)
        # takes care of non-printing ASCII
        move_stuff <- iconv(move_stuff, "latin1", "ASCII", sub = "")
        # strip out non-ASCII entirely
        # slow!
        comment <- substr(move_stuff, 6, nchar(move_stuff))
        comment[moves_with_comment] <- sapply(comment[moves_with_comment],
          function(z) as.character(extract_sgf_tag(z)))
        comment <- as.character(comment)
      }
      coord_sgf <- substr(move_stuff, 1, 5) # strip out comment, if any
      color <- substr(coord_sgf, 1, 1)
      color[color == "B"] <- "black"
      color[color == "W"] <- "white"
      coord_sgf <- sapply(coord_sgf, function(z)
       as.character(extract_sgf_tag(z)))

      coord_sgf <- as.character(coord_sgf)
      # minus 20? assumes board is 19x19
      move_number <- 1:length(coord_sgf)
      game_moves <- data.frame(number = move_number, color, coord_sgf,
       comment, stringsAsFactors = FALSE)
      # hash must be a function of colors and moves only!
      moves <- rbind(moves, game_moves)
    }

    hash_id <- NA
    n_moves <- 0

    if(nrow(moves) > 0){
      if (rotate == TRUE) moves$coord_sgf <- orient_sgf(moves$coord_sgf)
      moves$column <- match(substr(moves$coord_sgf, 1, 1), letters)
      moves$row <- match(substr(moves$coord_sgf, 2, 2), letters)
      moves <- moves[, c("number", "color", "column", "row", "comment")]
      hash_id <- substr(digest::sha1(
        moves[, c("number", "column", "row")]), 1, 19)
      n_moves <- max(moves$number)
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
