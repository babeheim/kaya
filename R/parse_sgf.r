

# test: coordinates have bad values, error!
# test: comments have non-ASCII characters, ok!?
# test: comments have ( )  [ ], supposedly okay but ow can you
# possibly fix??

# theres no solution to the bracket issue, they have to be right

# test: what if somene puts a )( inside a tag?

# test: with funny characters in the tags, e.g. kanji
# test: duplicated keys 

# comments for each move and also for beginning of game...

# error: rounded braces in the comments...)(

# error: corrupted moves!

# test: variable number of games per file
# test: with funny characters in the tags, e.g. kanji
# test: duplicated keys 

# need to rotate game into standard position before assigning the hash... 
# need to consider branching sgf files

parse_sgf <- function(sgf_lines){

  sgf_lines <- paste(sgf_lines, collapse="\n")
  sgf_lines <- gsub("\n", "", sgf_lines)

  # if multiple games, and nothing in-between them
  sgf_lines <- gsub("\\)\\(", "\\)~split~\\(", sgf_lines)
  sgf_lines <- strsplit(sgf_lines, "~split~")[[1]]

  n_games <- length(sgf_lines)
  output <- list()

  if( n_games == 1 ){

    game_start <- regexpr("\\(;", sgf_lines)[1]
    game_stop <- max(gregexpr(")", sgf_lines)[[1]])

    sgf_lines <- substr(sgf_lines, game_start+2, game_stop-1)

    sgf_lines <- strsplit(sgf_lines, ";")[[1]]

    metadata <- sgf_lines[1]

    metadata <- gsub("\\]","\\]~split~", metadata)
    metadata <- gsub("\\]~split~\\[","\\]\\[", metadata)
    metadata <- strsplit(metadata, "~split~")[[1]]

    moves <- data.frame(move=character(), 
      color=character(), coord_sgf=character())
    hash_id <- NA
    n_moves <- 0

    if( length(sgf_lines) > 1 ){

      move_string <- sgf_lines[2:length(sgf_lines)]

      moves <- substr(move_string, 1, 5) # strip out comments, if any

      hash_id <- substr(digest::sha1(moves), 1, 19)

      move <- 1:length(moves)
      color <- substr(moves, 1, 1)

      coord_sgf <- sapply(moves, function(z) as.character(extract_sgf_tag(z)))
      coord_sgf <- as.character(coord_sgf)
  
      moves <- data.frame(move, color, coord_sgf)

      n_moves <- nrow(moves)

    }

    meta <- list()
    for(i in 1:length(metadata)){
      # if is tag
      tag <- extract_sgf_tag(metadata[i])
      meta <- c(meta, tag)
    }
    
    output <- meta
    output$hash_id <- hash_id
    output$n_moves <- n_moves
    output$moves <- moves

  }

  if( n_games > 1 ){

    output <- lapply_pb(sgf_lines, parse_sgf)

  }

  return(output)

}

