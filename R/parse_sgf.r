

parse_sgf <- function(sgf_lines){

  sgf_lines <- paste(sgf_lines, collapse="\n")
  sgf_lines <- gsub("\n", "", sgf_lines)

  sgf_lines <- strsplit(sgf_lines, ";")

  metadata <- sgf_lines[[1]][2]

  metadata <- gsub("\\]","\\]~split~", metadata)
  metadata <- gsub("\\]~split~\\[","\\]\\[", metadata)
  metadata <- strsplit(metadata, "~split~")[[1]]

  move_string <- sgf_lines[[1]][3:length(sgf_lines[[1]])]
  move_string <- gsub("\\)$", "", move_string)

  moves <- substr(move_string, 1, 5) # strip out comments, if any

  hash_id <- substr(digest::sha1(moves), 1, 19)

  move <- 1:length(moves)
  color <- substr(moves, 1, 1)
  coord_sgf <- substr(moves, 3, 4)

  moves <- data.frame(move, color, coord_sgf)

  meta <- list()
  for(i in 1:length(metadata)){
    # if is tag
    tag <- extract_sgf_tag(metadata[i])
    meta <- c(meta, tag)
  }
  
  output <- meta
  output$hash_id <- hash_id
  output$n_moves <- length(move)
  output$moves <- moves

  # need to rotate game into standard position before assigning the hash... 
  # need to consider multiple () inside one sgf file
  # need to consider branching sgf files
  return(output)

}

