
# new and improved!

parse_tags <- function(tag_data) {
  if(length(tag_data) == 1){
    sgf_tag <- tag_data
    sgf_tag <- strsplit(sgf_tag, "\\[|\\]\\[|\\]")[[1]]
    sgf_tag <- stringi::stri_trans_general(sgf_tag, "latin-ascii")
    sgf_tag <- gsub("[\x01-\x1F]", "", sgf_tag)
    # takes care of non-printing ASCII
    sgf_tag <- gsub(" *$|^ *", "", sgf_tag)
    output <- list()
    output[[1]] <- sgf_tag[2:length(sgf_tag)]
    names(output) <- sgf_tag[1] # might have trouble here
  } else {
    output <- list()
    for(i in 1:length(tag_data)) output <- c(output, parse_tag(tag_data[i]))
  }
  return(output)
}

split_nodes <- function(branch_string) {
  output <- strsplit(branch_string, ";")[[1]]
  if(output[1] == "") output <- output[-1]
  return(output)
}

parse_nodes <- function(node_data) {
  if(length(node_data) != 1) stop("node data must be a single string")
  node_vec <- split_nodes(node_data)
  if (length(node_vec) == 1){
    node_string <- node_vec
    node_string <- gsub(" *$|^ *", "", node_string)
    node_string <- gsub("\\] *\\[", "\\]\\[", node_string)
    node_string <- gsub("\\]", "\\]~tb~", node_string)
    node_string <- gsub("\\]~tb~\\[", "\\]\\[", node_string)
    tag_data <- strsplit(node_string, "~tb~")[[1]]
    output <- parse_tags(tag_data)
  } else {
    output <- list()
    for(i in 1:length(node_vec)) output[[i]] <- parse_nodes(node_vec[i])
  }
  return(output)
}

bracket_matcher <- function(string){
  matched <- gregexpr("\\((?>[^()]|(?R))*\\)", string, perl = TRUE)[[1]]
  return(matched)
}

parse_sgf <- function(sgf_string, to.json = FALSE) {
  x <- bracket_matcher(sgf_string)
  if(length(x) == 1 & x[1] == 1){
    sgf_string <- substr(sgf_string, 2, nchar(sgf_string) - 1)
    x <- bracket_matcher(sgf_string)
  }
  output <- list()
  if(x[1] == (-1)) output$nodes <- parse_nodes(sgf_string)
  if(x[1] != (-1)){
    right_pars <- c(as.numeric(x))
    left_pars <- c(as.numeric(x + attr(x, "match.length") - 1))
    output <- list()
    if(right_pars[1] != 1) output$nodes <- parse_nodes(substr(sgf_string, 1, right_pars[1] - 1))
    output$branches <- list()
    for(i in 1:(length(right_pars))){
      output$branches[[i]] <- parse_sgf(substr(sgf_string, right_pars[i] + 1, left_pars[i] - 1)) # wow!
    }
  }
  if(to.json) output <- jsonlite::toJSON(output, pretty = TRUE)
  return(output)
}


strip_comments <- function(move_nodes) {
  out <- lapply(move_nodes, function(z) {
    keep <- which(names(z) %in% c("W", "B"))
    z[keep]
  })
  return(out)
}

simplify_move_nodes <- function(branch) {
  move_nodes <- branch$nodes
  move_nodes <- strip_comments(move_nodes)
  coord_sgf <- unlist(move_nodes)
  color <- names(coord_sgf)
  color[color == "B"] <- "black"
  color[color == "W"] <- "white"
  moves <- data.frame(color, coord_sgf, stringsAsFactors = FALSE)
  if ("branches" %in% names(branch)) {
    first_branch <- branch$branches[[1]]
    branch_moves <- simplify_move_nodes(first_branch)
    moves <- rbind(moves, branch_moves)
    # attach branches in the same way!
  }
  return(moves)
}

simplify_game <- function(game_list) {
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

read_sgf <- function(sgf_file, simplify = TRUE, to.json = FALSE, ...) {
  if (length(sgf_file) != 1) stop("only one path allowed")
  raw <- paste0(readLines(sgf_file), collapse = "")
  output <- parse_sgf(raw, to.json = to.json)
  if(to.json) simplify <- FALSE
  if(simplify) output <- simplify_game(output)
  return(output)
}
