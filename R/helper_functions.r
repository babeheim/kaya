
validate_game <- function(game_data){

  coords <- as.character(game_data$moves$coord_sgf)

  coords_invalid <- !all(unlist(strsplit(coords, "")) %in% letters[1:20])

  coords_wronglength <- !all(nchar(coords) %in% c(0, 2))

  duplicate_key <- any(duplicated(names(game_data)))

  long_keys <- unlist(lapply(game_data, length)>1)
  long_keys <- names(long_keys[long_keys])

  long_key_error <- !all(long_keys %in% c("moves", "AB", "AW"))

  output <- !(long_key_error | duplicate_key | coords_wronglength | coords_invalid)

  return(output)

}



write_sgf <- function(game_list, path){
  tags <- names(game_list)
  tags <- tags[-which(tags %in% c("kaya_notes", "hash_id", "n_moves", "moves", "filename"))]
  meta <- paste0( tags, "[", game_list[tags], "]", collapse="" ) 
  movestring <- paste0(game_data[[2]]$moves$color, "[", game_data[[2]]$moves$coord_sgf, "];", collapse="")
  output <- paste0( "(;", meta, movestring, ")")
  writeLines(output, path)
}



read_sgf <- function(sgf_file, ...){
  if(length(sgf_file)!=1) stop("only one path allowed")
  raw <- readLines(sgf_file)
  output <- parse_sgf(raw, ...)
  if(is.null(names(output))){  # condition true if output is an unnamed list
    for(i in 1:length(output)) output[[i]]$filename <- sgf_file
  } else {
    output$filename <- sgf_file
  }
  return(output)
}

sapply_pb <- function(X, FUN, ...){
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)

  wrapper <- function(...){
  curVal <- get("counter", envir = env)
  assign("counter", curVal +1 ,envir=env)
  setTxtProgressBar(get("pb", envir=env), curVal +1)
  FUN(...)
  }
  res <- sapply(X, wrapper, ...)
  close(pb)
  res
}

lapply_pb <- function(X, FUN, ...){
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)

  wrapper <- function(...){
  curVal <- get("counter", envir = env)
  assign("counter", curVal +1 ,envir=env)
  setTxtProgressBar(get("pb", envir=env), curVal +1)
  FUN(...)
  }
  res <- lapply(X, wrapper, ...)
  close(pb)
  res
}


extract_sgf_tag <- function(sgf_tag){
  if(length(sgf_tag) != 1) stop("sgf tag improper")
  sgf_tag <- strsplit(sgf_tag, "\\[|\\]\\[|\\]")[[1]]
  sgf_tag <- stringi::stri_trans_general(sgf_tag, "latin-ascii") # convert non-ASCII to closest ascii
  sgf_tag <- gsub("[\x01-\x1F]", "", sgf_tag) # takes care of non-printing ASCII
  sgf_tag <- iconv(sgf_tag, "latin1", "ASCII", sub="") # strip out non-ASCII entirely
  sgf_tag <- gsub(" *$|^ *", "", sgf_tag)
  output <- list()
  output[[1]] <- sgf_tag[2:length(sgf_tag)]
  names(output) <- sgf_tag[1] # might have trouble here
  return(output)
}
