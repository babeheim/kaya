

read_sgf <- function(file){
  if(length(file)==1){
    raw <- readLines(file)
    output <- parse_sgf(raw)
  } else {
    output <- sapply_pb(file, read_sgf)
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

extract_sgf_tag <- function(sgf_tag){
  if(length(sgf_tag) != 1) stop("sgf tag improper")
  sgf_tag <- strsplit(sgf_tag, "\\[|\\]\\[|\\]")[[1]]
  sgf_tag <- stringi::stri_trans_general(sgf_tag, "latin-ascii") # convert non-ASCII to closest ascii
  sgf_tag <- gsub("[\x01-\x1F]", "", sgf_tag) # takes care of non-printing ASCII
  sgf_tag <- iconv(sgf_tag, "latin1", "ASCII", sub="") # strip out non-ASCII entirely
  output <- list()
  output[[1]] <- sgf_tag[2:length(sgf_tag)]
  names(output) <- sgf_tag[1] # might have trouble here
  return(output)
}
