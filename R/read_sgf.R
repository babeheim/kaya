

read_sgf <- function(sgf_file, ...){
  if(length(sgf_file)!=1) stop("only one path allowed")
  raw <- readLines(sgf_file)
  output <- parse_sgf(raw, ...)
  if(length(output)>0){
    if(is.null(names(output))){  # condition true if output is an unnamed list
      for(i in 1:length(output)) output[[i]]$filename <- sgf_file
    } else {
      output$filename <- sgf_file
    }
  }
  return(output)
}