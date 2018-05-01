
read_sgf <- function(sgf_file, simplify = TRUE, to.json = FALSE, ...) {
  if (length(sgf_file) != 1) stop("only one path allowed")
  raw <- paste0(readLines(sgf_file, warn = FALSE), collapse = "")
  output <- parse_tree(raw, to.json = to.json)
  if(to.json) simplify <- FALSE
  if(simplify){
    if(!is.null(names(output))) {
      output <- simplify_game(output, ...)
    } else {
      output <- lapply(output, function(z) simplify_game(z, ...))
    }
  }
  return(output)
}
