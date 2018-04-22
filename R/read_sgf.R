
read_sgf <- function(sgf_file, simplify = TRUE, to.json = FALSE, ...) {
  if (length(sgf_file) != 1) stop("only one path allowed")
  raw <- paste0(readLines(sgf_file, warn = FALSE), collapse = "")
  scrubbed <- check_comment_escapes(raw)
  output <- parse_sgf(scrubbed, to.json = to.json)
  if(to.json) simplify <- FALSE
  if(simplify) output <- simplify_game(output, ...)
  return(output)
}
