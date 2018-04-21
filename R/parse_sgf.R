
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
