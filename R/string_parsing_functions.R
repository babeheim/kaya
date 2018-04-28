
parse_sgf <- function(sgf_string, to.json = FALSE) {
  sgf_string <- check_comment_escapes(sgf_string)
  if (length(sgf_string) > 1) stop("parse_sgf accepts only single strings")
  sgf_string <- gsub(" *$|^ *", "", sgf_string)
  sgf_string <- gsub("^\\(|\\)$", "", sgf_string)
  output <- split_sgf(sgf_string)
  output$nodes <- parse_branch(output$nodes)
  if ("branches" %in% names(output)) {
    for(i in 1:(length(output$branches))){
      output$branches[[i]] <- parse_sgf(output$branches[[i]]) # wow!
    }
  }
  if (to.json) output <- jsonlite::toJSON(output, pretty = TRUE)
  return(output)
}

parse_branch <- function(branch_string) {
  if(length(branch_string) != 1) stop("parse_branch accepts only single strings")
  node_vec <- split_branch(branch_string)
  output <- list()
  for(i in 1:length(node_vec)) output[[i]] <- parse_node(node_vec[i])
  return(output)
}

parse_node <- function(node_string) {
  if(length(node_string) > 1) stop("parse_node accepts only single strings")
  tag_vec <- split_node(node_string)
  output <- list()
  for(i in 1:length(tag_vec)) output <- c(output, parse_tag(tag_vec[i]))
  return(output)
}

parse_tag <- function(tag_data) {
  output <- list()
  if(length(tag_data) == 1){
    sgf_tag <- split_tag(tag_data)
    output[[1]] <- sgf_tag[2:length(sgf_tag)]
    names(output) <- sgf_tag[1] # might have trouble here
  } else {
    for(i in 1:length(tag_data)) output <- c(output, parse_tag(tag_data[i]))
  }
  if(any(duplicated(names(output)))) stop("duplicated tags in the same node")
#  if(any(nchar(names(output)) > 2)) stop("SGF tags are invalid; they contain too many characters")
  return(output)
}



###### old versions


# i suspect I can use regmatches here to clean up this code...
parse_sgf_old <- function(sgf_string, to.json = FALSE) {
  if(length(sgf_string) > 1) stop("parse_sgf accepts only single strings")
  x <- bracket_matcher(sgf_string)
  if(length(x) > 1) stop("string contains more than one game! Kaya is not designed for this, so please separate these first.")
  if(!(x[1] == 1 & attr(x, "match.length")[1] == nchar(sgf_string))) stop("sgf_string is not surrounded by parentheses; this isn't a valid SGF")
  if(length(x) == 1 & x[1] == 1){
    sgf_string <- substr(sgf_string, 2, nchar(sgf_string) - 1)
    x <- bracket_matcher(sgf_string)
  }
  # valid sgf, ok time to process
  output <- list()
  if(x[1] == (-1)) output$nodes <- parse_branch(sgf_string)
  if(x[1] != (-1)){
    right_pars <- c(as.numeric(x))
    left_pars <- c(as.numeric(x + attr(x, "match.length") - 1))
    if(right_pars[1] != 1) output$nodes <- parse_branch(substr(sgf_string, 1, right_pars[1] - 1))
    output$branches <- list()
    for(i in 1:(length(right_pars))){
      output$branches[[i]] <- parse_sgf(substr(sgf_string, right_pars[i], left_pars[i])) # wow!
    }
  }
  if(to.json) output <- jsonlite::toJSON(output, pretty = TRUE)
  return(output)
}
