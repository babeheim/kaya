
parse_tree <- function(tree_string, to.json = FALSE) {
  if (length(tree_string) > 1) stop("parse_tree accepts only single strings")
  tree_string <- group_parentheses(tree_string)
  tree_string <- check_comment_escapes(tree_string)
  tree_string <- purge_comments(tree_string)
  tree_string <- gsub(" *$|^ *", "", tree_string)
  tree_string <- gsub("^\\(|\\)$", "", tree_string)
  output <- split_tree(tree_string)
  output$nodes <- parse_branch(output$nodes)
  if ("branches" %in% names(output)) {
    for(i in 1:(length(output$branches))){
      output$branches[[i]] <- parse_tree(output$branches[[i]]) # wow!
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

parse_tag <- function(tag_data, strict = FALSE) {
  output <- list()
  if(length(tag_data) == 1){
    sgf_tag <- split_tag(tag_data)
    output[[1]] <- sgf_tag[2:length(sgf_tag)]
    names(output) <- sgf_tag[1] # might have trouble here
  } else {
    for(i in 1:length(tag_data)) output <- c(output, parse_tag(tag_data[i]))
  }
  if(any(duplicated(names(output)))) stop("duplicated tags in the same node")
  if(strict) if(any(nchar(names(output)) > 2)) stop("SGF tags are invalid; they contain too many characters")
  return(output)
}
