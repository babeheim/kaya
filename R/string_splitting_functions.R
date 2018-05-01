
# this doesnt work because the left bracket can exist un-escaped in valid comments, and must not be parsed
split_tag_old <- function(tag_string) {
  if(length(tag_string) != 1) stop("split_tag can only accept individual strings")
  tag_string <- strsplit(tag_string, "(?<!\\\\)\\[|(?<!\\\\)\\](?<!\\\\)\\[|(?<!\\\\)\\]", perl = TRUE)[[1]]
  tag_string <- stringi::stri_trans_general(tag_string, "latin-ascii")
  tag_string <- gsub("[\x01-\x1F]", "", tag_string)
  # takes care of non-printing ASCII
  tag_string <- gsub(" *$|^ *", "", tag_string)
  if(tag_string[1] == "") stop("input tag is improper, cannot locate key/value pair")
  tag_string[tag_string == ""] <- NA
  return(tag_string)
}



split_tag <- function(tag_string){
  if(length(tag_string) != 1) stop("split_tag can only accept individual strings")
  bracket_locations <- "\\[(.*?)(?<!\\\\)\\]"
  m <- gregexpr(bracket_locations, tag_string, perl = TRUE)
  tag_content <- regmatches(tag_string, m)[[1]]
  tag_content <- gsub("^\\[|\\]$", "", tag_content)
  tag_name <- substr(tag_string, 1, regexpr("\\[", tag_string) - 1)
  if(tag_name == "") stop("input tag is improper, cannot locate key/value pair")
  tag_content <- c(tag_name, tag_content)
  return(tag_content)
}

split_node <- function(node_string) {
  if(length(node_string) != 1) stop("split_node can only accept individual strings")
  node_string <- gsub(" *$|^ *", "", node_string)
  node_string <- gsub("(?<!\\\\)\\] *\\[", "\\]\\[", node_string, perl = TRUE)
  node_string <- gsub("(?<!\\\\)\\]", "\\]~tb~", node_string, perl = TRUE)
  node_string <- gsub("(?<!\\\\)\\]~tb~\\[", "\\]\\[", node_string, perl = TRUE)
  output <- strsplit(node_string, "~tb~")[[1]]
  return(output)
}

split_branch <- function(branch_string) {
  output <- strsplit(branch_string, "(?<!\\\\);", perl = TRUE)[[1]]
  # split using un-escaped semicolons
  if(length(output) == 0) stop("branch contains no valid nodes")
  if(output[1] == "") output <- output[-1]
  return(output)
}

split_tree <- function(tree_string) {
  tree_string <- gsub(" *$|^ *", "", tree_string)
  output <- list()
  node_pattern <- "(^.*?(?<!\\\\))(\\(|$)" 
  # group from start of line to first unescaped (, or end of line
  m <- gregexpr(node_pattern, tree_string, perl = TRUE)
  node_data <- regmatches(tree_string, m)[[1]]
  output$nodes <- gsub("\\($", "", node_data)
  subtree_data <- as.list(group_parentheses(tree_string))
  if (length(subtree_data) > 0) {
    output$branches <- subtree_data
  }
  return(output)
}

# takes multiple games as inputs and breaks them up!
split_sgf <- function(gametree) {
  parenthesis_pattern <- "\\(((?>=\\\\\\(|\\\\\\)|[^\\(\\)])|(?R))*\\)"
  m <- gregexpr(parenthesis_pattern, tree_string, perl = TRUE)
  game_data <- as.list(regmatches(tree_string, m)[[1]])
  return(game_data)
}