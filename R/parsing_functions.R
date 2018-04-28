

check_comment_escapes <- function(string) {
  n_left_brackets <- length(gregexpr("(?<!\\\\)\\]", string, perl = TRUE)[[1]])
  n_right_brackets <- length(gregexpr("(?<!\\\\)\\[", string, perl = TRUE)[[1]])
  if(n_left_brackets != n_right_brackets) stop("sgf seems invalid; square brackets don't balance, must fix first")
#  comment_pattern <- "\\[(?>[^\\[\\]]|(?R))*\\]"
#  comment_pattern <- "\\[((?>[^\\[\\]]+)|(?R))*\\]"
  bracket_pattern <- "\\[((?>\\\\\\[|\\\\\\]|[^\\[\\]])|(?R))*\\]"
  check <- gregexpr(bracket_pattern, string, perl = TRUE)
  if (check[[1]][1]!="-1") {
    corrected <- regmatches(string, check)
    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\(", "\\\\(", z, perl = TRUE))
    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\)", "\\\\)", z, perl = TRUE))
    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\](?!$)", "\\\\]", z, perl = TRUE))
    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\|^)\\[", "\\\\[", z, perl = TRUE))
    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\;", "\\\\;", z, perl = TRUE))
    regmatches(string, check) <- corrected
  }
  return(string)
}



split_tag <- function(tag_string) {
  if(length(tag_string) != 1) stop("split_tag can only accept individual strings")
  output <- list()
  tag_string <- strsplit(tag_string, "(?<!\\\\)\\[|(?<!\\\\)\\](?<!\\\\)\\[|(?<!\\\\)\\]", perl = TRUE)[[1]]
  tag_string <- stringi::stri_trans_general(tag_string, "latin-ascii")
  tag_string <- gsub("[\x01-\x1F]", "", tag_string)
  # takes care of non-printing ASCII
  tag_string <- gsub(" *$|^ *", "", tag_string)
  if(tag_string[1] == "") stop("input tag is improper, cannot locate key/value pair")
  tag_string[tag_string == ""] <- NA
  return(tag_string)
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

split_sgf <- function(sgf_string) {
  output <- list()
  node_pattern <- "(^.*?(?<!\\\\))(\\(|$)" 
  # group from start of line to first unescaped (, or end of line
  m <- gregexpr(node_pattern, sgf_string, perl = TRUE)
  node_data <- regmatches(sgf_string, m)[[1]]
  output$nodes <- gsub("\\($", "", node_data)
  parenthesis_pattern <- "\\(((?>=\\\\\\(|\\\\\\)|[^\\(\\)])|(?R))*\\)"
  # group by outer parentheses pairs except escaped pairs
  m <- gregexpr(parenthesis_pattern, sgf_string, perl = TRUE)
  if (m[[1]][1] != (-1)) {
    branch_data <- as.list(regmatches(sgf_string, m)[[1]])
    branch_data <- lapply(branch_data, function(z) gsub("\\)$", "", z))
    branch_data <- lapply(branch_data, function(z) gsub("^\\(", "", z))
    output$branches <- branch_data
  }
  return(output)
}

# takes multiple games as inputs and breaks them up!
split_gametree <- function(gametree) {
  parenthesis_pattern <- "\\(((?>=\\\\\\(|\\\\\\)|[^\\(\\)])|(?R))*\\)"
  m <- gregexpr(parenthesis_pattern, sgf_string, perl = TRUE)
  game_data <- as.list(regmatches(sgf_string, m)[[1]])
  return(game_data)
}


# I can probably make a better version of this where the parenthesis don't need
# to be turned into curly races

# split, which breaks one string of text into multiple pieces; 
# split_sgf takes an sgf_string and returns an object with $nodes and $branches, each of which is also the same kind of object
# split_branch takes a branch string and returns vector of nodes,
# split_node takes a node string and breaks it up into a list of tag strings
# split_tag takes the tag string and breaks it up into a key and value

# from these workhorse functions, we have the parse functions:
# parse_tag takes a vector of tag strings (from split_node), applies split_tag, and turns it into a named list
# parse_node takes a branch (from split_sgf), applies split_node, and then runs parse_tag on each node, creating an unnamed lits of nodes
# parse_sgf takes an sgf_file, applies split_sgf, and then runs parse_node on each output

# (?<!\\)\[|(?<!\\)\](?<!\\)\[|(?<!\\)\]
# this identifies the locations of three patterns, [, ] and [], with a lookbehind to ignore escapes
# we simply split the string at each location and we're good!

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
  if(any(nchar(names(output)) > 2)) stop("SGF tags are invalid; they contain too many characters")
  return(output)
}

parse_node <- function(node_string) {
  if(length(node_string) > 1) stop("parse_node accepts only single strings")
  tag_vec <- split_node(node_string)
  output <- list()
  for(i in 1:length(tag_vec)) output <- c(output, parse_tag(tag_vec[i]))
  return(output)
}

parse_branch <- function(branch_string) {
  if(length(branch_string) != 1) stop("parse_branch accepts only single strings")
  node_vec <- split_branch(branch_string)
  output <- list()
  for(i in 1:length(node_vec)) output[[i]] <- parse_node(node_vec[i])
  return(output)
}

parse_sgf <- function(sgf_string, to.json = FALSE) {
  sgf_string <- check_comment_escapes(sgf_string)
  if (length(sgf_string) > 1) stop("parse_sgf accepts only single strings")
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
