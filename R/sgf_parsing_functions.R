

split_tags <- function(tag_string) {
  if(length(tag_string) != 1) stop("split_tags can only accept individual strings")
  node_string <- tag_string
  node_string <- gsub(" *$|^ *", "", node_string)
  node_string <- gsub("(?<!\\\\)\\] *\\[", "\\]\\[", node_string, perl = TRUE)
  node_string <- gsub("(?<!\\\\)\\]", "\\]~tb~", node_string, perl = TRUE)
  node_string <- gsub("(?<!\\\\)\\]~tb~\\[", "\\]\\[", node_string, perl = TRUE)
  output <- strsplit(node_string, "~tb~")[[1]]
  return(output)
}

split_nodes <- function(branch_string) {
  output <- strsplit(branch_string, "(?<!\\\\);", perl = TRUE)[[1]]
  if(length(output) == 0) stop("branch contains no valid nodes")
  if(output[1] == "") output <- output[-1]
  return(output)
}

# split_branches? 

bracket_matcher <- function(string){
#  nesting_brackets <- "\\((?>[^()]|(?R))*\\)"
  nesting_brackets <- "(?<!\\\\)\\((?>[^()]|(?R))*\\)(?!\\\\)"
  matched <- gregexpr(nesting_brackets, string, perl = TRUE)[[1]]
  return(matched)
}

check_comment_escapes <- function(string) {
  balanced_square <- length(gregexpr("\\]", string)[[1]]) == length(gregexpr("\\[", string)[[1]])
  if(!balanced_square) stop("sgf seems invalid; square brackets don't balance, must fix first")
#  comment_pattern <- "\\[(?>[^\\[\\]]|(?R))*\\]"
  comment_pattern <- "\\[((?>[^\\[\\]]+)|(?R))*\\]"
  check <- gregexpr(comment_pattern, string, perl = TRUE)
  if (check[[1]][1]!="-1") {
    corrected <- regmatches(string, check)
    corrected <- lapply(corrected, function(z) gsub("\\(", "\\\\{", z))
    corrected <- lapply(corrected, function(z) gsub("\\)", "\\\\}", z))
    corrected <- lapply(corrected, function(z) gsub("\\](?!$)", "\\\\]", z, perl = TRUE))
    corrected <- lapply(corrected, function(z) gsub("(?<!^)\\[", "\\\\[", z, perl = TRUE))
    corrected <- lapply(corrected, function(z) gsub("\\;", "\\\\;", z))
    regmatches(string, check) <- corrected
  }
  return(string)
}

parse_tag <- function(tag_data) {
  output <- list()
  if(length(tag_data) == 1){
    sgf_tag <- tag_data
  #  if(grep("\\[(.*)\\]", tag_data) != 1) stop("input tag is improper; no matching square brackets")
#    sgf_tag <- strsplit(sgf_tag, "(?<!\\\\)\\[|(?<!\\\\)\\](?<!\\\\)\\[|(?<!\\\\)\\]")[[1]]
    sgf_tag <- strsplit(sgf_tag, "(?<!\\\\)\\[|(?<!\\\\)\\](?<!\\\\)\\[|(?<!\\\\)\\]", perl = TRUE)[[1]]
    sgf_tag <- stringi::stri_trans_general(sgf_tag, "latin-ascii")
    sgf_tag <- gsub("[\x01-\x1F]", "", sgf_tag)
    # takes care of non-printing ASCII
    sgf_tag <- gsub(" *$|^ *", "", sgf_tag)
    if(sgf_tag[1] == "") stop("input tag is improper, cannot locate key/value pair")
    sgf_tag[sgf_tag == ""] <- NA
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
  tag_vec <- split_tags(node_string)
  output <- list()
  for(i in 1:length(tag_vec)) output <- c(output, parse_tag(tag_vec[i]))
  return(output)
}

parse_branch <- function(branch_string) {
  if(length(branch_string) != 1) stop("parse_branch accepts only single strings")
  node_vec <- split_nodes(branch_string)
  output <- list()
  for(i in 1:length(node_vec)) output[[i]] <- parse_node(node_vec[i])
  return(output)
}

parse_sgf <- function(sgf_string, to.json = FALSE) {
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
