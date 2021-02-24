
read_sgf <- function(file, to.json = FALSE, rotate = TRUE){
  raw <- paste0(readLines(file, warn = FALSE), collapse = "")
  output <- parse_sgf(raw, to.json = to.json)
  if (!to.json) output <- simplify_game(output, rotate = rotate)
  return(output)
}

parse_sgf <- function(x, to.json = TRUE){
  x <- scrub_whitespace(x)
  x <- check_multigame(x)
  x <- escape_characters(x)
  x <- node_to_object(x)
  x <- branch_to_array(x)
  x <- fromJSON(x, simplifyVector = FALSE)
  if (to.json) x <- toJSON(x, pretty = TRUE)
  return(x)
}

check_multigame <- function(array_string){
  x <- array_string
  m <- gregexpr("\\(((?>=\\\\\\(|\\\\\\)|[^\\(\\)])|(?R))*\\)", x, perl = TRUE)
  if (length(regmatches(x, m)[[1]]) > 1) x <- paste0("(", x, ")")
  return(x)
}

scrub_whitespace <- function(sgf_string) {
  x <- sgf_string
  m <- gregexpr("(?<=\\[)((\\\\\\]|[^\\]])*)", x, perl = TRUE)
  outerstuff <- regmatches(x, m, invert = TRUE)
  outerstuff[[1]] <- gsub(" ", "", outerstuff[[1]])
  regmatches(x, m, invert = TRUE) <- outerstuff
  return(x)
}

escape_characters <- function(sgf_string) {
  x <- sgf_string
  m <- gregexpr("(?<=\\[)((\\\\\\]|[^\\]])*)", x, perl = TRUE)
  values <- regmatches(x, m)
  values[[1]] <- gsub("(?<!\\\\)\\(", "\\\\(", values[[1]], perl = TRUE)
  values[[1]] <- gsub("(?<!\\\\)\\)", "\\\\)", values[[1]], perl = TRUE)
  values[[1]] <- gsub("\\;", "\\\\;", values[[1]])
  regmatches(x, m) <- values
  return(x)
}

node_to_object <- function(sgf_string) {
  # extract all nodes, defined by `;,` and parse them into an R list, then collapse into JSON format using toJSON2
  x <- sgf_string
  m <- gregexpr(";((\\\\\\(|\\\\\\)|\\\\\\;|[^\\;\\)\\(])+)", x, perl = TRUE)
  nodes <- regmatches(x, m)
  for (i in 1:length(nodes[[1]])){
    temp <- paste(parse_node(nodes[[1]][i]), collapse = ", ")
    temp <- paste0("{", temp, "}")
    nodes[[1]][i] <- temp
  }
  regmatches(x, m) <- nodes # all nodes should be in JSON object notation now
  x <- gsub(",\\)", "\\)", x)  # to clean up last object in array
  x <- gsub("\\}\\(", "\\},\\(", x) # object then array 
  x <- gsub("\\}\\{", "\\},\\{", x) # object then object
  x <- gsub("\\\\;", "\\;", x) # object then object
  x <- gsub("\\\\:", "\\:", x) # object then object
  return(x)
}

branch_to_array <- function(sgf_string) {
  # after `escape_characters()`, unescaped `)` only exist to show branch locations
  # so here we turn them into array brackets, `[` and `]`
  # however, this must be run *after* `node_to_object()`, because SGF tags also use square brackets
  x <- sgf_string
  x <- gsub("(?<!\\\\)\\(", "\\[", x, perl = TRUE)
  x <- gsub("(?<!\\\\)\\)", "\\]", x, perl = TRUE)
  x <- gsub("\\\\\\(", "(", x) # de-escape parentheses, if any, they must be in comments
  x <- gsub("\\\\\\)", ")", x) # de-escape parentheses, if any, they must be in comments
  x <- gsub("\\]\\[", "\\],\\[", x) ## ensure commas separating arrays are correct
  x <- gsub("\\\\", "\\", x) # de-escape everything else
  return(x)
}

parse_node <- function(node_string) {
  if (length(node_string) > 1) stop("parse_node accepts only single strings")
  node_string <- gsub("^;", "", node_string)
  tag_vec <- split_node(node_string)
  output <- list()
  for (i in 1:length(tag_vec)) output <- c(output, parse_tag(tag_vec[i]))
  return(output)
}

split_node <- function(node_string) {
  if (length(node_string) != 1) stop("split_node can only accept individual strings")
  node_string <- gsub(" *$|^ *", "", node_string)
  m <- gregexpr("(\\]\\[|\\\\](?!=\\w)|[^\\]])*?\\](?=\\w|$)", node_string, perl = TRUE)
  output <- regmatches(node_string, m)[[1]]
  return(output)
}

parse_tag <- function(tag_data, strict = FALSE) {
  output <- list()
  if (length(tag_data) == 1){
    sgf_tag <- split_tag(tag_data)
    output <- sgf_tag[2:length(sgf_tag)]
    if (length(output) == 1) output <- paste0("\"", output, "\"")
    if (length(output) > 1) output <- as.character(toJSON(output))
    output <- paste(paste0("\"", sgf_tag[1], "\":"), output) # might have trouble here
  } else {
    for (i in 1:length(tag_data)) output <- c(output, parse_tag(tag_data[i]))
  }
  if (any(duplicated(names(output)))) stop("duplicated tags in the same node")
  if (strict) if (any(nchar(names(output)) > 2)) stop("SGF tags are invalid; they contain too many characters")
  return(output)
}

split_tag <- function(tag_string) {
  if (length(tag_string) != 1) stop("split_tag can only accept individual strings")
  bracket_locations <- "(?<=\\[)((\\\\\\]|[^\\]])*)"
  m <- gregexpr(bracket_locations, tag_string, perl = TRUE)
  tag_content <- regmatches(tag_string, m)[[1]]
  tag_name <- substr(tag_string, 1, regexpr("\\[", tag_string) - 1)
  if (tag_name == "") stop("input tag is improper, cannot locate key/value pair")
  tag_content <- c(tag_name, tag_content)
  return(tag_content)
}
