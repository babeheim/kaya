

purge_comments <- function(escaped_string) {
  comment_groups <- gregexpr("(?<![a-zA-Z])C\\[.*?(?<!\\\\)\\]", escaped_string, perl = TRUE)
  regmatches(escaped_string, comment_groups) <- ""
  return(escaped_string)
}

check_comment_escapes <- function(string) {
  bracket_pattern <- "\\[(.*?)(?<!\\\\)\\]"
  check <- gregexpr(bracket_pattern, string, perl = TRUE)
  if (check[[1]][1]!="-1") {
    corrected <- regmatches(string, check)
    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\(", "\\\\(", z, perl = TRUE))
    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\)", "\\\\)", z, perl = TRUE))
    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\;", "\\\\;", z, perl = TRUE))
#   corrected <- lapply(corrected, function(z) gsub("(?<!\\\\)\\](?!$)", "\\\\]", z, perl = TRUE))
#    corrected <- lapply(corrected, function(z) gsub("(?<!\\\\|^)\\[", "\\\\[", z, perl = TRUE))
    regmatches(string, check) <- corrected
  }
  return(string)
}

validate_sgf <- function(path = NA, string = NA){
  if(length(path) == 1) {
    if(is.na(string)) res <- try(read_sgf(path), silent = TRUE)
    if(!is.na(string)) res <- try(parse_sgf(string), silent = TRUE)
    if(class(res) == "list") output <- TRUE
    if(class(res) == "try-error") output <- FALSE
  } else {
    output <- unlist(lapply_pb(path, validate_sgf))
  }
  return(output)
}

create_database <- function(sgf_paths) {
  my_files <- sgf_paths
  jsons <- list()
  for(i in 1:length(my_files)){
    game_data <- read_sgf(my_files[i])
    game_data$m1 <- game_data$moves$coord_sgf[1]
    game_data$m2 <- game_data$moves$coord_sgf[2]
    jsons[[i]] <- toJSON(game_data[-which(names(game_data) %in% c("AB", "AW", "moves"))])
  }
  # read jsons into a single list of lists
  output <- lapply(jsons, function(z) fromJSON(z, simplifyVector=TRUE))
  # toJSON(output, pretty=TRUE)
  # combine lists of lists into one big dataframe of lists, by using jsonlite cleverly
  output <- fromJSON(as.character(toJSON(output)), simplifyVector=TRUE)
  output <- vectorize(output)
  return(output)
}

validate_games <- function(path) {
  if(length(path) == 1){
    res <- try(game_data <- read_sgf(path), silent = TRUE)
    failed <- class(res) == "try-error"
    if(failed) stop(paste(path, "is not a valid sgf"))
    coords <- as.character(game_data$moves$coord_sgf)
    coords_invalid <- !all(unlist(strsplit(coords, "")) %in% letters[1:20])
    coords_wronglength <- !all(nchar(coords) %in% c(0, 2))
    duplicate_key <- any(duplicated(names(game_data)))
    long_keys <- unlist(lapply(game_data, length) > 1)
    long_keys <- names(long_keys[long_keys])
    long_key_error <- !all(long_keys %in% c("moves", "AB", "AW"))
    output <- !(long_key_error | duplicate_key | coords_wronglength |
      coords_invalid)
  } else {
    output <- rep(NA, length(path))
    for(i in 1:length(path)) output[i] <- validate_games(path[i])
  }
  return(output)
}

write_sgf <- function(game_list, path){
  tags <- names(game_list)
  tags <- tags[-which(tags %in% c("kaya_notes", "hash_id", "n_moves",
    "moves", "filename"))]
  if ("AB" %in% tags) game_list$AB <- paste0("[", game_list$AB, "]",
    collapse = "")
  if ("AW" %in% tags) game_list$AW <- paste0("[", game_list$AW, "]",
    collapse = "")
  meta <- paste0( tags, "[", game_list[tags], "]", collapse = "")
  meta <- gsub("\\[\\[", "[", meta)
  meta <- gsub("\\]\\]", "]", meta)
  coord_sgf <- paste0(letters[game_list$moves$column],
    letters[game_list$moves$row])
  print("this doesn't do handicap stones yet")
  colors <- ifelse(game_list$moves$color == "white", "B", "W")
  movestring <- paste0(colors, "[", coord_sgf, "];", collapse = "")
  output <- paste0( "(;", meta, ";", movestring, ")")
  writeLines(output, path)
}

sapply_pb <- function(X, FUN, ...){
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
  wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal + 1 ,envir = env)
    setTxtProgressBar(get("pb", envir = env), curVal + 1)
    FUN(...)
  }
  res <- sapply(X, wrapper, ...)
  close(pb)
  res
}

lapply_pb <- function(X, FUN, ...){
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
  wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal + 1 ,envir = env)
    setTxtProgressBar(get("pb", envir = env), curVal + 1)
    FUN(...)
  }
  res <- lapply(X, wrapper, ...)
  close(pb)
  res
}
