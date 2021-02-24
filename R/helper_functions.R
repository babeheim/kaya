
create_database <- function(sgf_paths) {
  my_files <- sgf_paths
  jsons <- list()
  for (i in 1:length(my_files)) {
    game_data <- try(read_sgf(my_files[i]))
    if (class(game_data) != "try-error") {
      game_data$m1 <- game_data$moves$coord_sgf[1]
      game_data$m2 <- game_data$moves$coord_sgf[2]
      jsons[[i]] <- toJSON(game_data[-which(names(game_data) %in% c("AB", "AW", "moves"))])
    }
    if (i %% 100 == 0) print(i)
  }
  # read jsons into a single list of lists
  output <- lapply(jsons, function(z) fromJSON(z, simplifyVector = TRUE))
  # combine lists of lists into one big dataframe of lists, by using jsonlite cleverly
  output <- fromJSON(as.character(toJSON(output)), simplifyVector = TRUE)
  output <- as.data.frame(bind_rows(output))
  return(output)
}

validate_sgfs <- function(files) {
  output <- rep("", length(files))
  if (length(files) == 1) {
    if (!file.exists(files)) stop("no file at this location")
    res <- try(game_data <- read_sgf(files), silent = TRUE)
    failed <- class(res) == "try-error"
    if (failed) {
      output <- paste(files, "is not a valid sgf file")
    } else {
      coords <- as.character(game_data$moves$coord_sgf)
      coords_invalid <- !all(unlist(strsplit(coords, "")) %in% letters[1:20])
      coords_wronglength <- !all(nchar(coords) %in% c(0, 2))
      if (coords_invalid | coords_wronglength) {
        output <- "coordinates are invalid"
      }
      duplicate_key <- any(duplicated(names(game_data)))
      long_keys <- unlist(lapply(game_data, length) > 1)
      long_keys <- names(long_keys[long_keys])
      long_key_error <- !all(long_keys %in% c("moves", "AB", "AW"))
      if (duplicate_key | long_key_error) {
        if (output != "") output <- paste(output, "sgf codes are invalid", sep = "; ")
        if (output == "") output <- "sgf codes are invalid"
      }
      if (!(long_key_error | duplicate_key | coords_wronglength |
        coords_invalid)) {
        output <- "sgf is valid"
      }
    }
  } else {
    for (i in 1:length(files)) {
      output[i] <- validate_sgfs(files[i])
      if (i %% 100 == 0) print(i)
    }
  }
  return(output)
}


write_sgf <- function(game_list, file) {
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
  writeLines(output, file)
}

lapply_pb <- function(X, FUN, ...) {
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
  wrapper <- function(...) {
    curVal <- get("counter", envir = env)
    assign("counter", curVal + 1 ,envir = env)
    setTxtProgressBar(get("pb", envir = env), curVal + 1)
    FUN(...)
  }
  res <- lapply(X, wrapper, ...)
  close(pb)
  res
}
