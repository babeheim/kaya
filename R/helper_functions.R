


strip_comments <- function(move_nodes) {
  out <- lapply(move_nodes, function(z) {
    keep <- which(names(z) %in% c("W", "B"))
    z[keep]
  })
  return(out)
}

simplify_move_nodes <- function(branch) {
  move_nodes <- branch$nodes
  move_nodes <- strip_comments(move_nodes)
  coord_sgf <- unlist(move_nodes)
  color <- names(coord_sgf)
  color[color == "B"] <- "black"
  color[color == "W"] <- "white"
  moves <- data.frame(color, coord_sgf, stringsAsFactors = FALSE)
  if ("branches" %in% names(branch)) {
    first_branch <- branch$branches[[1]]
    branch_moves <- simplify_move_nodes(first_branch)
    moves <- rbind(moves, branch_moves)
    # attach branches in the same way!
  }
  return(moves)
}

extract_sgf_tag <- function(sgf_tag){
  if (length(sgf_tag) != 1) stop("sgf tag improper")
  sgf_tag <- strsplit(sgf_tag, "\\[|\\]\\[|\\]")[[1]]
  sgf_tag <- stringi::stri_trans_general(sgf_tag, "latin-ascii")
  sgf_tag <- gsub("[\x01-\x1F]", "", sgf_tag)
  # takes care of non-printing ASCII
  sgf_tag <- gsub(" *$|^ *", "", sgf_tag)
  output <- list()
  output[[1]] <- sgf_tag[2:length(sgf_tag)]
  names(output) <- sgf_tag[1] # might have trouble here
  return(output)
}


count_liberties <- function(moves, board_size = 19){
  # assumes all are active
  n_liberties <- rep(NA, nrow(moves))
  for (i in 1:nrow(moves)){
    i_x <- moves$column[i]
    i_y <- moves$row[i]
    south_free <- !any(moves$row == (i_y - 1) & moves$column == i_x) &
      i_y != 1
    north_free <- !any(moves$row == (i_y + 1) & moves$column == i_x) &
      i_y != board_size
    west_free <- !any(moves$row == (i_y) & moves$column == (i_x - 1)) &
      i_x != 1
    east_free <- !any(moves$row == (i_y) & moves$column == (i_x + 1)) &
      i_x != board_size
    n_liberties[i] <- sum(south_free, north_free, west_free, east_free)
  }
  return(n_liberties)
}

id_maker <- function(n, reserved = "", seed = NA, nchars = NA){
  my_let <- letters
  my_num <- 0:9
  if (is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(
    as.POSIXlt(Sys.time())))
  if (!is.na(seed) & is.numeric(seed)) set.seed(seed)
  output <- replicate(n, paste(sample(c(my_let, my_num),
    nchars, replace = TRUE), collapse = ""))
  rejected <- duplicated(output) | output %in% reserved |
    substr(output, 1, 1) %in% my_num
  while (any(rejected)){
    output <- output[-which(rejected)]
    remaining <- n - length(output)
    output <- c(output, replicate(remaining, paste(sample(c(my_let, my_num),
      nchars, replace = TRUE), collapse = "")))
    rejected <- duplicated(output) | output %in% reserved |
      substr(output, 1, 1) %in% my_num
  }
  output
}

id_groups <- function(moves){
  direct_mat <- id_direct_connections(moves)
  color_network <- graph_from_adjacency_matrix(direct_mat)
  group_id <- components(color_network)$membership
  return(group_id)
}

id_groups_old <- function(moves){
  direct_mat <- id_direct_connections(moves)
  group_IDs <- moves$group_id
  for (i in 1:length(group_IDs)){
    tie_cols <- which(direct_mat[i, ])
    leftmost <- group_IDs[tie_cols][1]
    ingroup <- tie_cols
    for (j in 1:length(tie_cols)){
      tie_rows <- which(direct_mat[, tie_cols[j]])
      ingroup <- c(ingroup, tie_rows)
      for (k in 1:length(tie_rows)){
        tie_cols_2 <- which(direct_mat[tie_rows[k], ])
        ingroup <- c(ingroup, tie_cols_2)
        for (l in 1:length(tie_cols_2)){
          tie_rows_2 <- which(direct_mat[, tie_cols_2[l]])
          ingroup <- c(ingroup, tie_rows_2)
          for (m in 1:length(tie_rows_2)){
            tie_cols_3 <- which(direct_mat[tie_rows_2[m], ])
            ingroup <- c(ingroup, tie_cols_3)
            for (n in 1:length(tie_cols_3)){
              tie_rows_3 <- which(direct_mat[, tie_cols_3[n]])
              ingroup <- c(ingroup, tie_rows_3)
              for (o in 1:length(tie_rows_3)){
                tie_cols_4 <- which(direct_mat[tie_rows_3[o], ])
                ingroup <- c(ingroup, tie_cols_4)
              }
            }
          }
        }
      }
    }
    ingroup <- sort(unique(ingroup))
    group_IDs[ingroup] <- leftmost
  }
  return(group_IDs)
}

# does this handle NA's correctly?
id_direct_connections <- function(moves){
  direct_mat <- matrix(FALSE, nrow = nrow(moves), ncol = nrow(moves))
  diag(direct_mat) <- TRUE
  for (i in 1:nrow(moves)){
    i_y <- moves$row[i]
    i_x <- moves$column[i]
    south <- which(moves$row == (i_y - 1) & moves$column == i_x)
    north <- which(moves$row == (i_y + 1) & moves$column == i_x)
    west <- which(moves$row == (i_y) & moves$column == (i_x - 1))
    east <- which(moves$row == (i_y) & moves$column == (i_x + 1))
    if (length(south) > 0) direct_mat[i, south] <- TRUE
    if (length(north) > 0) direct_mat[i, north] <- TRUE
    if (length(west) > 0) direct_mat[i, west] <- TRUE
    if (length(east) > 0) direct_mat[i, east] <- TRUE
  }
  return(direct_mat)
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
