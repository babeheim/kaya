
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
