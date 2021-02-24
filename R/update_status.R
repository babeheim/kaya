
update_status <- function(game_moves, viz = FALSE) {
  # expects a `game_moves` data frame wth five columns:
  # number, row, column, color and group_id
  check1 <- class(game_moves) == "data.frame"
  check2 <- all(c("column", "row", "number", "color", "group_id") %in% names(game_moves))
  check3 <- nrow(game_moves) > 0
  check <- check1 & check2 & check3
  if(!check) stop("not a valid game_moves object")
  n_moves <- max(game_moves$number)
  game_moves$n_liberties <- NA
  for (i in 1:n_moves) {
    current_row <- which(game_moves$number == i)
    is_pass <- is.na(game_moves$column[current_row])
    # passes are defined by NA in column and row
    if (!is_pass) {
      current_color <- game_moves$color[current_row]
      other_color <- ifelse(current_color == "black", "white", "black")

      # 0. check that the current move's location isn't occupied
      extant_moves <- which(game_moves$number < i &
        game_moves$group_id != "removed" & !is.na(game_moves$row))
      if (length(extant_moves) > 0) {
        collision <- any(game_moves$column[extant_moves] == game_moves$column[current_row] & 
          game_moves$row[extant_moves] == game_moves$row[current_row])
        if(collision) stop(paste0("illegal collision detected, move ", i, " is to an occupied location"))
      }

      # 1. update group identity of stones of same color for all moves up to this move
      update_rows <- which(game_moves$color == current_color & 
        game_moves$number <= i & game_moves$group_id != "removed")
      if (length(update_rows) > 0) {
        game_moves$group_id[update_rows] <- paste(current_color,
          id_groups(game_moves[update_rows, c("column", "row", "group_id")]))
      }

      black_groups <- game_moves$group_id[which(game_moves$group_id != "removed" & game_moves$color == "black")]
      white_groups <- game_moves$group_id[which(game_moves$group_id != "removed" & game_moves$color=="white")]
      bad <- any(black_groups %in% white_groups)
      if (bad) stop("groups are mixing up colors!")

      # 2. recount liberties for all stones
      active_rows <- which(game_moves$number <= i & game_moves$group_id != "removed")
      game_moves$n_liberties[active_rows] <- count_liberties(game_moves[active_rows,])

      # 3. remove enemy groups with 0 liberties! 
      update_rows <- which(game_moves$color == other_color & game_moves$number < i & game_moves$group_id != "removed")
      group_liberties <- tapply(game_moves$n_liberties[update_rows], game_moves$group_id[update_rows], sum)
      removable_groups <- names(which(group_liberties == 0))
      if (length(removable_groups) > 0) {
        game_moves$group_id[update_rows][which(game_moves$group_id[update_rows] %in% removable_groups)] <- "removed"
      }

      # 4. recount liberties again
      active_rows <- which(game_moves$number <= i & game_moves$group_id != "removed" & !is.na(game_moves$row))
      game_moves$n_liberties[active_rows] <- count_liberties(game_moves[active_rows,])

      if (viz) {
        plot(game_moves$column[active_rows], game_moves$row[active_rows],
          bg = game_moves$color[active_rows], pch = 21, ylim = c(0, 20), xlim = c(0, 20),
          axes = FALSE, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        polygon(c(0, 0, 20, 20), c(0, 20, 20, 0))
        points(game_moves$column[current_row], game_moves$row[current_row], col = "red", pch = 20, cex = 0.5)
      }

      # 5. suicide check - ah its just a problem with my algorithm
      update_rows <- which(game_moves$color == current_color & game_moves$number <= i &
        game_moves$group_id != "removed" & !is.na(game_moves$row))
      group_liberties <- tapply(game_moves$n_liberties[update_rows], game_moves$group_id[update_rows], sum)
      removable_groups <- names(which(group_liberties == 0))
      if (length(removable_groups) > 0) {
        warning(paste0("suicide detected at move ", i))
      }
    }
  }
  return(game_moves$group_id)
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

make_ids <- function(n, reserved = "", seed = NA, nchars = NA){
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
