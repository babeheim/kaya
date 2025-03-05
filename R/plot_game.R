

write_gif <- function(game_object, file,
  number = FALSE, fps = 10, n_loops = 0, start = NA, stop = NA) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The `magick` package is required but not installed. Install it with `install.packages(\"magick\")`.")
  }

  if (is.na(start)) start <- 1
  if (is.na(stop)) stop <- game_object$n_moves

  frame_files <- character()

  for (i in start:stop){
    pane_filename <- paste0("animated_pane_", sprintf("%04d", i), ".png")
    png(pane_filename, height = 5.25, width = 5, units = "in", res = 300)
    plot_game(game_object, stop = i)
    dev.off()
    frame_files <- c(frame_files, pane_filename)
  }

  img_sequence <- magick::image_read(frame_files)
  img_gif <- magick::image_animate(img_sequence, fps = fps)
  magick::image_write(img_gif, path = file, format = "gif")

  file.remove(frame_files)
}



write_tiny_gif <- function(game_object, file, fps = 10, 
  n_loops = 0, start = NA, stop = NA, track_captures = TRUE) {

  if (is.na(stop)) stop <- game_object$n_moves
  board_size <- as.numeric(game_object$SZ)[1]
  game_moves <- game_object$moves

  # necessary to get the plot right
  game_moves$row <- (-1) * game_moves$row
  game_moves$group_id <- make_ids(nrow(game_moves), nchar=3)
  game_moves$n_liberties <- NA

  frame_files <- character()

  for (i in 1:stop) {

    current_row <- which(game_moves$number == i)
    is_pass <- is.na(game_moves$column[current_row])

    # passes are defined by NA in column and row
    if (!is_pass) {

      current_color <- game_moves$color[current_row]
      other_color <- ifelse(current_color == "black", "white", "black")

      if (track_captures) {

        # 0. check that the current move's location isn't occupied
        extant_moves <- which(game_moves$number < i &
          game_moves$group_id != "removed" & !is.na(game_moves$row))
        if (length(extant_moves) > 0) {
          collision <- any(game_moves$column[extant_moves] == game_moves$column[current_row] & 
            game_moves$row[extant_moves] == game_moves$row[current_row])
          if (collision) {
            stop(paste0("illegal collision detected, move ", i,
              " is to an occupied location"))
          }
        }

        # 1. update group identity of stones of same color for all moves up to this move
        update_rows <- which(game_moves$color == current_color &
          game_moves$number <= i & game_moves$group_id != "removed")
        if (length(update_rows) > 0) {
          game_moves$group_id[update_rows] <- paste(current_color,
            id_groups(game_moves[update_rows, c("column", "row", "group_id")]))
        }

        black_moves <- which(game_moves$group_id != "removed" &
          game_moves$color == "black")
        white_moves <- which(game_moves$group_id != "removed" &
          game_moves$color=="white")
        bad <- any(game_moves$group_id[black_moves] %in%
          game_moves$group_id[white_moves])
        if (bad) stop("groups are mixing up colors!")

        # 2. recount liberties for all stones
        active_rows <- which(game_moves$number <= i &
          game_moves$group_id != "removed")
        game_moves$n_liberties[active_rows] <- count_liberties(game_moves[active_rows,])

        # 3. remove enemy groups with 0 liberties! 
        update_rows <- which(game_moves$color == other_color &
          game_moves$number < i & game_moves$group_id != "removed")
        group_liberties <- tapply(game_moves$n_liberties[update_rows],
          game_moves$group_id[update_rows], sum)
        removable_groups <- names(which(group_liberties == 0))
        if (length(removable_groups) > 0) {
          removable_moves <- which(game_moves$group_id[update_rows] %in% removable_groups)
          game_moves$group_id[update_rows][removable_moves] <- "removed"
        }

      }

      pane_filename <- paste0("animated_pane_", sprintf("%04d", i), ".png")
      png(pane_filename, height = 2, width = 2, units = "in", res = 300)
      par(mar=c(0.1, 0.1, 0.1, 0.1))
      active_rows <- which(game_moves$number <= i &
        game_moves$group_id != "removed")
      plot(1, 1, col=NULL, xlim=c(0, (board_size + 1)),
        ylim = -c((board_size + 1), 0), axes = FALSE,
        xaxt="n", yaxt="n", xlab="", ylab="")
      polygon(c(0, 0, (board_size + 1), (board_size + 1)),
        -c(0, (board_size + 1), (board_size + 1), 0))
      line.color <- gray(0.9)
      for (j in 1:board_size) {
        lines(c(j, j), -c(1, board_size), col = line.color)
        lines(c(1, board_size), -c(j, j), col = line.color)
      }
      points(game_moves$column[active_rows], game_moves$row[active_rows],
        bg=game_moves$color[active_rows], pch=21)
      current_row <- which(game_moves$number == i)
      points(game_moves$column[current_row],
       game_moves$row[current_row], col="red", pch=20, cex=0.5)
      dev.off()
      frame_files <- c(frame_files, pane_filename)
    }
  }

  img_sequence <- magick::image_read(frame_files)
  img_gif <- magick::image_animate(img_sequence, fps = fps)
  magick::image_write(img_gif, path = file, format = "gif")

  file.remove(frame_files)

}

plot_game <- function(game_object, number = FALSE, stop = NA, ...) {
  board_size <- as.numeric(game_object$SZ)[1]
  if (is.na(stop)) stop <- game_object$n_moves
  moves <- game_object$moves[game_object$moves$number <= stop, ]
  # evaluate
  moves$row <- moves$row * (-1)
  moves$group_id <- make_ids(nrow(moves), nchar = 3)
  moves$group_id <- update_status(moves)
  moves$rev_color <- ifelse(moves$color == "black", "white", "black" )
  tar <- which(moves$number <= stop & moves$group_id != "removed")
  plot_empty_board(board_size)
  goban.side <- par("pin")[1]
  stone.size <- goban.side / (19.44 * 0.076)
  points(moves$column[tar], moves$row[tar], cex = stone.size,
    pch = 21, bg = moves$color[tar])
  if (number == TRUE) text(moves$column[tar], moves$row[tar],
    labels = moves$number[tar], col = moves$rev_color[tar])
}

plot_empty_board <- function(board_size = 19, goban.color = "darkgoldenrod1",
 line.color = "black") {
  stopifnot(2 <= board_size & board_size <= 52)
  if (board_size == 19) {
    starpoints <- c(4, 10, 16)
  } else if (board_size == 13) {
    starpoints <- c(4, 7, 10)
  } else if (board_size == 9) {
    starpoints <- c(3, 5, 7)
  } else {
    starpoints <- NULL
  }
  title.height <- 0.25  # in inches?
  par(mai = c(0, 0, title.height, 0))
  par(bg = goban.color)
  plot(NULL, type = "n", xaxt = "n", yaxt = "n", xlab = "",
    ylab = "", xlim = c(1, board_size), ylim = -c(board_size, 1))
  for (i in 1:board_size) {
    lines(c(i, i), -c(1, board_size), col = line.color)
    lines(c(1, board_size), -c(i, i), col = line.color)
  }
  if (!is.null(starpoints)) {
    for (i in starpoints) {
      for (j in starpoints) {
        points(i, -j, pch = 20, cex = 1, col = line.color)
      }
    }
  }
}
