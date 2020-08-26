
write_tiny_gif <- function(game_object, file, delay = 2, 
  n_loops = 0, start = NA, stop = NA){
  if (is.na(stop)) stop <- game_object$n_moves
  game_moves <- game_object$moves
  game_moves$group_id <- id_maker(nrow(game_moves), nchar=3)
  n_moves <- max(game_moves$number)
  game_moves$n_liberties <- NA
  for (i in 1:stop){
    current_row <- which(game_moves$number == i)
    is_pass <- is.na(game_moves$column[current_row])
    # passes are defined by NA in column and row
    if (!is_pass){
      current_color <- game_moves$color[current_row]
      other_color <- ifelse(current_color == "black", "white", "black")

      # 0. check that the current move's location isn't occupied
      extant_moves <- which(game_moves$number < i & game_moves$group_id != "removed" & !is.na(game_moves$row))
      if(length(extant_moves) > 0){
        collision <- any(game_moves$column[extant_moves] == game_moves$column[current_row] & 
          game_moves$row[extant_moves] == game_moves$row[current_row])
        if(collision) stop(paste0("illegal collision detected, move ", i, " is to an occupied location"))
      }

      # 1. update group identity of stones of same color for all moves up to this move
      update_rows <- which(game_moves$color == current_color & game_moves$number <= i & game_moves$group_id != "removed")
      if(length(update_rows) > 0) game_moves$group_id[update_rows] <- paste(current_color, id_groups(game_moves[update_rows, c("column", "row", "group_id")]))

      bad <- any(game_moves$group_id[which(game_moves$group_id != "removed" & game_moves$color == "black")] %in%  game_moves$group_id[which(game_moves$group_id != "removed" & game_moves$color=="white")])
      if(bad) stop("groups are mixing up colors!")

      # 2. recount liberties for all stones
      active_rows <- which(game_moves$number <= i & game_moves$group_id != "removed")
      game_moves$n_liberties[active_rows] <- count_liberties(game_moves[active_rows,])

      # 3. remove enemy groups with 0 liberties! 
      update_rows <- which(game_moves$color == other_color & game_moves$number < i & game_moves$group_id != "removed")
      group_liberties <- tapply(game_moves$n_liberties[update_rows], game_moves$group_id[update_rows], sum)
      removable_groups <- names(which(group_liberties == 0))
      if(length(removable_groups) > 0){
        game_moves$group_id[update_rows][which(game_moves$group_id[update_rows] %in% removable_groups)] <- "removed"
      }

      pane_filename <- paste0("animated_pane_", sprintf("%04d", i), ".png")
      png(pane_filename, height = 2, width = 2, units = "in", res = 300)
      par(mar=c(0.1, 0.1, 0.1, 0.1))
      active_rows <- which(game_moves$number <= i & game_moves$group_id != "removed")
      plot(1, 1, col=NULL, ylim=c(0, 20), xlim=c(0, 20), 
        axes=FALSE, xaxt="n", yaxt="n", xlab="", ylab="")
      polygon(c(0, 0, 20, 20), c(0, 20, 20, 0))
      line.color <- gray(0.9)
      size <- 19
      for (j in 1:size){
        lines(c(j, j), c(1, size), col = line.color)
        lines(c(1, size), c(j, j), col = line.color)
      }
      points(game_moves$column[active_rows], game_moves$row[active_rows],
        bg=game_moves$color[active_rows], pch=21)
      current_row <- which(game_moves$number == i)
      points(game_moves$column[current_row],
       game_moves$row[current_row], col="red", pch=20, cex=0.5)
      dev.off()
    }
  }
  my_filename <- file
  convert_call <- paste0("convert -loop ", n_loops,
   " -delay ", delay, " animated_pane* ", my_filename)
  print("compiling gif")
  system(convert_call)
  pane_temp <- list.files(".", pattern = "animated_pane*")
  file.remove(pane_temp)
}

# there has to be a way to incorporate the whole update_status function so
# we dont have to rerun the same calculations over and over again?