
update_status <- function(game_moves, viz = FALSE){

  # expects a game_moves wth five columns:
  # number, row, column and color
  check1 <- class(game_moves) == "data.frame"
  check2 <- all(c("column", "row", "number", "color",
   "group_id") %in% names(game_moves))
  check3 <- nrow(game_moves) > 0
  check <- check1 & check2 & check3
  if(!check) stop("not a valid game_moves object")

  n_moves <- max(game_moves$number)
  game_moves$n_liberties <- NA

  for(i in 1:n_moves){
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

      # 4. recount liberties again
      active_rows <- which(game_moves$number <= i & game_moves$group_id != "removed" & !is.na(game_moves$row))
      game_moves$n_liberties[active_rows] <- count_liberties(game_moves[active_rows,])

      if(viz){
        plot(game_moves$column[active_rows], game_moves$row[active_rows], bg=game_moves$color[active_rows], pch=21, ylim=c(0, 20), xlim=c(0, 20), axes=FALSE, xaxt="n", yaxt="n", xlab="", ylab="")
        polygon(c(0, 0, 20, 20), c(0, 20, 20, 0))
        points(game_moves$column[current_row], game_moves$row[current_row], col="red", pch=20, cex=0.5)
      }

      # 5. suicide check - ah its just a problem with my algorithm
      update_rows <- which(game_moves$color == current_color & game_moves$number <= i & game_moves$group_id != "removed" & !is.na(game_moves$row))
      group_liberties <- tapply(game_moves$n_liberties[update_rows], game_moves$group_id[update_rows], sum)
      removable_groups <- names(which(group_liberties == 0))
      if(length(removable_groups) > 0){
        warning(paste0("suicide detected at move ", i))
      }
    }
  }
  return(game_moves$group_id)
}
