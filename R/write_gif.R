
write_gif <- function(game_object, file,
  number = FALSE, delay = 50, n_loops = 0, start = NA, stop = NA){
  if (is.na(start)) start <- 1
  if (is.na(stop)) stop <- game_object$n_moves
  for (i in start:stop){
    pane_filename <- paste0("animated_pane_", sprintf("%04d", i), ".png")
    png(pane_filename, height = 5.25, width = 5, units = "in", res = 300)
    plot_game(game_object, stop = i)
    dev.off()
  }
  my_filename <- file
  convert_call <- paste0("convert -loop ", n_loops,
   " -delay ", delay, " animated_pane* ", my_filename)
  print("compiling gif")
  system(convert_call)
  pane_temp <- list.files(".", pattern = "animated_pane*")
  file.remove(pane_temp)
}
