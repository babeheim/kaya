
plot_board <- function(size = 19, goban.color = "darkgoldenrod1", line.color = "black"){
  starpoints <- c(4, 10, 16) 
  if(size == 13){
    starpoints <- c(4, 7, 10)
  }
  if(size == 9){
    starpoints <- c(3,5,7)
  }
  title.height <- .25  # in inches
  par(mai = c(0, 0, title.height, 0))
  par(bg = goban.color)
  plot(1:size, 1:size, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  for(i in 1:size){
    lines(c(i,i), c(1,size), col = line.color)
    lines(c(1,size), c(i,i), col = line.color)
  }
  for(i in starpoints){
    for(j in starpoints){
      points(i, j, pch=20, cex=1, col = line.color)
    }
  }
}
