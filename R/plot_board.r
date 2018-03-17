
plot_board <- function(size=19, clear.goban=TRUE, goban.color="darkgoldenrod1", line.color="black"){
    
  lb <- 1:size
  
  starpoints <- c(4,10,16)  # 4, 7, 10 for 13x13, and 3, 5, 7 for 9x9
    
  if(size == 13){
    starpoints <- c(4, 7, 10)
  } 
  if(size == 9){
    starpoints <- c(3,5,7)
  }
  
  title.height <- .25  # in inches
  goban.side <- 5   # in inches
  
  # if(length(dev.list())!=0 & all(round(par("pin"))==5) & clear.goban==F) return()
  
  # if(length(dev.list())==0 | any(round(par("pin"))!=5)){  # if a goban is not current open, create a window for one....
  
  #   graphics.off()
    
  #   # if(.Platform$OS.type=="windows") {
  #   #   windows(width = goban.side, height = goban.side+title.height, rescale="fixed")
  #   # }
  #   # # if(.Platform$OS.type=="unix") {
  #   # #   quartz(width = goban.side, height = goban.side+title.height)
  #   # # }
  #   # if(Sys.info()["sysname"]=="Linux"){
  #   #   X11(width = goban.side, height = goban.side+title.height)
  #   # }    
  #   # if(Sys.info()["sysname"]=="Darwin"){
  #   #   quartz(width = goban.side, height = goban.side+title.height)
  #   # }
        
  #   # this isnt working right in rmarkdown    
  # }
    
  par(mai=c(0, 0, title.height, 0)) 
  par(bg=goban.color)      
  plot(lb, lb, type = "n",xaxt="n",yaxt="n",xlab="",ylab="")
  
  for(i in lb){
    lines(c(i,i), c(1,size), col=line.color) 
    lines(c(1,size), c(i,i), col=line.color)
  }
  
  for(i in starpoints){
    for(j in starpoints){
      points(i,j,pch=20,cex=1, col=line.color)
    }
  }
  
}
