## ---------------------- make.dist.vector()
##
##
##
##

make.dist.vector <- function(max.point, trap.interval){ # Define function
  
  ##----------------------------------------------------------------------------##
  ##                                  ARGS                                      ##
  ##----------------------------------------------------------------------------##
  ##  parasite - Column name                                                    ##
  ##  max.dist - The maximum distance to be considered from a focal point       ##
  ##----------------------------------------------------------------------------##
  
 
  
  
  vals3 <- seq(0, max.point, by=trap.interval)	
  distmatrix3 <- as.data.frame(permutations(n = length(vals3), r=2, v=vals3, repeats.allowed = T)) # n=vec size; r=number to choose each time
  colnames(distmatrix3) <- c("lat", "long") # Give them sensible names
  
  tempmat <- distmatrix3
  tempmat$distfromfoc <- sqrt((0 - tempmat$lat)^2 + (0 - tempmat$long)^2) 
  
  dist <- unique(tempmat$distfromfoc)
  dist <- sort(dist)
  dist <- dist[which (dist <= max.point)]
  return(dist)
}
