## ---------------------- parasite.function()
##
##
##
##

parasite.function <- function (dat, dat2, neighbour.variable, dist){
  ## -------------------- inputs ---
  ## dat            -
  ## dat2           -
  ## variable.name  -
  ## distances      -
  ## -------------------------------
  
  for (NH.dist in 1:length(dist)){ # cycle through all possible neighbourhood sizes
    
    no <- length(unique(dat2$ID.CapDate)) # All ID.CapDate entries
    
    for (i in 1:no){ # loop through all ID.CapDate entries
      
      infsum <- 0
      uninfsum <- 0
      #Get lat,long,date for focal individual
      focal.lat <- dat2[i,"lat"]
      focal.long <- dat2[i,"long"]
      focal.date <- dat2[i,"Capture.date"]
      
      
      # Percentage of the NH in the grid
      
      
      dist_vec <- make.dist.vector(dist[NH.dist], trap.interval)
      dist_vec <- sort(dist_vec)
      
      #generate list of number of points for each value of 'dist' from central point
      numpoints <- apply(as.array(dist_vec),1,exppoints)	#gives a list 1, 5, 9, 13, 21, ... for all values in 'dist'
      #bind that list to dist
      dnumpoint <- as.data.frame(cbind (dist_vec, numpoints))
      dat2[i,PropOnGrid[NH.dist]] <- prop.on.grid(focal.lat, focal.long, NH.dist)
      
      
      
      
      dat.subset <- dat # Subset animals on same grid (for later use - disregard for now)
      
      
      for (j in 1:length(dat.subset$ID.CapDate)){ # Loop through all other animals
        neighbour.long <- dat.subset[j,"long"]
        neighbour.lat <- dat.subset[j,"lat"]
        temp.date <- dat.subset[j,"Capture.date"]
        
        neighbourdist <- sqrt((focal.lat - neighbour.lat)^2 + (focal.long - neighbour.long)^2)
        
        if(neighbourdist <= dist[NH.dist] &
           temp.date < focal.date ){
          ifelse(dat.subset[j,neighbour.variable]==1, infsum <- infsum + 1, 
                 ifelse(dat.subset[j,neighbour.variable]==0, uninfsum <- uninfsum + 1, NA))	
        }	#Close if
      }	#Close j loop
      
    dat2[i,NoInf[NH.dist]] <- infsum
    dat2[i,NoUninf[NH.dist]] <- uninfsum
    dat2[i,Prev[NH.dist]] <- infsum/(infsum + uninfsum)
      
      
      
    }
    
    
  }
  
  return(dat2)
  
}
