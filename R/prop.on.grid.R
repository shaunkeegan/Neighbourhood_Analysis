## ---------------------- prop.on.grid()
##
##
##
##




#function that takes a focal's lat and long, and a specified distance (s), and returns 
#	the proportion of trap locations on grid within distance s of the focal's location
prop.on.grid <- function(focal.lat, focal.long, s){
  
  #create a dataframe with all coordinates in a (min.point to max.point) x (min.point to max.point) grid
  vals3 <- seq(min.point, max.point, by=10)	
  distmatrix3 <- as.data.frame(permutations(n = length(vals3), r=2, v=vals3, repeats.allowed = T)) # n=vec size; r=number to choose each time
  colnames(distmatrix3) <- c("lat", "long") # Give them sensible names
  

  
  tempmat <- distmatrix3
  tempmat$distfromfoc <- sqrt((focal.lat - tempmat$lat)^2 + (focal.long - tempmat$long)^2)	#distance of each gridpoint from focal
  temp <- subset(tempmat, distfromfoc<=s)		#subset out just those locations that are within 's' of the focal
  val <- dnumpoint[dnumpoint$dist==s,"numpoints"]
  ifelse(length(val)==0, return(0), return(nrow(temp)/val))	#

}