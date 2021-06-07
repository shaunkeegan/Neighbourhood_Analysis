## --------------------- EXECUTE: parasite.function
##
##
##
##





## ----------------------------------------------------------- LOAD DATA 

setwd ("~/Dropbox/Work/Research/Neighbourhood_Analysis/data")
data <- read.csv("mod_6_0914_b.csv", header = T)


## ------------------------------------------------------ LOAD FUNCTIONS

setwd ("~/Dropbox/Work/Research/Neighbourhood_Analysis/R/")
source("make.dist.vector.R")
source("parasite.function.R")
source("data.clean.R")
source("exppoints.R")
source("prop.on.grid.R")


## ----------------------------------------------------- INPUT VARIABLES

# ---------- Numeric Variables 

min.point <- 0    # Minimum value of lat/long - almost always 0
max.point <- 110  # Maaximum value of lat/long on a given grid
trap.interval <- 10

# ---------- Name Variables 

dist <- make.dist.vector(max.point, trap.interval)
dist <- sort(dist)
dist_trunc <- round(dist, digits = 1)

PropOnGrid <- rep("PropOnGrid", length(dist))
PropOnGrid <- paste(PropOnGrid, dist_trunc, sep = "_")
NoInf <- rep("NoInf", length(dist))
NoInf <- paste(NoInf, dist_trunc, sep = "_")
NoUninf <- rep("NoUninf", length(dist))
NoUninf <- paste(NoUninf, dist_trunc, sep = "_")
Prev <- rep("Prev", length(dist))
Prev <- paste(Prev, dist_trunc, sep = "_")

focal.variable <- "EhungInt"
neighbour.variable <- "HpolINF"

## ------------------------------------------------ SUBSET DATA

dat <- data.clean(data)
dat2 <- dat[which (dat$treated.Y.N == 0),]

keep1 <- c("ID", "ID.CapDate", "lat", "long", "Capture.date", "Sex", "Age", focal.variable, neighbour.variable)
keep2 <- c("ID", "ID.CapDate", "lat", "long", "Capture.date", neighbour.variable)
dat2 <- na.omit(dat2[,keep1])
dim(dat2)	
dat <- na.omit(dat[,keep2])
dim(dat)	




dat3 <- parasite.function(dat,dat2,neighbour.variable, dist)


