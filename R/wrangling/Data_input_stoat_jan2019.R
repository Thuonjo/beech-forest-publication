######################################
####additonal information needed to estimate stoat control parameter
#or valley parameter
##create a dataset that .....

##create a dataset that .....
#stoats where controlled in all of the eglington valleys AND from

#nab the structure from rats
stoat <- read.csv("C://Code/data/Rat_lag_data.csv", header = TRUE)
stoat[,1:4] <- 1
stoat[13:20,2] <- NA
stoat[,5:8] <- 2
stoat[12:20,5:8] <- 1
stoat[13:20,5] <- NA
#stoat[1,] <- NA

# stoat
