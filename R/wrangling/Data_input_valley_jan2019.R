#valley structure
valley <- read.csv("C://Code/data/rat_dens_cheat_1.csv", header = TRUE)
valley[,1:4] <- 1
valley[13:20,2] <- NA
valley[,5:8] <- 2
valley[12:20,5:8] <- 3
valley[13:20,5] <- NA
valley[1,] <- NA
#try just keeping 2
valley <- valley[2:20,]

# construct a grid x trip matrix for valley
valley <- matrix(nrow = 20, ncol = 8)

# Eglinton +stoat control = 1
valley[1:20, 1:4] <- 1
# Hollyford -stoat control = 2
valley[1:12, 5:8] <- 2
# Hollyford +stoat control = 3
valley[13:20, 5:8] <- 3
# missing trips
valley[13:20, c(2, 5)] <- NA

# valley