# Data input
# 28/11/2017

###############################################################################
#installing new packages
# install.packages("ggplot2")
# install.packages("plyr")
# install.packages("gapminder")
# installed.packages("lubridate")
# install.packages("cowplot")
# install.packages("jagsUI")
# install.packages("tidyverse")
# install.packages("Matrix")
# install.packages("ggthemes")
# install.packages("gridExtra")
# install.packages("coda")

#use structure from Figure 5 high and low for now
# library(tidyverse)
# library(lubridate)
# library(jagsUI)
# library(cowplot)
# library(Matrix)
# library(ggthemes)
# library(ggplot2)
# library(gridExtra)
# library(coda)
# library(reshape2)
# 
# par(ask=F)


###############MICE#####################

# Import data
dat <- read.table("C://Code/data/Eglintonvalleydatatest1.txt", header=T, sep="\t", quote="")
# glimpse(dat)

# create a variable to distinguish each valley and each unique grid
dat <- mutate(dat, valley = ifelse(area.id == "P-", "egl", "hol"),
              grid = paste(valley, grid.id),
              date = dmy(date),
              month = as.character(month(date, label = T)),
              month = ifelse(month == "Dec", "Nov", month),
              month = ifelse(month == "Mar", "Feb", month),
              month = ifelse(month == "Sep", "Aug", month),
              year = year(date))




#these counts were checked against the trip records in rodents.mdb and the following changes
# glimpse(dat)
# table(dat$month)
# table(dat$grid, dat$trip.no)

n.trip <- length(levels(factor(dat$trip.no)))
n.grid <- length(levels(factor(dat$grid)))
# n.trip
# n.grid

# Import data
dat <- read.table("C://Code/data/Eglintonvalleydatatest1.txt", header=T, sep="\t", quote="")

# create a variable to distinguish each valley and each unique grid
dat <- mutate(dat, valley = ifelse(area.id == "P-", "egl", "hol"),
              grid = paste(valley, grid.id),
              date = dmy(date),
              month = as.character(month(date, label = T)),
              month = ifelse(month == "Dec", "Nov", month),
              month = ifelse(month == "Mar", "Feb", month),
              month = ifelse(month == "Sep", "Aug", month),
              year = year(date))

# glimpse(dat)
# table(dat$month)
# table(dat$grid, dat$trip.no)

n.trip <- length(levels(factor(dat$trip.no)))
n.grid <- length(levels(factor(dat$grid)))
# n.trip
# n.grid


################################################################################
# calculate the number of nights trapping on each grid in each trip
# first filter by unique values of grid, trip.no and date
sub.dat <- filter(dat, duplicated(paste(grid, trip.no, date)) == F)

# then count the number of unique dates by grid and trip.no
n.nights <- table(sub.dat$grid, sub.dat$trip.no)
# n.nights

# These counts were checked against the trip records in rodents.mdb and the following changes
# need to be made:

n.nights[2, 1] <- 5
n.nights[9, 1] <- 4
n.nights[2, 10] <- 4
n.nights[4, 10] <- 4
n.nights[6, 10] <- 4
n.nights[10, 13] <- 3
n.nights[4, 14] <- 3

# transpose this so that trips are rows = j, and grids are columns = k
n.nights <- t(n.nights)
# n.nights

################################################################################
# make a variable that indicates the rank order of each day on each trip
# we'll need this for dealing with dead individuals

# get the unique dates for each trip in each valley
get.date <- filter(dat, duplicated(paste(valley, trip.no, date)) == F) %>%
  select(valley, trip.no, date)

# for each valley and trip order the dates and number by rank order (= day.ord)
get.date <- group_by(get.date, valley, trip.no) %>%
  mutate(day.ord = rank(date))

# get.date

# select what we need and then merge with dat
get.date <- select(get.date, valley, trip.no, date, day.ord)

dat <- left_join(dat, get.date)
# glimpse(dat)
# table(dat$day.ord)

################################################################################
## MICE ##
# subset by species and grid
################################################################################

mice <- subset(dat, species == "Mouse" & !(grid.id %in% c("R1", "R2")))

################################################################################
# re-factor all factor columns
mice <- mice %>% mutate_each_(funs(factor(.)), names( .[,sapply(., is.factor)]))

# table(mice$grid)
# table(mice$fate)

# reset total number of grids
n.grid <- length(table(mice$grid))

# reset n.nights by removing the columns for R1 and R2
n.nights <- n.nights[, -c(5, 6, 11, 12)]

################################################################################
# sort out whether things died
# table(mice$fate, mice$died, exclude = NULL)

# fate == DEAD was applied to animals when they were killed on the kill grids
# table(mice$grid, mice$fate, mice$species)
# but there are some captures on kill grids because later on they stopped killing

# died == y was applied to animals that were supposed to be tagged but died
# these are usually also recorded as NEW, OLD or RECAP

# also look at notes
#  table(mice$trap.note)

# searching for one of the words in the list below should pick up all of the dead animals
# but need to check
dead.exp <- c("doa", "DOA", "eaten", "dead", "killed", "owl", "Dead", "DEAD", "Died", "Killed")

# table(mice$died)
# code to go through each word in the list and use grep to locate the rows where trap.note contains that word
# then set died == y for those rows
for(i in 1:length(dead.exp)) {
  mice$died[grep(dead.exp[i], mice$trap.note)] <- "y"
}
# table(mice$died)

################################################################################
# remove individuals where fate = DEAD
mice <- filter(mice, fate != "DEAD")

################################################################################
# sort out issues with individual tags
a <- table(mice$tag.right, mice$fate)
# head(a)
# tail(a)

# flag the not-normal tags
mice <- mutate(mice, flag.tag = ifelse(tag.right %in% c("", "0", "9999") & fate != "DEAD", 1, 0))

# table(mice$flag.tag, mice$died)
# many of the unusual tages are animals that were not tagged because they died, so they are NEW and dead (no previous tag)
# the remainder probably escaped before they could be tagged
# mice[mice$flag.tag == 1 & mice$died == "", ]
# that's true for a bunch of them, as indicated by the notes
# for the remainder they most likely escaped, which we will assume, although the 9999s may be a trip where they ran out of tags
# either way, we can't do much with these records
# if these mice escaped, we should treat them as if they were never captured from the point of view of the mark-recapture analysis
# so drop these

mice <- filter(mice, !(flag.tag == 1 & died == ""))

# for the individuals with no tag that died on capture, give each of these a new unique tag number starting with d
# so that we can count them as unique individuals
# first find how many
n.nd <- length(mice$flag.tag[mice$flag.tag == 1 & mice$died == "y"])

# then assign a new tag number to each one
mice$tag.right <- as.character(mice$tag.right)
mice$tag.right[mice$flag.tag == 1 & mice$died == "y"] <- paste("d", 1:n.nd)
# mice$tag.right[mice$flag.tag == 1 & mice$died == "y"]

# for each individual, calculate the number of times it was captured on each grid on each trip
ind <- group_by(mice, valley, grid.id, grid, trip.no, month, year, tag.right) %>%
  summarise(n = n()) %>%
  mutate(recap = ifelse(n > 1, 1, 0))

# table(ind$n)
# table(ind$recap)

################################################################################
# the minimum number of animals caught on each grid on each trip
# and number of recaptures
min.mice <- group_by(ind, valley, grid.id, grid, trip.no) %>%
  summarise(recap = sum(recap),
            n = n())

# ggplot(min.mice, aes(y = n, x = trip.no)) +
#   geom_point() +
#   geom_line() +
#   facet_grid(valley ~ grid.id) +
#   theme_bw()

################################################################################
# identify dead mice that should have been tagged
dm <- filter(mice, fate != "DEAD" & died == "y")

# check that mice did not die twice
# table(table(dm$tag.right))

# individual 2711
dm[dm$tag.right == "2711", ]
# must have died on day 4 not 3
dm <- dm[!(dm$tag.right == "2711" & dm$day.ord == 3), ]
table(table(dm$tag.right))

# select the day they died on
dm <- mutate(dm, day.died = day.ord) %>%
  select(tag.right, day.died)

# merge this with the full data set
mice <- left_join(mice, dm)

# for each individual on each grid, determine the day of death if it died
dd <- group_by(mice, valley, grid.id, grid, trip.no, tag.right) %>%
  summarise(day.died = mean(day.died, na.rm = T))

# number of mice that died that should have been tagged
# sum(table(dd$day.died))

# update ind to include day.died variable
ind$day.died <- dd$day.died

################################################################################
# use the data frame ind to create an array with the number of captures for each individual on each grid
# maximum number caught on a grid
max.n <- max(min.mice$n)

# set up Y to be 4 times this length to allow sufficient number of augmented individuals

# set up array Y with rows i = indiviudals, columns = j = trip, k = grids
Y <- array(dim = c(max.n * 4 + 1, n.trip, n.grid))

# array with the number of nights that each individual was trapped for on each trip and grid
J <- Y

# array with day that mice died if they did
die <- Y

# matrix to record the number of individuals captured on each trip on each grid
ind.cap <- matrix(0, nrow = n.trip, ncol = n.grid)

# make a numeric variable for each grid
ind$grid.n <- as.numeric(factor(ind$grid))

for(j in 1:n.trip) {
  for(k in 1:n.grid) {
    # mice caught on grid k during trip j, with n = number of times caught
    cap <- ind$n[ind$trip.no == j & ind$grid.n == k]
    # the day that mice died if they did on grid k and trip j
    day.died <- ind$day.died[ind$trip.no == j & ind$grid.n == k]
    n.cap <- length(cap)
    # write values to the arrays
    if(n.cap > 0) {
      Y[1:n.cap, j, k] <- cap
      ind.cap[j, k] <- n.cap
      die[1:n.cap, j, k] <- day.died
    }
    # write J from number of trap nights calculated earlier
    J[, j, k] <- n.nights[j, k]
  }
}

# check that Y does not exceed J
# table(Y > J)

# number of animals captured on each trip x grid
# ind.cap

# update J by changing number of trap nights to day that aminal died if it died
die[1:20, , 1]
J[1:20, , 1]

J[is.na(die) == F] <- die[is.na(die) == F]
# check it worked
J[1:20, , 1]

# check that Y does not exceed J
# table(Y > J)

# 4 cases where animal died but then appears to have been recaught
Y[which(Y > J)]
J[which(Y > J)]

# for the time being correct these to make Y = J
Y[which(Y > J)] <- J[which(Y > J)]
# table(Y > J)

################################################################################
# row i is individual animal
# column j is trip
# sheet k is grid

# variables we need for the model

# Y = number of captures for each mouse (i) caught on each trip (j) on each grid (k)
# z = latent variable for whether pseudo mice were present or not
# J = number of nights trapping for each mouse (i) on each trip (j) on each grid (k)

# table(mice$grid)

# set missing values in Y to 0 captures for the pseudo individuals
Y[is.na(Y) == T] <- 0

# latent indicator variable
z <- ifelse(Y > 0, 1, NA)

N_grid <- dim(Y)[3]
N_animals <- dim(Y)[1]
N_trip <- dim(Y)[2]

################################################################################
# set a prior on the number of individuals that could be present linked to the number
# caught by allowing the number of augmented individuals to vary
# = 4 times the number caught or a minimum of 5
# to do this we create a variable N_animals that is the number of augmented individuals
# on each trip and grid to sample up to

my <- ifelse(Y > 0, 1, 0)
N_animals <- matrix(nrow = N_trip, ncol = N_grid)
for(i in 1:N_grid) {
  N_animals[, i] <- colSums(my[, , i])
}

N_animals <- N_animals * 4
N_animals <- ifelse(N_animals < 5, 5, N_animals)

for(k in 1:N_grid) {
  for(j in 1:N_trip){
    z[((N_animals[j, k] + 1):dim(z)[1]), j, k] <- 0
  }
}

# create a new variable for N_trip that is a vector with the number of trips for each grid
# thus leaving out trips that did not occur
# n.nights
my <- ifelse(n.nights > 0, 1, 0)
N_trip <- colSums(my)
N_trip

# sum(N_animals)


