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
n.nights.t <- t(n.nights)

# n.nights
n.nights.m <- n.nights.t
# str(n.nights.m)

#dataset with header issues
write.table(n.nights.m, "C://Code/data/n-nights-m.txt")

#matrix to dataframe
n.nights.df <- melt(na.rm = TRUE, n.nights.t, as.is = TRUE)
# n.nights.df

#save for manuscript
write_csv(n.nights.df, "C://Code/data/n-nights-df.csv")

