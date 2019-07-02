#Cleaning raw data

# rat Captures
dat.raw <- read.table("C://Code/data/Eglintonvalleydatatest1.txt", header=T, sep="\t", quote="")

# create a variable to distinguish each valley and each unique grid
dat <- mutate(dat.raw, valley = ifelse(area.id == "P-", "egl", "hol"),
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
# 
# glimpse(dat)

write_csv(dat, "C://Code/data/clean.dat.raw.csv")
