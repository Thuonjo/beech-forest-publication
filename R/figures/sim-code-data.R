#simulated data......
#overall variables
season <- rep(as.character(as.factor(c("Summer", "Autumn", "Winter", "Spring"))),3)

#Abundance
no.stoats <- c(25,15,25,15,10,160,200,200,25,15,25,20)
stoats <- c(12,10,12,5,7,75,100,100,10,3,5,4)

#seed
beech.seed <- c(0,0,0,0,0,rnorm(1,1550,1),rnorm(1,1800,1),rnorm(1,2000,1),0,0,0,0)
lcl.seed <- c(0,0,0,0,rnorm(1,10,2),rnorm(1,1000,50),rnorm(1,3000,100),0,0,0,0,0)
ucl.seed <- c(0,0,0,0,rnorm(1,190,10),rnorm(1,3000,50),rnorm(1,5000,100),0,0,0,0,0)
stata <- seq(1,12,1)
control <- factor(rep(1,12))
#control <- as.factor(c(rep(c("no.stoats"),4),rep(c("stoats"),4)))
labels1 <-  c("Summer", "Autumn", "Winter", "Spring", "Summer", "Autumn", "Winter", "Spring")

#date
date <- as.Date(as.character(c("1999-02-01","1999-05-01","1999-08-01","1999-11-01",
                               "2000-02-01","2000-05-01","2000-08-01","2000-11-01",
                               "2001-02-01","2001-05-01","2001-08-01","2001-11-01")))

#merge data
dat <- data.frame(control,season,stata,date,stoats,no.stoats,beech.seed,lcl.seed,ucl.seed)
# glimpse(dat)

# change levels
levels(dat$season) = c("Summer", "Autumn", "Winter", "Spring")

# reduce data to 10 not 12 seasons
dat <- filter(dat,stata < 11 & stata > 1)
# dat <- filter(dat,stata > 2)


############## hypothesis plot data
#short to long data switch

dat1 <- select(dat,stoats,no.stoats,season, date, stata, beech.seed) %>%
  gather(control,value,stoats:no.stoats)

# glimpse(dat1)

# reduce data to 10 not 12 seasons
dat1 <- filter(dat1,stata < 11 & stata > 1) %>%
  droplevels()

# dat <- filter(dat,stata > 2)

write_csv(dat1, "C://Code/data/simulated_data.csv")
sim.dat <- read_csv(file = "C://Code/data/simulated_data.csv")