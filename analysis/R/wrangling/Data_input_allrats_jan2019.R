# Data input
# RATS ONLY
# "Functions should do one thing and one thing only....
# Jan 2019

###############################################################################
#installing new packages
# install.packages("ggplot2")
# install.packages("plyr")
# install.packages("gapminder")
# installed.packages("lubridate")
# install.packages("cowplot")
# install.packages("jagsUI")
# # install.packages("tidyverse")
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
# 
# par(ask=F)

########Data import #####################
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

#look at data
# glimpse(dat)
# table(dat$month)
# table(dat$grid, dat$trip.no)

#extract number of trips and grids in rat dataset
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
## RATS ##
# subset by species and grid

################################################################################
# combine kiore and ship rats
dat$species <- as.character(dat$species)
dat$species <- ifelse(dat$species %in% c("Kiore", "Ship Rat"), "Rat", dat$species)

# table(dat$species, dat$grid.id)

#looks correct from here
rats <- subset(dat, species == "Rat")

#the M1 and M2s are all dead!!!
#but they are in the dataset here
write.csv(rats, "R1R2rat_data.csv")

################################################################################
#remove R1 and R2 grids
rats <- subset(dat, species == "Rat" & !(grid.id %in% c("R1", "R2")))

# re-factor all factor columns
rats <- rats %>% mutate_each_(funs(factor(.)), names( .[,sapply(., is.factor)]))

# table(rats$grid)
# rats
# table(rats$fate)

# reset total number of grids
n.grid <- length(table(rats$grid))

# reset n.nights by removing the columns for M1 and M2
n.nights <- n.nights[, -c(5, 6, 11, 12)]

################################################################################
# sort out whether things died
# table(rats$fate, rats$died, exclude = NULL)

# fate == DEAD was applied to animals when they were killed on the kill grids
# table(rats$grid, rats$fate, rats$species)
# but there are some captures on kill grids because later on they stopped killing

# died == y was applied to animals that were supposed to be tagged but died
# these are usually also recorded as NEW, OLD or RECAP

# also look at notes
table(rats$trap.note)

# searching for one of the words in the list below should pick up all of the dead animals
# but need to check
#removed the animals that were not killed
#instead I will tag these so that then they can be counted

dead.exp <- c("doa", "DOA", "eaten", "Eaten", "Dead", "Killed")

table(rats$died)
# code to go through each word in the list and use grep to locate the rows where trap.note contains that word
# then set died == y for those rows
for(i in 1:length(dead.exp)) {
  rats$died[grep(dead.exp[i], rats$trap.note)] <- "y"
}

rats$died[grep("DEAD", rats$fate)] <- "y"

# table(rats$died,rats$grid)
# remove individuals where fate = DEAD
# we don't do this here as want to keep all the killed rats

################################################################################
# sort out issues with individual tags
a <- table(rats$tag.right, rats$fate)
# head(a)
# tail(a)

# flag the not-normal tags
rats <- mutate(rats, flag.tag = ifelse(tag.right %in% c("", "0"), 1, 0))

# table(rats$flag.tag, rats$died)
# many of the unusual tages are animals that were not tagged because they died, so they are NEW and dead (no previous tag)
# the remainder probably escaped before they could be tagged
rats[rats$flag.tag == 1 & rats$died == "", ]
# that's true for a bunch of them, as indicated by the notes
# for the remainder they most likely escaped, which we will assume, although the 9999s may be a trip where they ran out of tags
# either way, we can't do much with these records
# if these rats escaped, we should treat them as if they were never captured from the point of view of the mark-recapture analysis
# so drop these

rats <- filter(rats, !(flag.tag == 1 & died == ""))

# for the individuals with no tag that died on capture, give each of these a new unique tag number starting with d
# so that we can count them as unique individuals
# first find how many
n.nd <- length(rats$flag.tag[rats$flag.tag == 1 & rats$died == "y"])

# then assign a new tag number to each one
rats$tag.right <- as.character(rats$tag.right)
rats$tag.right[rats$flag.tag == 1 & rats$died == "y"] <- paste("d", 1:n.nd)
rats$tag.right[rats$flag.tag == 1 & rats$died == "y"]

# for each individual, calculate the number of times it was captured on each grid on each trip
ind <- group_by(rats, valley, grid.id, grid, trip.no, month, year, tag.right) %>%
  summarise(n = n()) %>%
  mutate(recap = ifelse(n > 1, 1, 0))

# table(ind$n,ind$grid)
# table(ind$recap)

################################################################################
# the minimum number of animals caught on each grid on each trip
# and number of recaptures

#what is happening here??

min.rats <- group_by(ind, valley, grid.id, grid, trip.no) %>%
  summarise(recap = sum(recap),
            n = n())

# ggplot(min.rats, aes(y = n, x = trip.no)) +
#   geom_line() +
#   facet_g  geom_point() +
# rid(valley ~ grid.id) +
#   theme_bw()


#rats.density

rat.dens <- tapply(min.rats$n, list(min.rats$trip.no, min.rats$grid),sum)
# rat.dens

#put 0s where they should be and NAs where there were no trips

# names(rat.dens)

rat.dens.df <- data.frame(tapply(min.rats$n, list(min.rats$trip.no, min.rats$grid),sum))
# rat.dens.df

rat.dens.df <- replace_na(rat.dens.df,list(egl.M1 = 0, egl.M2 = 0,egl.MR1 = 0, egl.MR2 = 0, hol.M1 = 0,
                                           hol.M2 = 0,  hol.MR1 = 0, hol.MR2 = 0))

# names(rat.dens.df)
# head(rat.dens.df)
#from 13:20 in egl m2 and hol m1 need to be NAs
#which equates to col 1 and 4
rat.dens.df[c(13:20),2] <- "NA"
rat.dens.df[c(13:20),5] <- "NA"

#back to table
rat.dens <- data.matrix(rat.dens.df)
#change names DOES NOT WORK FOR MATRIX
# names(rat.dens) <- c("egl M1",  "egl M2" , "egl MR1", "egl MR2", "hol M1",
# "hol M2" , "hol MR1", "hol MR2")

#working
colnames(rat.dens) <- c("egl M1",  "egl M2" , "egl MR1", "egl MR2", "hol M1",
                        "hol M2" , "hol MR1", "hol MR2")


#just have to get this done so doing it manually
#bullshit way
# write.csv(rat.dens, "rat_dens_cheat.csv")
write.csv(min.rats, "C://Code/data/mna_allrat.csv")
#
# min.rats_change <- read.csv("rat_dens_cheat_1.csv")
# min.rats_change <- data.matrix(min.rats_change)
# str(min.rats_change)

# str(n.nights)

#---------------------------------------------------------------------
#RATS just saved as CSV for now

################################################################################
# read in data on minimum number of animals for rats
rat.mna <- read.csv("C://Code/data/rat_dens_cheat_1.csv")
rat.mna <- as.matrix(rat.mna)

#check that the two matrices are the same
# rat.mna == rat.dens

# lag the values by one and reset the missing values
lag.rat.mna <- apply(rat.mna, 2, function(x) lag(x))
lag.rat.mna[13, c(2, 5)] <- NA
# lag.rat.mna

##laged
log.lag.rat.mna <- log10(apply(rat.mna, 2, function(x) lag(x))+1)
log.lag.rat.mna[13, c(2, 5)] <- NA

colnames(log.lag.rat.mna) <- c("egl M1",  "egl M2" , "egl MR1", "egl MR2", "hol M1",
                               "hol M2" , "hol MR1", "hol MR2")

log.lag.rat.mna <- log.lag.rat.mna[c(2:20),]

#not lag
log.rat.mna <- log10((rat.mna)+1)
log.rat.mna[13, c(2, 5)] <- NA

colnames(log.rat.mna) <- c("egl M1",  "egl M2" , "egl MR1", "egl MR2", "hol M1",
                           "hol M2" , "hol MR1", "hol MR2")

#saving data read for analysis
write.csv(log.rat.mna, "C://Code/data/Rat_data_matrix.csv")
write.csv(lag.rat.mna, "C://Code/data/Rat_lag_data.csv")
write.csv(min.rats, "C://Code/data/Rat_data.csv")


# comes from Appendix_one rats comparison ---------------------------------
# see this for full details
# rmarkdown::render("./Davidson_2019_BeechForest_Appendix.Rmd")

#kiore RATS only
rat.kiore <- read_csv("C://Code/data/mna_kiore.csv")
# glimpse(rat.kiore)

#ship RATS - used for analysis as rt
rat.rat <- read_csv("C://Code/data/mna_rat.csv")

# glimpse(rat.rat)
# mice <- read.csv()
# already sourced everthing i need ..
# sum(ind$n)




# ###########rats###########################################################
# #rats....
# #try this to get lines....
# # # abundance
# source("./R_publication/functions_BHM_CR_models/extract_variables.R")
# outl.N <- extract.var(model = dat.CR, name.model = "log", var.to.extract = "lam", start.point = 1, ind.cap = ind.cap, mice = mice)
# 
# outl.N$min_rats <- rat.mna[is.na(rat.mna) == F]
# 
# meanR <- group_by(outl.N,valley, trip) %>%
#   summarise(sum.r = sum(min_rats),
#             mean.r = mean(min_rats),
#             sd = sd(min_rats),
#             lcl.r = mean(min_rats) - (1.96 * (sd(min_rats)/ sqrt(length(min_rats)))),
#             ucl.r = mean(min_rats) + (1.96 * (sd(min_rats)/ sqrt(length(min_rats)))),
#             control = NA)
# 
# 
# for(i in 1:length(meanR$valley)) {
#   meanR$control[i]  <-  ifelse(meanR$valley[i] == "hol" & meanR$trip[i] > 12, "control", "no control")
#   meanR$control[i]  <-  ifelse(meanR$valley[i] == "egl", "control", meanR$control[i])
# 
# }
# 
# 
# #look at min.ind code....
# head(meanR)
# str(meanR)
# 
# # merge control and valley for plotting
# meanR <- mutate(meanR, Conditions = paste(control, valley))
# meanR$Conditions <- factor(meanR$Conditions, levels = c("control egl", "no control hol", "control hol"))
# meanR$trip <- as.factor(meanR$trip)
# 
# group_by(meanR,trip)
# meanR$true.date <- as.Date(as.character(c("1999-05-01","1999-08-01","1999-11-01",
#                                           "2000-02-01","2000-05-01","2000-08-01","2000-11-01",
#                                           "2001-02-01","2001-05-01","2001-08-01","2001-11-01",
#                                           "2002-05-01","2002-11-01",
#                                           "2003-02-01","2003-05-01","2003-08-01","2003-11-01",
#                                           "2004-02-01","2004-05-01","2004-08-01")))
# 
# str(meanR$true.date)
# #variation using standard CI technique
# #this doesn't really make sense as this is an estimate of MNA but across grids
# 
# 
# # for entering blank ticks
# # function to enter blanks
# insert_minor <- function(major_labs, n_minor) {
#   labs <- c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
#   labs[1:(length(labs)-n_minor)]
# }
# 
# # parameters to make it work
# # where the ticks are
# ticks <- c("1998","1999", "2000", "2001", "2002", "2003", "2004", "2005")
# 
# # major,minor
# tick.labels <- insert_minor(ticks, 4)
# # only data ticks
# # tick.labels <- c(tick.labels[c(3:18)], tick.labels[c(21:29)],tick.labels[c(31)])
# # season ticks
# # major,minor
# tick.labels <- insert_minor(ticks, 4)
# tick.labels <- c(tick.labels[c(5:33)])
# 
# write.csv(meanR, "C://Code/data/rat_plot_final.csv")
# 
# # doing the business
# ########
# 
# out.r <- pred.log
# 
# out.r <- out.r %>%
#   mutate(grid = factor(grid),
#          month = factor(month),
#          valley = factor(valley),
#          controlT = factor(controlT),
#          control = NA,
#          trip = as.numeric(trip))
# 
# str(out.r$trip)
# 
# # correct labels....
# for(i in 1:length(out.r$valley)) {
#   out.r$control[i]  <-  ifelse(out.r$valley[i] == "hol" & out.r$trip[i] > 12, "control", "no control")
#   out.r$control[i]  <-  ifelse(out.r$valley[i] == "egl", "control", out.r$control[i])
# }
# 
# # merge control and valley for plotting
# out.r <- mutate(out.r, Conditions = paste(control, valley))
# out.r$Conditions <- factor(out.r$Conditions, levels = c("control egl", "no control hol", "control hol"))
# 
# 
# # predict lines for each valleyhead(out.r)
# # predict lines for each valley
# 
# pred.lines <- out.r %>%
#   # filter(month == "Aug") %>%
#   select(controlT, month, b0, b.seed, b.dens, b.rat, se.r, Conditions) %>%
#   group_by(month) %>%
#   summarise(b0 = mean(b0),
#             b.seed = mean(b.seed),
#             b.dens = mean(b.dens),
#             b.rat = mean(b.rat),
#             se.r = mean(se.r)) %>%
#   arrange(month) %>%
#   ungroup()
# 
# pred.lines$b.seed
# 
# 
# 
# 
# 
# 
# 
# # what is this actually doing???
# # finds average of each month and each parameter
# # average for rats
# pred.lines <- out.r %>%
#   select(controlT, b0, b.seed, b.dens, b.rat, month) %>%
#   mutate(paras = paste(controlT, month)) %>%
#   group_by(controlT, month) %>% 
#   summarise(first = head(paras,1), 
#             count = n_distinct(paras),
#             b0 = mean(b0),
#             b.seed = mean(b.seed),
#             b.dens = mean(b.dens),
#             b.rat = mean(b.rat)) %>%
#   ungroup()
# 
# pred.lines %>%
#   summarise(b0 = mean(b0),
#             b.seed = mean(b.seed),
#             b.dens = mean(b.dens),
#             b.rat = mean(b.rat)
#             ) %>%
#   ungroup()
# 
# # max seed = 
# max.seed <-max(seed$seedm2)
# m.seed <-mean(seed$seedm2)
# 
# -0.489+0.474*log(max.seed)
# -0.489+0.474*log(m.seed)
# 
# 
# # does it fit with max r?
# max(out.r$mean.r)
# max(out.r$ucl.r)
# # desnity effect at mice = 100
# 
# -0.489
# 
# 
# # extract rat mean.se
# pred.lines <- out.rat %>%
#   summarise(b.rat = mean(mean.af),
#             rat.se = mean(se.af))
# 
# pred.lines
# 
# # density of mice.se
# # extract mean.se
# pred.lines.1 <- out.dens %>%
#   # group_by(month,controlT) %>%
#   summarise(b.dens = mean(mean.af),
#             dens.se = mean(se.af))
# 
# pred.lines.1
# 
# # density of mice.se
# # extract mean.se
# pred.lines.2 <- out.seed %>%
#   # group_by(month,controlT) %>%
#   summarise(b.seed = mean(mean.af),
#            seed.se = mean(se.af))
# 
# # CV = (SD/xbar) * 100.
# pred.lines[2]/pred.lines[1]
# pred.lines.1[2]/pred.lines.1[1]
# 
# pred.lines.2[2]/pred.lines.2[1]
# 
# 
# 
# # August data only and estimates from model
# aug.out <- filter(out.r, month == "Aug")
# 
# head(aug.out)
# seed.sim <- seq(0, 2.8, 0.01)       #mean()
# density.sim <- rep(mean(aug.out$lag.N),length(seed.sim))  #mean overall control/valleys?? does this make sense
# rat.sim <- rep(mean(aug.out$lag.rat.mna,length(seed.sim)))
# 
# # data for the four regression lines
# # function to create regression fit for one valley and month
# cd <- function(controlT, month, b0, b.seed, b.dens, b.rat) {
#   ss <- seed.sim
#   dd <- density.sim
#   rr <- rat.sim
#   yy <- b0 + b.seed * ss + b.dens * dd + b.rat * rr
#   return(data.frame(controlT = controlT, month = month, 
#                     yy = yy, ss = ss))
# }
# 
# pred.lines.a <- filter(pred.lines, month == "Aug")
# 
# # data to generate regression lines for hypothesis 1
# eglC.aug <- cd(b0 = pred.lines.a[1, 3], b.seed = pred.lines.a[1, 4],
#                b.dens = pred.lines.a[1, 5], b.rat = pred.lines.a[1, 6],
#                "eglC", "Aug")
# 
# holC.aug <- cd(b0 = pred.lines.a[2, 3], b.seed = pred.lines.a[2, 4], "hol", "Aug")
# holN.aug <- cd(b0 = pred.lines.a[3, 3], b.seed = pred.lines.a[3, 4], "egl", "Aug")
# 
# pred <- bind_rows(eglC.aug, holN.aug, holC.aug)
# 
# 
# # simple does it,,,,,,bob
# yy <- rep(NA,length(seed.sim))
# seed.sim <- seq(0, 2.8, 0.01)
# 
# 
# # data for the four regression lines
# # function to create regression fit for one valley and month
# cd <- function(controlT, month, yy, ss, b0, b.seed) {
#   ss <- ss
#   yy <- b0 + b.seed * ss
#   return(data.frame(yy = yy, ss = ss))
# }
# 
# pred.lines.a <- filter(pred.lines, month == "Aug")
# 
# 
# # data to generate regression lines for hypothesis 1
# eglC.aug <- cd(yy = yy, ss= ss, 
#                b0 = pred.lines.a[1, 3], b.seed = pred.lines.a[1, 4],
#                "eglC", "Aug")
# 
# 
# 
# 
# 
# # old school loop
# # simple does it,,,,,,bob
# ss <- seq(-4, 4, 0.001)
# yy <- rep(NA,length(ss))
# b0 <- as.numeric(pred.lines.a[1, 3])
# b.seed <- as.numeric(pred.lines.a[1, 4])
# 
# for(i in 1: length(yy)) {
# yy[i] <- b0 + b.seed * ss[i]
# }
# 
# model.e <-  data.frame(c(yy),c(ss))
# names(model.e) <- c("mice", "seed")
# model.e$Conditions <- "control egl"
# head(model.e)
# tail(model.e)
# 
# 
# # hol control
# # old school loop
# # simple does it,,,,,,bob
# ss <- seq(-4, 4, 0.001)
# yy <- rep(NA,length(ss))
# b0 <- as.numeric(pred.lines.a[2, 3])
# b.seed <- as.numeric(pred.lines.a[2, 4])
# 
# for(i in 1: length(yy)) {
#   yy[i] <- b0 + b.seed * ss[i]
# }
# 
# model.hc <-  data.frame(c(yy),c(ss))
# names(model.hc) <- c("mice", "seed")
# model.hc$Conditions <- "control hol"
# head(model.hc)
# tail(model.hc)
# 
# 
# 
# # hol stoats
# # old school loop
# # simple does it,,,,,,bob
# ss <- seq(-4, 4, 0.001)
# yy <- rep(NA,length(ss))
# b0 <- as.numeric(pred.lines.a[3, 3])
# b.seed <- as.numeric(pred.lines.a[3, 4])
# 
# for(i in 1: length(yy)) {
#   yy[i] <- b0 + b.seed * ss[i]
# }
# 
# model.hs <-  data.frame(c(yy),c(ss))
# names(model.hs) <- c("mice", "seed")
# model.hs$Conditions <- "no control hol"
# head(model.hs)
# tail(model.hs)
