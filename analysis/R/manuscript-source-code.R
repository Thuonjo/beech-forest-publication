# for manuscript source code...
#march 2019
# Abundance ---------------------------------------------------------------
# create new variables
## variable of trips numbers as a vector
# it is the REVERSE!?!
# [trip and grid]
# uses model output labeled
# raw counts (ind) into CR model
source("./R/wrangling/Data_CRinput_mice_jan2019.R", echo = FALSE)

#month levels to get it right
month_levels <-
  c("Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec")

#output data
# CR.model
# desktop
CR.model.out <- readRDS("C://Code/final_cauchy_2_5.rds")

# for now this is hand selected...
abund.dat <- data.frame(CR.model.out$summary[, c(1, 2, 3, 7)])

#renaming variables
names(abund.dat) <- c("N", "se.N", "lcl.N", "ucl.N")
abund.dat$var <- rownames(abund.dat)

#reduce dataset to 136 or 144 rows
## right now it is raw data
abund.dat1 <- abund.dat %>%
  filter(substr(var, 1, 1) == "N")

#extracting grids and trips
#function
library(stringr)

#remove numbers function
# numextract <- function(string){
#   str_extract(string, "\\-*\\d+\\.*\\d*")
# }
# unit <- str_split_fixed(dat$var, concentration, n=2)[,2]
# #remove characters function
# charextract <- function(string){
#   str_extract(string, "\\-*\\d+\\.*\\d*")
# }
# numextract(abund.dat1$var)
# charextract(abund.dat1$var)

# abund.dat1

#tidyverse way
# abund.dat1 %<%
#   mutate(grid = (var,

new_dat <- as.character(abund.dat1$var)

#grids
new_var <- case_when(
  str_detect(new_dat, ",1]") ~ "egl M1",
  str_detect(new_dat, ",2]") ~ "egl M2",
  str_detect(new_dat, ",3]") ~ "egl MR1",
  str_detect(new_dat, ",4]") ~ "egl MR2",
  str_detect(new_dat, ",5]") ~ "hol M1",
  str_detect(new_dat, ",6]") ~ "hol M2",
  str_detect(new_dat, ",7]") ~ "hol MR1",
  str_detect(new_dat, ",8]") ~ "hol MR2",
  TRUE ~ "other"
)

# trips
new_var_1 <-
  case_when(
    str_detect(string = new_dat, pattern = "N\\[1,") ~ 1,
    str_detect(string = new_dat, pattern = "N\\[2,") ~ 2,
    str_detect(string = new_dat, pattern = "N\\[3,") ~ 3,
    str_detect(string = new_dat, pattern = "N\\[4,") ~ 4,
    str_detect(string = new_dat, pattern = "N\\[5,") ~ 5,
    str_detect(string = new_dat, pattern = "N\\[6,") ~ 6,
    str_detect(string = new_dat, pattern = "N\\[7,") ~ 7,
    str_detect(string = new_dat, pattern = "N\\[8,") ~ 8,
    str_detect(string = new_dat, pattern = "N\\[9,") ~ 9,
    str_detect(string = new_dat, pattern = "N\\[10,") ~ 10,
    str_detect(string = new_dat, pattern = "N\\[11,") ~ 11,
    str_detect(string = new_dat, pattern = "N\\[12,") ~ 12,
    str_detect(string = new_dat, pattern = "N\\[13,") ~ 13,
    str_detect(string = new_dat, pattern = "N\\[14,") ~ 14,
    str_detect(string = new_dat, pattern = "N\\[15,") ~ 15,
    str_detect(string = new_dat, pattern = "N\\[16,") ~ 16,
    str_detect(string = new_dat, pattern = "N\\[17,") ~ 17,
    str_detect(string = new_dat, pattern = "N\\[18,") ~ 18,
    str_detect(string = new_dat, pattern = "N\\[19,") ~ 19,
    str_detect(string = new_dat, pattern = "N\\[20,") ~ 20
  )

# loop ?

#reduce dataset to 136 or 144 rows
## right now it is raw data
abund.dat1 <- abund.dat %>%
  filter(substr(var, 1, 1) == "N") %>%
  mutate(
    grid = case_when(
      str_detect(var, ",1]") ~ "egl M1",
      str_detect(var, ",2]") ~ "egl M2",
      str_detect(var, ",3]") ~ "egl MR1",
      str_detect(var, ",4]") ~ "egl MR2",
      str_detect(var, ",5]") ~ "hol M1",
      str_detect(var, ",6]") ~ "hol M2",
      str_detect(var, ",7]") ~ "hol MR1",
      str_detect(var, ",8]") ~ "hol MR2",
      TRUE ~ "other"
    ),
    trip = case_when(
      str_detect(string = var, pattern = "N\\[1,") ~ 1,
      str_detect(string = var, pattern = "N\\[2,") ~ 2,
      str_detect(string = var, pattern = "N\\[3,") ~ 3,
      str_detect(string = var, pattern = "N\\[4,") ~ 4,
      str_detect(string = var, pattern = "N\\[5,") ~ 5,
      str_detect(string = var, pattern = "N\\[6,") ~ 6,
      str_detect(string = var, pattern = "N\\[7,") ~ 7,
      str_detect(string = var, pattern = "N\\[8,") ~ 8,
      str_detect(string = var, pattern = "N\\[9,") ~ 9,
      str_detect(string = var, pattern = "N\\[10,") ~ 10,
      str_detect(string = var, pattern = "N\\[11,") ~ 11,
      str_detect(string = var, pattern = "N\\[12,") ~ 12,
      str_detect(string = var, pattern = "N\\[13,") ~ 13,
      str_detect(string = var, pattern = "N\\[14,") ~ 14,
      str_detect(string = var, pattern = "N\\[15,") ~ 15,
      str_detect(string = var, pattern = "N\\[16,") ~ 16,
      str_detect(string = var, pattern = "N\\[17,") ~ 17,
      str_detect(string = var, pattern = "N\\[18,") ~ 18,
      str_detect(string = var, pattern = "N\\[19,") ~ 19,
      str_detect(string = var, pattern = "N\\[20,") ~ 20
    ),
    grid.n = as.numeric(factor(grid)),
    grid = factor(grid),
    trip.no = trip,
    valley = factor(ifelse(grepl("egl", grid), "egl", "hol"))
  )

#check
# head(abund.dat1)
# glimpse(abund.dat1)
# str gooood!
levels(abund.dat1$grid)

abund.dat2 <- abund.dat1 %>%
  mutate(control = case_when(
    trip > 12 & valley == "hol" ~ "H+",
    str_detect(string = valley, pattern = "egl") ~ "E+",
    str_detect(string = grid, pattern = "hol") ~ "H-"
  ))

# ,
# str_detect(string = grid, pattern = "hol" & trip > 12) ~ "H+"))


# table(abund.dat2$control, abund.dat2$trip)


#check
#no good
# head(abund.dat2)
# glimpse(abund.dat2)


###########cORRECT TO HERE#######################
#abundance must have correct joining variables
# grid needs to be .. "egl M1" etc
# so
#issue
# levels(as.factor(abund.dat2$grid.n))

#seed data file

#need mice first
source("./R/wrangling/Data_CRinput_mice_jan2019.R")
source("./R/wrangling/Data_input_allrats_jan2019.R")
source("./R/wrangling/Data_input_seed_jan2019.R")

# import seed dataset to jion
# this is modified in the seed R code....
# n.seed.2

# read_csv("C://Code/data/Seed_data_hol_egl_Figure2_data.csv")


# make sure data has grid and trip
# join by these with full data on left
# tibble(abund.dat2)
# ?tibble

# ISSUE WITH trip == nonsense numbers
red.seed <-
  select(n.seed.2, grid, trip, year, month, cum.seed) #%>%
  # filter(!trip.no == 1) %>%
  # mutate(trip = trip.no)


# full data
abund.dat3 <- bind_cols(abund.dat2,
                        red.seed) %>%
  mutate(
    N.seed = cum.seed / N,
    # N.seed = ifelse(cum.seed>0,cum.seed / N, cum.seed),
    log.seed = ifelse(cum.seed > 0, log10(cum.seed), cum.seed),
    control = NA,
    valley = ifelse(grepl("egl", grid), "egl", "hol")
  )

# abund.dat3$log.seed

# correct labels....
# Stoat control
for (i in 1:length(abund.dat3$valley)) {
  abund.dat3$control[i]  <-
    ifelse(abund.dat3$valley[i] == "hol" &
             abund.dat3$trip[i] > 12,
           "control",
           "no control")
  abund.dat3$control[i]  <-
    ifelse(abund.dat3$valley[i] == "egl", "control", abund.dat3$control[i])

}

# merge control and valley for plotting
# names(abund.dat3)
library(stringr)
# abund.dat3$grid

# abund.dat4 <- abund.dat3 %>%
grids.dat <-
  colsplit(
    string = abund.dat3$grid,
    pattern = " ",
    names = c("valley.rep", "grid.rats")
  )

abund.dat4 <- abund.dat3 %>%
  bind_cols(grids.dat) %>%
  mutate(
    grid.rats = factor(grid.rats, levels = c("M1", "M2", "MR1", "MR2")),
    Conditions = ifelse(
      grid.rats == "M1" | grid.rats == "M2",
      "rats.removed",
      "rats.present"
    )
  )


table(abund.dat4$Conditions, abund.dat4$valley, abund.dat4$control)

# create variable for grouping
abund.dat5 <- abund.dat4 %>%
  mutate(
    grouping.1 = ifelse(N < 49, "treat.lowN", "treat.highN"),
    grouping.2 = ifelse(N < 10, "treat.lowN", "treat.highN"),
    grouping.3 = ifelse(
      year == 2001 |
        year == 2002 | year == 2004,
      "treat.lowN",
      "treat.highN"
    ),
    grouping.4 = ifelse(year == 2001 |
                          year == 2002, "treat.lowN", "treat.highN"),
    true.date = as.Date(factor(
      trip,
      labels = c(
        "1999-05-01",
        "1999-08-01",
        "1999-11-01",
        "2000-02-01",
        "2000-05-01",
        "2000-08-01",
        "2000-11-01",
        "2001-02-01",
        "2001-05-01",
        "2001-08-01",
        "2001-11-01",
        "2002-05-01",
        "2002-11-01",
        "2003-02-01",
        "2003-05-01",
        "2003-08-01",
        "2003-11-01",
        "2004-02-01",
        "2004-05-01",
        "2004-08-01"
      )
    )),
    treat.six = paste(valley, control, Conditions)
  )

#check data
# summary(abund.dat5)
# glimpse(abund.dat5)
# Sorting factors for plots
#nice labels
# abund.dat5 %>%
#   mutate(Rats = factor(Conditions, labels = c("Full", "Reduced")),
#          Control = factor(control, labels = c("Yes", "No")),
#          Valley = factor(valley, labels = c("Eglinton", "Hollyford")),
#          Date = as.Date(true.date))


# Rate of increase --------------------------------------------------------

#genrated from functions as so
#function script
# source("./R/model/extract_variables.R")
# source("./R/wrangling/Data_CRinput_mice_jan2019.R")
#
# # outl.N
# # outl.r
#
# outl.N <- extract.var(model = CR.model.out,
#                       name.model = "full",
#                       var.to.extract = "lam",
#                       start.point = 1,
#                       ind.cap = ind.cap,
#                       mice = mice)
#
# outl.r <- extract.var(model = CR.model.out,
#                       name.model = "full",
#                       var.to.extract = "r",
#                       start.point = 2,
#                       ind.cap = ind.cap,
#                       mice = mice)


# combining n, seed and r -------------------------------------------------------
# this can be seen as the model output dataset.
out <- abund.dat5

# head(out)
#set factors.. this might not be correct
# out$grid <- as.factor(out$grid)

# checking missing data issues
# table(is.na(out.seed))

out.r <- out %>%
  mutate(lag.N = lag(N),
         sjt = lag(log.seed),
         valley = factor(valley, levels = c("egl", "hol"))) %>%
  filter(!is.na(lag.N) & !trip == 1)

# glimpse(out.r)
# %>%
# select("mean.lam","se.lam" ,"lcl.lam" ,"ucl.lam","trip","grid.n","grid", "valley","grid.id","min_ind","lag.N") # %>%

# names(out.N) <- c("estimate","se" ,"lcl" ,"ucl","trip","grid.n","grid", "valley","grid.id","min_ind","lag.N")
# out.N$var <- "N"
# include a column of lagged MNA for rats
lrmna <- as.vector(lag.rat.mna)
lrmna <- lrmna[is.na(lrmna) == F]
out.r$lag.rat.mna <- lrmna

# extracting rates of increase
# from scratch
# for now this is hand selected...
rate.dat <- data.frame(CR.model.out$summary[, c(1, 2, 3, 7)])

#renaming variables
names(rate.dat) <- c("mean.r", "se.r", "lcl.r", "ucl.r")
rate.dat$var <- rownames(rate.dat)

#reduce dataset to 136 or 144 rows
## right now it is raw data
rate.dat1 <- rate.dat %>%
  filter(substr(var, 1, 1) == "r")  %>%
  mutate(
    grid = case_when(
      str_detect(var, ",1]") ~ "egl M1",
      str_detect(var, ",2]") ~ "egl M2",
      str_detect(var, ",3]") ~ "egl MR1",
      str_detect(var, ",4]") ~ "egl MR2",
      str_detect(var, ",5]") ~ "hol M1",
      str_detect(var, ",6]") ~ "hol M2",
      str_detect(var, ",7]") ~ "hol MR1",
      str_detect(var, ",8]") ~ "hol MR2",
      TRUE ~ "other"
    ),
    trip = case_when(
      str_detect(string = var, pattern = "r\\[1,") ~ 1,
      str_detect(string = var, pattern = "r\\[2,") ~ 2,
      str_detect(string = var, pattern = "r\\[3,") ~ 3,
      str_detect(string = var, pattern = "r\\[4,") ~ 4,
      str_detect(string = var, pattern = "r\\[5,") ~ 5,
      str_detect(string = var, pattern = "r\\[6,") ~ 6,
      str_detect(string = var, pattern = "r\\[7,") ~ 7,
      str_detect(string = var, pattern = "r\\[8,") ~ 8,
      str_detect(string = var, pattern = "r\\[9,") ~ 9,
      str_detect(string = var, pattern = "r\\[10,") ~ 10,
      str_detect(string = var, pattern = "r\\[11,") ~ 11,
      str_detect(string = var, pattern = "r\\[12,") ~ 12,
      str_detect(string = var, pattern = "r\\[13,") ~ 13,
      str_detect(string = var, pattern = "r\\[14,") ~ 14,
      str_detect(string = var, pattern = "r\\[15,") ~ 15,
      str_detect(string = var, pattern = "r\\[16,") ~ 16,
      str_detect(string = var, pattern = "r\\[17,") ~ 17,
      str_detect(string = var, pattern = "r\\[18,") ~ 18,
      str_detect(string = var, pattern = "r\\[19,") ~ 19,
      str_detect(string = var, pattern = "r\\[20,") ~ 20
    ),
    grid.n = as.numeric(factor(grid)),
    grid = factor(grid),
    valley = factor(ifelse(grepl("egl", grid), "egl", "hol"))
  )

rate.dat2 <- rate.dat1 %>%
  select("mean.r",
         "se.r" ,
         "lcl.r" ,
         "ucl.r",
         "trip",
         "grid.n",
         "grid",
         "valley",
         "grid.n")

# glimpse(rate.dat2)
# glimpse(out.r)

out.full.136 <- left_join(out.r,
                          rate.dat2)

# %>%
#                   select()
#
# glimpse(out.full.136)

# variables ---------------------------------------------------------------
# adding parameter estimates as final data
# add in the appropriate parameter estimates
# parameters
out.para <- data.frame(CR.model.out$summary[, c(1, 2, 3, 7)])
# glimpse(out.para)

names(out.para) <- c("mean.b", "se.b", "lcl.b", "ucl.b")

out.para$var <- rownames(out.para)
var_names <- rownames(out.para)
# glimpse(out.para)

out.para1 <- out.para %>%
  filter(substr(var, 1, 1) == "b") %>%
  mutate(
    control = case_when(
      str_detect(var, "\\[1,") ~ "control",
      str_detect(var,  "\\[2,") ~ "no control",
      str_detect(var,  "\\[3,") ~ "control",
      TRUE ~ "other"
    ),
    valley = case_when(
      str_detect(var, "\\[1,") ~ "egl",
      str_detect(var,  "\\[2,") ~ "hol",
      str_detect(var,  "\\[3,") ~ "hol",
      TRUE ~ "other"
    ),
    month = case_when(
      str_detect(var, ",1]") ~ "Feb",
      str_detect(var, ",2]") ~ "May",
      str_detect(var, ",3]") ~ "Aug",
      str_detect(var, ",4]") ~ "Nov",
      TRUE ~ "other"
    )
  ) %>%
  filter(!valley == "other")


out.para1 %>%
  select(var, mean.b,valley, control,month) %>%
spread(key = var, value = mean.b, fill = FALSE)

# glimpse(out.para)

# unique(out.para1$)

# final parameter dataset -----------------------------------------------------------
# parameter outputs

#reducing the dataset to  averages over month, valley and control
#decided I cant do this and just want 3 prediction lines for each figure = 12*4 = 48

out.dat1 <- out.full.136 %>%
  group_by(valley, control, month) %>%
  summarise(dat.mice.mean = mean(N),
    dat.seed.mean = mean(sjt),
    dat.rat.mean = mean(lag.rat.mna),
    dat.mice.min = min(N),
    dat.seed.min = min(sjt),
    dat.rat.min = min(lag.rat.mna),
    dat.mice.max = max(N),
    dat.seed.max = max(sjt),
    dat.rat.max = max(lag.rat.mna)
  ) %>%
  ungroup() %>%
  droplevels() %>%
  select(valley,
         control,
         month,
         dat.seed.mean,
         dat.rat.mean,
         dat.mice.min,
         dat.seed.min,
         dat.rat.min,
         dat.mice.max,
         dat.seed.max,
         dat.rat.max)

out.para2 <- out.para1 %>%
      mutate(para = case_when(
      str_detect(var, "b0") ~ "b0",
      str_detect(var, "b.seed") ~ "b.seed",
      str_detect(var, "b.dens") ~ "b.dens",
      str_detect(var, "b.rat") ~ "b.rat",
      TRUE ~ "other"
    ))


# # merge parameter estimates into out.r
out.final <-left_join(out.para2,out.dat1, by = c("valley",  "control", "month"))

#remove error estimates for lines
rm.errors <- c("se.b", "lcl.b", "ucl.b", "var")

out.final1 <- out.para2 %>%
                select(-rm.errors)



# UP TO HERE!!!! ----------------------------------------------------------

# 48 estimated lines to predict from max and min values
# TO predict we need a new dataset for each predict pointpoints

# head(out.final1)

ggplot(out.final1, aes(y = mean.b, x = para, col = valley, shape = paste(control,valley))) +
  geom_point(size =4, alpha = 0.7, position = position_jitter(width = 0.3)) +
  facet_wrap(~month) +
  theme_stata() +
  theme(axis.line.x = element_line(size = 1))

# data for lines ----------------------------------------------------------
# Feb - seed
#feb filtered (parameters)
# flip dataset
out.para.feb <- out.final1 %>%
  filter(month == "Feb") %>%
    spread(key = para, value = mean.b)

#feb filtered (data)
out.dat1.feb <- out.dat1 %>%
  filter(month == "Feb")

#merge two datasets
feb.plot <- merge(out.para.feb,
                       out.dat1.feb)

#wil this work for all of feb plot lines
# seed-feb ----------------------------------------------------------------
#predict estimates for seed lines
seed.feb.plot1 <- feb.plot %>%
  mutate(pred.min = b0 + (b.seed * dat.seed.min),
    pred.max = (b0 + b.seed * dat.seed.max))

# data in tidyverse for ggplot2
out.seed.feb2 <- seed.feb.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.seed.min, dat.seed.max)) %>%
  select(valley, control, mean.r, lag.sjt) %>%
  mutate(treat = paste(valley, control))

# dens-feb ----------------------------------------------------------------
#spread
dens.feb.plot1 <- feb.plot %>%
  mutate(pred.min = b0 + (b.dens * dat.mice.min),
         pred.max = (b0 + b.dens * dat.mice.max))

#tidyverse
out.dens.feb2 <- dens.feb.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
  select(valley, control, mean.r, lag.sjt) %>%
  mutate(treat = paste(valley, control))


# rat-feb -----------------------------------------------------------------
#spread and predict
rat.feb.plot1 <- feb.plot %>%
  mutate(pred.min = b0 + (b.dens * dat.rat.min),
         pred.max = (b0 + b.dens * dat.rat.max))

#tidyverse
out.rat.feb2 <- rat.feb.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
  select(valley, control, mean.r, lag.sjt) %>%
  mutate(treat = paste(valley, control))

# data-outputs ------------------------------------------------------------

#r-bind document
# write.csv(out.final.2, "C://Code/data/allprediction-output-data.csv")

