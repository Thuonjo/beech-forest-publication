# Low (A) and High (B) abundance (n_jt) prediction 
# ANOVA and plot set-up
# March 2019

# selected group
# reduce to low abundance only data only
low <- c(10,11,12,13,14,15) 

# head(abund.dat5)
# str(abund.dat5$trip)
# out.N comes from source(manuscript-source-code.R)

low.abund.dat <- abund.dat5 %>% 
  # mutate(N = mean.lam) %>%  # for when using out.N
  filter(trip == 10 | trip == 11 | trip == 12 | trip == 13 | trip == 14 | trip == 15)

# filter(low.abund.dat, trip.no == 15)$true.date

# summary for plots
# create summarised datasets of need
low.cond.sum <- low.abund.dat %>%
  group_by(valley, control,Conditions) %>%
  summarise(N.count = n(),
            N = mean(N, rm.na = TRUE),
            tvalue = qt(p = 0.025, df = N.count - 1, lower.tail = FALSE),
            low.se = mean(se.N),
            lcl.low.tv = N - (tvalue*low.se),
            ucl.low.tv = N + (tvalue*low.se),
            lcl.low = N - (1.96*low.se),
            ucl.low = N + (1.96*low.se))

low.con <- low.abund.dat %>%
  group_by(control)  %>%
  summarise(N.count = n(),
            N = mean(N, rm.na = TRUE),
            tvalue = qt(p = 0.025, df = N.count - 1, lower.tail = FALSE),
            low.se = mean(se.N),
            lcl.low.tv = N - (tvalue*low.se),
            ucl.low.tv = N + (tvalue*low.se),
            lcl.low = N - (1.96*low.se),
            ucl.low = N + (1.96*low.se))

low.con1 <- low.con %>%
              mutate(lcl.low = ifelse(lcl.low < 0, 0, lcl.low))

# test.dat.1 <- low.abund.dat %>%
#   select(N, valley, control, Conditions, trip, grid, grid.n, month,year)

low.mod1 <- glm(N ~ control, family = "gaussian", data = low.abund.dat)
summary.low.mod1 <- summary(low.mod1)

low.mod2 <- glm(N ~ control + valley, family = "gaussian", data = low.abund.dat)
summary.low.mod2 <- summary(low.mod2)

low.mod3 <- glm(N ~ control + valley + Conditions, family = "gaussian", data = low.abund.dat)
summary.low.mod3 <- summary(low.mod3)

low.mod3.log <- glm(log(N) ~ control + valley + Conditions, family = "gaussian", data = low.abund.dat)
summary.low.mod3.log <- summary(low.mod3.log)
# summaries
# summary(low.mod4)
# html and pdf only
# jtools::summ(low.mod2.n)

# ??jtools
# parameters for extraction
# plot_summs(low.mod2, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

co.effs <- c(row.names(as.data.frame(summary(low.mod3)$coefficients)))

# summary
s.final.model <- summary(low.mod3)
# s.final.model$coefficients[2,4]

anova(low.mod3, test = "F")

# family modelling options?
# low.mod3.1 <- glm(n.seed.c ~ valley + control + Conditions, family = "poisson", data = low.abund.dat)
summary.low.mod3<- summary(low.mod3)

#variables
mods.name <- c("low.mod1", "low.mod2", "low.mod3")
model.aic <- c(summary.low.mod1$aic,summary.low.mod2$aic,summary.low.mod3$aic)


mod.dev <- c(summary.low.mod1$deviance,summary.low.mod2$deviance,summary.low.mod3$deviance)



#dataset output
mod.selection <- tibble(model.aic = model.aic,
                        mods.name = mods.name,
                        mod.dev = mod.dev)


# 
# 
# # NOT USED!!
#   
# # data --------------------------------------------------------------------
# #import data
# source("./R/wrangling/Data_CRinput_mice_jan2019.R")
# 
# # Root to core computer as cant push to git... toooo much
# model <- readRDS("C://Code/final_cauchy_2_5.rds")
# 
# # N_trip
# # n.trip
# # N_grid
# # n.grid
# 
# #full mouse dataset first so each extra bind adds NA to turn to zeros to plot
# meanM <- read_csv("C://Code/data/old data/CR_output_N.csv") %>%
#   # select (valley, grid ,trip ,mean.lam) %>%
#   select (grid ,trip ,mean.lam,valley) %>%
#   transmute(trip = trip,
#             grid = as.factor(grid),
#             est.dat = mean.lam,
#             valley = as.factor(valley),
#             spp = "mice")
# 
# # table(meanM$grid)
# # table(meanS$grid)
# 
# # rats have only 80 estimates not 144
# meanR <- read_csv("C://Code/data/mna_allrat.csv") %>%
#   # select (valley, grid ,trip.no ,n) %>%
#   select (grid ,trip.no ,n) %>%
#   transmute(trip = trip.no,
#             grid = as.factor(grid),
#             est.dat = n,
#             # valley = as.factor(valley),
#             spp = "rats")
# 
# meanS <- read_csv("C://Code/data/old data/Seed_data_hol_egl_total.csv") %>%
#   select (grid ,trip.no ,seed) %>%
#   transmute(trip = trip.no,
#             grid = ifelse(grid == "egl R1" , NA, grid),
#             grid = ifelse(grid == "hol R1" , NA, grid),
#             grid = ifelse(grid == "egl R2" , NA, grid),
#             grid = ifelse(grid == "hol R2" , NA, grid),
#             est.dat = seed,
#             spp = "seed") %>%
#   drop_na() %>%
#   droplevels()
# 
# #bind rats and dataframe should stay the same
# dat.msr <- bind_rows(meanM, meanS, meanR)
# 
# # table(dat.msr$trip)
# 
# meanM.R.S <-   dat.msr %>%
#   mutate(control = NA,
#          group = as.factor(paste(trip,grid)), 
#          grid = as.factor(grid),
#          valley = ifelse(grepl("egl", grid), "egl","hol" ),
#          grid = ifelse(grid == "egl R1" , NA, grid),
#          grid = ifelse(grid == "hol R1" , NA, grid),
#          grid = ifelse(grid == "egl R2" , NA, grid),
#          grid = ifelse(grid == "hol R2" , NA, grid))
# 
# # correct labels....
# for(i in 1:length(meanM.R.S$valley)) {
#   meanM.R.S$control[i]  <-  ifelse(meanM.R.S$valley[i] == "hol" & meanM.R.S$trip[i] > 12, "control", "no control")
#   meanM.R.S$control[i]  <-  ifelse(meanM.R.S$valley[i] == "egl", "control", meanM.R.S$control[i])
# }
# 
# dat.msr.1 <- meanM.R.S %>%
#   #drop_na() %>%
#   mutate(valley = as.factor(valley),
#          Conditions = paste(control, valley),
#          Conditions = as.factor(Conditions),
#          spp = as.factor(spp),
#          control = as.factor(control),
#          true.date = as.factor(trip))
# 
# dat.msr.1$Conditions <- factor(dat.msr.1$Conditions, levels = c("control egl", "no control hol", "control hol"))
# 
# levels(dat.msr.1$true.date) <- as.Date(as.character(c("1999-05-01","1999-08-01","1999-11-01",
#                                                       "2000-02-01","2000-05-01","2000-08-01","2000-11-01",
#                                                       "2001-02-01","2001-05-01","2001-08-01","2001-11-01",
#                                                       "2002-05-01","2002-11-01",
#                                                       "2003-02-01","2003-05-01","2003-08-01","2003-11-01",
#                                                       "2004-02-01","2004-05-01","2004-08-01")))
# # table(dat.msr.1$trip,dat.msr.1$true.date)
# 
# #summary plotting dataset
# mean1 <- dat.msr.1 %>%
#   group_by(Conditions,spp,trip,valley,true.date) %>%
#   summarise(mean.s = mean(est.dat),
#             sd.s = sd(est.dat),
#             se.s = sd(est.dat)/sqrt(length(est.dat))*1.96,
#             lcl.s = mean(est.dat) - (sd(est.dat)/sqrt(length(est.dat))*1.96),
#             ucl.s = mean(est.dat) + (sd(est.dat)/sqrt(length(est.dat))*1.96)
#   )
# 
# # glimpse(mean1)
# 
# #more summaries
# mean2 <- dat.msr.1 %>%
#   group_by(spp,trip) %>%
#   summarise(mean.s = mean(est.dat),
#             sd.s = sd(est.dat),
#             se.s = sd(est.dat)/sqrt(length(est.dat))*1.96,
#             lcl.s = mean(est.dat) - (sd(est.dat)/sqrt(length(est.dat))*1.96),
#             ucl.s = mean(est.dat) + (sd(est.dat)/sqrt(length(est.dat))*1.96)
#   )
# 
# # glimpse(mean2)
# 
# #summary plotting dataset
# mean3 <- dat.msr.1 %>%
#   group_by(spp,valley) %>%
#   summarise(mean.s = mean(est.dat),
#             sd.s = sd(est.dat),
#             se.s = sd(est.dat)/sqrt(length(est.dat))*1.96,
#             lcl.s = mean(est.dat) - (sd(est.dat)/sqrt(length(est.dat))*1.96),
#             ucl.s = mean(est.dat) + (sd(est.dat)/sqrt(length(est.dat))*1.96)
#   )
# 
# # glimpse(mean3)
# 
