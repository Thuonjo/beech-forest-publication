# High (B) abundance (n_jt) prediction 
# ANOVA and plot set-up
# March 2019

# reduce to low abundance only data only
high.abund.dat <- abund.dat5 %>%
  filter(trip.no == 6 | trip.no == 7 | trip.no == 16 | trip.no == 17 )  %>%
  mutate(n.seed.l = N/log.seed,
         n.seed.c = N/cum.seed) %>%
  droplevels()  %>%
  mutate(n.seed.l = ifelse(n.seed.l > 10000 , 0, n.seed.l),
         n.seed.c = ifelse(n.seed.c > 10000 , 0, n.seed.c))

# glimpse(high.abund.dat)
# table(high.abund.dat$trip, high.abund.dat$valley)
# unique(filter(high.abund.dat, trip == 6)$true.date)
# unique(filter(high.abund.dat, trip == 7)$true.date)
# unique(filter(high.abund.dat, trip == 16)$true.date)
# unique(filter(high.abund.dat, trip == 17)$true.date)

# summary for plots
# create summarised datasets of need
high.cond.sum <-  high.abund.dat %>%
  group_by(valley,control, Conditions) %>%
  summarise(N.count = n(),
            mean.seed.c = mean(n.seed.c, rm.na = TRUE),
            tvalue = qt(p = 0.025, df = N.count - 1),
            high.sd = sd(n.seed.c),
            high.se = sd(n.seed.c)/sqrt(N.count),
            lcl.high.tv = mean.seed.c - (tvalue* high.se),
            ucl.high.tv = mean.seed.c + (tvalue* high.se),
            lcl.high = mean.seed.c - (1.96* high.se),
            ucl.high = mean.seed.c + (1.96* high.se))

high.con <-  high.abund.dat %>%
  group_by(control) %>%
  summarise(N.count = n(),
            mean.seed.c = mean(n.seed.c, rm.na = TRUE),
            tvalue = qt(p = 0.025, df = N.count - 1),
            high.sd = sd(n.seed.c),
            high.se = sd(n.seed.c)/sqrt(N.count),
            lcl.high.tv = mean.seed.c - (tvalue* high.se),
            ucl.high.tv = mean.seed.c + (tvalue* high.se),
            lcl.high = mean.seed.c - (1.96* high.se),
            ucl.high = mean.seed.c + (1.96* high.se))


test.dat.2 <- high.abund.dat %>%
  select(N, valley, control, Conditions, trip, grid, n.seed.c)

high.plot.avo <-   high.abund.dat %>%
  mutate(pt.pts = as.numeric(factor(paste(valley, control,Conditions))),
         pt.ft = factor(paste(valley, control, Conditions)),
         pt.ft.g2 = factor(paste(valley, control, Conditions)),
         valley = factor(valley, labels =  c("Eglinton", "Hollyford")),
         Conditions = factor(Conditions),
         control = factor(control))

# names(high.plot.avo)
# factor(plot.dat2$pt.pts)
# factor(plot.dat2$pt.ft.g2)

levels(high.plot.avo$pt.ft.g2) <- c("egl control rats.present",
                                    "egl control rats.removed" ,    
                                    "hol no control rats.present", 
                                    "hol no control rats.removed",
                                    "hol control rats.present", 
                                    "hol control rats.removed")

levels(high.plot.avo$pt.ft.g2) <- c(3,3,2,2,2,2)




# test.dat.1 <- high.abund.dat %>%
#   select(N, valley, control, Conditions, trip, grid, n.seed.c)

# high.mod1
high.mod1 <- glm(n.seed.c ~ control, family = "gaussian", data = high.abund.dat)
summary.high.mod1 <- summary(high.mod1)

anova(high.mod1, test = "F")

# high.mod2
high.mod2 <- glm(n.seed.c ~ control + valley, family = "gaussian", data = high.abund.dat)

summary.high.mod2 <- summary(high.mod2)

anova(high.mod2, test = "F")

#high.mod3
high.mod3 <- glm(n.seed.c ~ valley + control + Conditions, family = "gaussian", data = high.abund.dat)
summary.high.mod3 <- summary(high.mod3)

anova(high.mod2, test = "F")

# family modelling options?
# high.mod3.1 <- glm(n.seed.c ~ valley + control + Conditions, family = "poisson", data = high.abund.dat)
# summary.high.mod3.1 <- summary(high.mod3.1)

#variables
mods.name <- c("high.mod1", "high.mod2", "high.mod3")
model.aic <- c(summary.high.mod1$aic,summary.high.mod2$aic,summary.high.mod3$aic)


mod.dev <- c(summary.high.mod1$deviance,summary.high.mod2$deviance,summary.high.mod3$deviance)



#dataset output
mod.selection <- tibble(model.aic = model.aic,
                        mods.name = mods.name,
                        mod.dev = mod.dev)


# output from models in workable format
co.effs <- c(row.names(as.data.frame(summary(high.mod3)$coefficients)))

# could plot from this
flextable::flextable(data.frame(co.effs, summary(high.mod3)$coefficients))

# summary
s.final.model <- summary(high.mod2)

# s.final.model$coefficients[2,4]

# p1 <- ggplot(high.abund.dat, aes(x = cum.seed)) + 
#   geom_histogram() + 
#   theme_classic()
# 
# # p2 <- ggplot(high.abund.dat, aes(x = n.seed.c)) + 
# #   geom_histogram() + 
# #   theme_classic()
# # 
# # cowplot::plot_grid(p1,p2, ncol = 2)
# # 
# # hist(high.abund.dat$n.seed.c)
# # hist(high.abund.dat$cum.seed)
# 
# # html and pdf only
# # jtools::summ(high.mod2.n)
# 
# # ??jtools
# # parameters for extraction
# # plot_summs(high.mod2, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
# 
# #could plot from this
# # summary(high.mod3)
# # anova( high.mod2)
# 
# # # summary
# # model.summary1 <- summary( high.mod2)
# # 
# # jtools::summ( high.mod2)
# # 
# # # ??jtools
# # # parameters for extraction
# # 
# # plot_summs( high.mod3, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)# 1.  All population estimates below or above a number
# # 2.  All estimates at winter (hypo 2) and summer (hypo 4)
# # 3.  ????
# 
# # for now I think that the best method to do this
# # is to address the seasons of differnce (3.)
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
#   # select (valley, grid ,trip ,n) %>%
#   select (grid ,trip ,n) %>%
#   transmute(trip = trip,
#             grid = as.factor(grid),
#             est.dat = n,
#             # valley = as.factor(valley),
#             spp = "rats")
# 
# meanS <- read_csv("C://Code/data/old data/Seed_data_hol_egl_total.csv") %>%
#   select (grid ,trip ,seed) %>%
#   transmute(trip = trip,
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
