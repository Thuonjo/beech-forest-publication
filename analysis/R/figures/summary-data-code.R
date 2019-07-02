#full mouse dataset first so each extra bind adds NA to turn to zeros to plot
meanM <- abund.dat5 %>%
  # select (valley, grid ,trip ,mean.lam) %>%
  select (grid ,trip ,N,valley) %>%
  transmute(trip = trip,
            grid = as.factor(grid),
            est.dat = N,
            valley = as.factor(valley),
            spp = "mice")

# table(meanM$grid)
# table(meanS$grid)

# rats have only 80 estimates not 144
meanR <- read_csv("C://Code/data/mna_allrat.csv") %>%
  select (valley, grid ,trip.no ,n) %>%
  # select (grid ,trip.no ,n) %>%
  transmute(trip = trip.no,
            grid = as.factor(grid),
            est.dat = n,
            valley = as.factor(valley),
            spp = "rats")

meanS <- n.seed.2 %>%
  ungroup()%>%
  select (grid ,trip.no ,cum.seed) %>%
  transmute(trip = trip.no,
            grid = ifelse(grid == "egl R1" , NA, grid),
            grid = ifelse(grid == "hol R1" , NA, grid),
            grid = ifelse(grid == "egl R2" , NA, grid),
            grid = ifelse(grid == "hol R2" , NA, grid),
            est.dat = cum.seed,
            spp = "seed") %>%
  drop_na() %>%
  droplevels()

#bind rats and dataframe should stay the same
dat.msr <- bind_rows(meanM, meanS, meanR)

# table(dat.msr$trip)

meanM.R.S <-   dat.msr %>%
  mutate(control = NA,
         group = as.factor(paste(trip,grid)), 
         grid = as.factor(grid),
         valley = ifelse(grepl("egl", grid), "egl","hol" ),
         grid = ifelse(grid == "egl R1" , NA, grid),
         grid = ifelse(grid == "hol R1" , NA, grid),
         grid = ifelse(grid == "egl R2" , NA, grid),
         grid = ifelse(grid == "hol R2" , NA, grid))

# correct labels....
for(i in 1:length(meanM.R.S$valley)) {
  meanM.R.S$control[i]  <-  ifelse(meanM.R.S$valley[i] == "hol" & meanM.R.S$trip[i] > 12, "control", "no control")
  meanM.R.S$control[i]  <-  ifelse(meanM.R.S$valley[i] == "egl", "control", meanM.R.S$control[i])
}

dat.msr.1 <- meanM.R.S %>%
  #drop_na() %>%
  mutate(valley = as.factor(valley),
         Conditions = paste(control, valley),
         Conditions = as.factor(Conditions),
         spp = as.factor(spp),
         control = as.factor(control),
         true.date = as.factor(trip))

dat.msr.1$Conditions <- factor(dat.msr.1$Conditions, levels = c("control egl", "no control hol", "control hol"))

levels(dat.msr.1$true.date) <- as.Date(as.character(c("1999-05-01","1999-08-01","1999-11-01",
                                                      "2000-02-01","2000-05-01","2000-08-01","2000-11-01",
                                                      "2001-02-01","2001-05-01","2001-08-01","2001-11-01",
                                                      "2002-05-01","2002-11-01",
                                                      "2003-02-01","2003-05-01","2003-08-01","2003-11-01",
                                                      "2004-02-01","2004-05-01","2004-08-01")))

# table(dat.msr.1$trip,dat.msr.1$true.date)

#summary plotting dataset
mean1 <- dat.msr.1 %>%
  group_by(Conditions,spp,trip,valley,true.date) %>%
  summarise(mean.s = mean(est.dat),
            sd.s = sd(est.dat),
            se.s = sd(est.dat)/sqrt(length(est.dat))*1.96,
            lcl.s = mean(est.dat) - (sd(est.dat)/sqrt(length(est.dat))*1.96),
            ucl.s = mean(est.dat) + (sd(est.dat)/sqrt(length(est.dat))*1.96)
  )

# glimpse(mean1)

#more summaries
mean2 <- dat.msr.1 %>%
  group_by(spp,trip) %>%
  summarise(mean.s = mean(est.dat),
            sd.s = sd(est.dat),
            se.s = sd(est.dat)/sqrt(length(est.dat))*1.96,
            lcl.s = mean(est.dat) - (sd(est.dat)/sqrt(length(est.dat))*1.96),
            ucl.s = mean(est.dat) + (sd(est.dat)/sqrt(length(est.dat))*1.96)
  )

# glimpse(mean2)

#summary plotting dataset
mean3 <- dat.msr.1 %>%
  group_by(spp,valley) %>%
  summarise(mean.s = mean(est.dat),
            sd.s = sd(est.dat),
            se.s = sd(est.dat)/sqrt(length(est.dat))*1.96,
            lcl.s = mean(est.dat) - (sd(est.dat)/sqrt(length(est.dat))*1.96),
            ucl.s = mean(est.dat) + (sd(est.dat)/sqrt(length(est.dat))*1.96)
  )

# glimpse(mean3)

