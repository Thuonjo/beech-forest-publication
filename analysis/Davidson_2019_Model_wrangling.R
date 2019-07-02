## ----global-options, message=FALSE, warning=FALSE, include=FALSE---------
# export .r code only
# knitr::purl("./Davidson_2019_BeechForest.Rmd")

# render draft to webpage
# rmarkdown::render(input = "Davidson_2019_BeechForest.Rmd")
# ,
#                   output_format = "html_document",
#                   output_file = "Davidson_2019_t.html")

#render nb.html for webpage
# rmarkdown::render(input = "Davidson_2019_BeechForest.Rmd", output_format = "html_notebook", output_dir = "_posts")

# compareGroups::cGroupsGUI(mtcars)
# document global rules
knitr::opts_chunk$set(comment=NA,
                      fig.path = "./figs/",
                      echo=FALSE, 
                      fig.height=6, 
                      fig.width=10,
                      message=FALSE, 
                      warning=FALSE)

# data
CR.model.out <- readRDS("C://Code/final_cauchy_2_5.rds")


## ----sorting-references, eval=FALSE, include=FALSE-----------------------
## library(citr)
## citr::md_cite("Beech-forest.bib")
## 
## # bib(file = "Beech-forests.bib")


## ----eniviroment, message=FALSE, warning=FALSE, include=FALSE------------
# # libraries needed
source("./R/r-packages-needed.R", echo = FALSE)
source("./R/theme_raw_fig3s.r", echo = FALSE)
source("./R/davidson_2019_theme.r", echo = FALSE)
# source("./rcode/manuscript-source-code.R")


## ----import-data-previous-data-------------------------------------------

# All parameter estimates for tables and plots

# Overall Data
# source("./Rcode/manuscript-source-code.R", echo = FALSE)

# data --------------------------------------------------------------------
# export data
plot.dat.all1 <- read_csv("C://Code/data/plot-all-data1.csv")
# out.final <- read_csv("./data/final-outputs.csv")
# abund.dat5 <- read_csv("./data/abundance5.csv")

# plot
#plot code
# glimpse(out.final)
# glimpse(abund.dat5)
glimpse(plot.dat.all1)

# combining n, seed and r -------------------------------------------------------
# this can be seen as the model output dataset.
plot.dat.all1 <- plot.dat.all1 %>%
  mutate(trip = as.numeric(trip))
# already done!

# rats have only 80 estimates not 144
meanR <- read_csv("C://Code/data/mna_allrat.csv") 
# str(meanR)
# glimpse(meanR)
# str(plot.dat.all1) 
# str(joined.rats)

meanR <- meanR %>%
  select (valley, grid ,trip.no ,n) %>%
  # select (grid ,trip.no ,n) %>%
  mutate(trip = as.numeric(trip.no),
            grid = grid,
            rat.mna = n,
         lag.rat.mna = lag(n),
         Valley = valley)

joined.rats <- full_join(plot.dat.all1, meanR, by = c("valley", "trip", "grid")) %>%
                mutate(Valley = valley)


## ------------------------------------------------------------------------
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
    grid = as.character(grid),
    valley = as.character(ifelse(grepl("egl", grid), "egl", "hol"))
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

out.full.136 <- left_join(joined.rats,
                          rate.dat2)

# %>%
#                   select()
#
glimpse(out.full.136)

out.full.136 <- out.full.136 %>%
  mutate(Control = control,
         Valley = valley)


## ------------------------------------------------------------------------
# # Cleaning up total dataset
# plot.dat.all1  <- plot.dat.all1 %>%
#   mutate(trip = as.character(trip))# already done!
# 
# # rats have only 80 estimates not 144
# meanR <- read_csv("./data/mna_allrat.csv") %>%
#   select (valley, grid ,trip.no ,n) %>%
#   # select (grid ,trip.no ,n) %>%
#   mutate(trip = as.character(trip.no),
#             grid = grid,
#             lag.rat.mna = n,
#          Valley = valley)
# 
# joined.rats <- left_join(plot.dat.all1, meanR, by = c("grid", "Valley", "trip"))


## ----parameter-data------------------------------------------------------
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
  )
  

# final parameter dataset -----------------------------------------------------------
# parameter outputs
out.dat1 <- out.full.136 %>%
  group_by(Valley, Control, month) %>%
  summarise(dat.mice.mean = mean(N),
    dat.seed.mean = mean(log.seed),
    dat.rat.mean = mean(lag.rat.mna, na.rm = TRUE),
    dat.mice.min = min(N),
    dat.seed.min = min(log.seed),
    dat.rat.min = min(lag.rat.mna, na.rm = TRUE),
    dat.mice.max = max(N),
    dat.seed.max = max(cum.seed),
    dat.rat.max = max(lag.rat.mna)
  ) %>%
  ungroup() %>%
  select(Valley,
         Control,
         month,
         dat.seed.mean,
         dat.rat.mean,
         dat.mice.min,
         dat.seed.min,
         dat.rat.min,
         dat.mice.max,
         dat.seed.max,
         dat.rat.max)%>%
  droplevels() 

glimpse(out.dat1)
# names(plot.dat.all1)

out.para2 <- out.para1[1:48,]

out.para3 <- out.para2 %>%
      mutate(para = case_when(str_detect(var, "b0") ~ "b0", 
      str_detect(var, "b.seed") ~ "b.seed",
      str_detect(var, "b.dens") ~ "b.dens",
      str_detect(var, "b.rat") ~ "b.rat"),
      Control = control,
      Valley = valley)

# # merge parameter estimates into out.r
out.final <-left_join(out.para3,out.dat1, by = c("Valley",  "Control", "month"))

#remove error estimates for lines
rm.errors <- c("se.b", "lcl.b", "ucl.b", "var")

out.final1 <- out.para3 %>%
                select(-rm.errors)

para.plot.dat <- out.final1 %>%
  mutate_if(is.factor, as.character) %>%
  # filter(month == "Feb") %>%
    droplevels() %>%
  
                  mutate(Estimate = mean.b,
                         Control = control,
                         Valley = valley,)

### With para errors
para.plot.erors <- out.para3 %>%
  mutate_if(is.factor, as.character) %>%
  # filter(month == "Feb") %>%
    droplevels() %>%
  
                  mutate(Estimate = mean.b,
                         Control = control,
                         Valley = valley,)

para.plot.dat$para <- factor(para.plot.dat$para, labels = c("Density", "Rats", "Seed", "Intercept"))


## ------------------------------------------------------------------------
# Rate of increase between Autumn and Winter
# (May and August rate of change)
# May mice abundace, seed and rat data
# Overall Data
# source("./Rcode/manuscript-source-code.R", echo = FALSE)

# final plot --------------------------------------------------------------

# pc.seed.full <-
pC.plot.3 <- out.full.136 %>%
  filter(month == "Feb")

# pc.plot.feb <- 
ggplot(pC.plot.3, aes(y = mean.r, x = log.seed)) +
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
  geom_point(aes(colour = valley,shape = valley, fill = control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
  #   geom_line(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt, group = treat),size = 0.75, alpha = 0.7) +
  # geom_point(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt,shape = valley, fill = control), size = 5) +

# facet_wrap(~month, scales = "free")+
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod"))


## ------------------------------------------------------------------------
# pc.seed.full <-
pC.plot.3 <- out.full.136 %>%
  filter(month == "May")

  ggplot(pC.plot.3, aes(y = mean.r, x = log.seed)) +
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
  geom_point(aes(colour = valley,shape = valley, fill = control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
  #   geom_line(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt, group = treat),size = 0.75, alpha = 0.7) +
  # geom_point(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt,shape = valley, fill = control), size = 5) +

# facet_wrap(~month, scales = "free")+
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod"))


## ------------------------------------------------------------------------
# pc.seed.full <-
pC.plot.aug <- out.full.136 %>%
  filter(month == "Aug")

  ggplot(pC.plot.aug, aes(y = mean.r, x = log.seed)) +
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
  geom_point(aes(colour = valley,shape = valley, fill = control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
  #   geom_line(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt, group = treat),size = 0.75, alpha = 0.7) +
  # geom_point(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt,shape = valley, fill = control), size = 5) +

# facet_wrap(~month, scales = "free")+
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod"))


## ------------------------------------------------------------------------
# pc.seed.full <-
pC.plot.nov <- out.full.136 %>%
  filter(month == "Nov")

ggplot(pC.plot.nov, aes(y = mean.r, x = log.seed)) +
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
  geom_point(aes(colour = valley,shape = valley, fill = control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
  #   geom_line(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt, group = treat),size = 0.75, alpha = 0.7) +
  # geom_point(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt,shape = valley, fill = control), size = 5) +

# facet_wrap(~month, scales = "free")+
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod"))


## ------------------------------------------------------------------------
# creates the reduced parameter dataset for seeds

# 24 long (x2) 12 replicates
para.plot.dat1 <- para.plot.dat %>%
      mutate(Control = control, Valley = valley)

out.para1 <- para.plot.dat1 %>%
  select(mean.b, Control, Valley, para, month) %>%
  spread(key = para, value = mean.b)

# for each group I need to ....
#feb filtered (data)
out.dat1 <- out.full.136 %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat2 <-  out.full.136 %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  group_by(Control, Valley, month) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.rat.mna, na.rm = TRUE),

    MAX.rat = max(lag.rat.mna, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = min(lag.N, na.rm = TRUE),
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
seed.plots <- left_join(out.dat2, out.para1, by = c("Control", "Valley", "month"))

# predict estimates for seed lines
# seed.plot1 <- seed.plots %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
seed.plot1 <- seed.plots %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * min.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.max = Intercept + (Seed * MAX.seed) + (Density*M.dens) + (Rats*M.rat))

# # r2
# # plot(pred~mean.r, data = seed.feb.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.seed2 <- seed.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.sjt = ifelse(pred.fact == "pred.min", min.seed, MAX.seed)) %>%
  select(Valley, Control, mean.r, lag.sjt, month, pred.fact) %>%
  mutate(lag.sjt = ifelse(lag.sjt>0, lag.sjt, 0.1),
         treat = paste(Valley,Control))

# # dens-feb ----------------------------------------------------------------
# #spread
# dens.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.feb2 <- dens.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-feb -----------------------------------------------------------------
# #spread and predict
# rat.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.feb2 <- rat.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
# data ----------------------
### Seed lines (feb)

pC.plot.3 <- out.full.136 %>%
  mutate(Control = control,
         Valley = valley)

out.dat1 <- out.full.136 %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

# pc.plot.feb <- 
  
ggplot(out.dat1, aes(y = mean.r, x = lag.sjt)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = valley,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.seed2, aes(y = mean.r, x = lag.sjt, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.seed2, aes(y = mean.r, x = lag.sjt, shape = Valley, fill = Control, group = treat), size = 5) +

facet_grid(Control~month, scales = "free") +
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  #
  # # scale_colour_manual(name = "Stoat control",
  # #                     labels = c("E-", "H+", "H-"),
  # #                     values = c("black","black", "black")) +
  # # scale_shape_manual(name = "Valley",
  # #                    labels = c("E", "H"),
  # #                    values = c(25,21)) +
  # # scale_fill_manual(name = "Stoat control",
  # #                   labels = c("E-", "H+", "H-"),
  # #                   values = c("white","black", "white")) +


xlab(expression(paste("Intake"," ", "Rate"," ","(",italic(S[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))


## ------------------------------------------------------------------------
# data for lines ----------------------------------------------------------
# Feb - seed
#feb filtered (parameters)
# flip dataset
out.para.feb <- para.plot.dat %>%
  filter(month == "Aug") %>%
      mutate(Control = control, Valley = valley) %>%
  select(mean.b, Control, Valley, para) %>%
    droplevels() %>%
  spread(key = para, value = mean.b)

#this gives us 12 times lines to estimate from


# for each group I need to ....

#feb filtered (data)
out.dat1.feb <- out.full.136 %>%
  filter(month == "Aug") %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat1.feb2 <-  out.full.136 %>%
  filter(month == "Aug") %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  group_by(Control, Valley) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.rat.mna, na.rm = TRUE),

    MAX.rat = max(lag.rat.mna, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = min(lag.N, na.rm = TRUE),
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
feb.plot <- left_join(out.dat1.feb2 ,out.para.feb)

# predict estimates for seed lines
# seed.feb.plot1 <- feb.plot %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
seed.feb.plot1 <- feb.plot %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * min.seed) + (Density*M.dens) + (Rats*M.dens),
         pred.max = Intercept + (Seed * MAX.seed) + (Density*M.dens) + (Rats*M.dens))

# # r2
# # plot(pred~mean.r, data = seed.feb.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.seed.feb2 <- seed.feb.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.sjt = ifelse(pred.fact == "pred.min", min.seed, MAX.seed)) %>%
  select(Valley, Control, mean.r, lag.sjt) %>%
  mutate(treat = paste(Valley, Control),
         lag.sjt = ifelse(lag.sjt>0, lag.sjt, 0))
# 
# # dens-feb ----------------------------------------------------------------
# #spread
# dens.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.feb2 <- dens.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-feb -----------------------------------------------------------------
# #spread and predict
# rat.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.feb2 <- rat.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
### Seed lines (aug)
  
pC.plot.3 <- out.full.136 %>%
  filter(month == "Aug") %>%
  mutate(Control = control,
         Valley = valley)



## ------------------------------------------------------------------------
pc.plot <-   ggplot(pC.plot.3, aes(y = mean.r, x = log.seed)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = Control, 
                 shape = valley, 
                 fill = Control),
             stroke = 1.001, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.seed.feb2, aes(y = mean.r, x = lag.sjt, shape = Valley, fill = Control, group = treat), size = 5) +

    scale_color_manual(name = "Stoat Control",
                     values = c("white", "black", "white")) +

  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  scale_size_manual(name = "Rat Control", values = c(2.5, 3, 2.5)) +
  scale_fill_manual(
    name = "Stoat Control",
    values = c("cornflowerblue", "darkorange", "cornflowerblue")
  ) +
     # Remove fill legend and replace the fill legend using the newly created size
  guides(
    col = "none",
    size = guide_legend(override.aes = list(
  shape = c(15,0),alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      shape = c(24, 21), size = 4
    )),
    fill = guide_legend(override.aes = list(
      col = c("cornflowerblue", "darkorange"),shape = c("square"),
      size = 4
    ))
  ) +
  
xlab(expression(paste("Intake"," ", "Rate"," ","(",italic(S[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
  
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))


## ------------------------------------------------------------------------
# data for lines ----------------------------------------------------------
# Feb - seed
#feb filtered (parameters)
# flip dataset
para.plot.dat1 <- para.plot.dat %>%
      mutate(Control = control, Valley = valley)

out.para1 <- para.plot.dat %>%
      mutate(Control = control, Valley = valley) %>%
  select(mean.b, Control, Valley, para, month) %>%
    droplevels() %>%
  spread(key = para, value = mean.b)

# for each group I need to ....

#feb filtered (data)
out.dat1 <- out.full.136 %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat2 <-  out.full.136 %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  group_by(Control, Valley, month) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.rat.mna, na.rm = TRUE),

    MAX.rat = max(lag.rat.mna, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = min(lag.N, na.rm = TRUE),
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
dens.plots <- left_join(out.dat2, out.para1, by = c("Control", "Valley", "month"))

# predict estimates for seed lines
# seed.plot1 <- seed.plots %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
dens.plot1 <- dens.plots %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * M.seed) + (Density*min.dens) + (Rats*M.rat),
         pred.max = Intercept + (Seed * M.seed) + (Density*MAX.dens) + (Rats*M.rat))

# # r2
# # plot(pred~mean.r, data = seed.feb.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.dens2 <- dens.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.N = ifelse(pred.fact == "pred.min", min.dens, MAX.dens)) %>%
  select(Valley, Control, mean.r, lag.N, month, pred.fact) %>%
  mutate(lag.N = ifelse(lag.N>0, lag.N, 0.1),
         treat = paste(Valley,Control))


# 
# # dens-feb ----------------------------------------------------------------
# #spread
# dens.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.feb2 <- dens.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-feb -----------------------------------------------------------------
# #spread and predict
# rat.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.feb2 <- rat.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
# data ----------------------
  
pC.plot.3 <- out.full.136 %>%
  mutate(Control = control,
         Valley = valley)

out.dat1 <- out.full.136 %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

ggplot(out.dat1, aes(y = mean.r, x = lag.N)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = valley,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.dens2, aes(y = mean.r, x = lag.N, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.dens2, aes(y = mean.r, x = lag.N, shape = Valley, fill = Control, group = treat), size = 5) +

facet_grid(Control~month, scales = "free") +
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  #
  # # scale_colour_manual(name = "Stoat control",
  # #                     labels = c("E-", "H+", "H-"),
  # #                     values = c("black","black", "black")) +
  # # scale_shape_manual(name = "Valley",
  # #                    labels = c("E", "H"),
  # #                    values = c(25,21)) +
  # # scale_fill_manual(name = "Stoat control",
  # #                   labels = c("E-", "H+", "H-"),
  # #                   values = c("white","black", "white")) +


xlab(expression(paste("Mouse"," ", "Density"," ","(",italic(N[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))



## ------------------------------------------------------------------------
# data for lines ----------------------------------------------------------
# Feb - seed
#feb filtered (parameters)
# flip dataset
out.para.feb <- para.plot.dat %>%
  filter(month == "Feb") %>%
      mutate(Control = control, Valley = valley) %>%
  select(mean.b, Control, Valley, para) %>%
    droplevels() %>%
  spread(key = para, value = mean.b)

#this gives us 12 times lines to estimate from


# for each group I need to ....

#feb filtered (data)
out.dat1.feb <- out.full.136 %>%
  filter(month == "Feb") %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat1.feb2 <-  out.full.136 %>%
  filter(month == "Feb") %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  group_by(Control, Valley) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.rat.mna, na.rm = TRUE),

    MAX.rat = max(lag.rat.mna, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = 0,
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
feb.plot <- left_join(out.dat1.feb2 ,out.para.feb)

# predict estimates for seed lines
# seed.feb.plot1 <- feb.plot %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
feb.plot1 <- feb.plot %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * M.seed) + (Density*min.dens) + (Rats*M.dens),
         pred.max = Intercept + (Seed * M.seed) + (Density*MAX.dens) + (Rats*M.dens))

# # r2
# # plot(pred~mean.r, data = seed.feb.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.dens.feb2 <- feb.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.N = ifelse(pred.fact == "pred.min", min.dens, MAX.dens)) %>%
  select(Valley, Control, mean.r, lag.N) %>%
  mutate(treat = paste(Valley, Control),
         lag.N = ifelse(lag.N>0, lag.N, 0.1))
# 
# # dens-feb ----------------------------------------------------------------
# #spread
# dens.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.feb2 <- dens.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-feb -----------------------------------------------------------------
# #spread and predict
# rat.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.feb2 <- rat.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
# data ----------------------
### Seed lines (feb)
  
pD.plot.3 <- out.full.136 %>%
  filter(month == "Feb") %>%
  mutate(Control = control,
         Valley = valley,
         lag.sjt = lag(log.seed),
         lag.N = lag(N))

pd.plot.feb <- ggplot(pD.plot.3, aes(y = mean.r, x = lag.N)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = valley,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.dens.feb2, aes(y = mean.r, x = lag.N, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.dens.feb2, aes(y = mean.r, x = lag.N, shape = Valley, fill = Control, group = treat), size = 5) +

   scale_color_manual(name = "Stoat Control",
                     values = c("white", "black", "white")) +

  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  scale_size_manual(name = "Rat Control", values = c(2.5, 3, 2.5)) +
  scale_fill_manual(
    name = "Stoat Control",
    values = c("cornflowerblue", "darkorange", "cornflowerblue")
  ) +
     # Remove fill legend and replace the fill legend using the newly created size
  guides(
    col = "none",
    size = guide_legend(override.aes = list(
  shape = c(15,0),alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      shape = c(24, 21), size = 4
    )),
    fill = guide_legend(override.aes = list(
      col = c("cornflowerblue", "darkorange"),shape = c("square"),
      size = 4
    ))
  ) +
  
  #
  # # scale_colour_manual(name = "Stoat control",
  # #                     labels = c("E-", "H+", "H-"),
  # #                     values = c("black","black", "black")) +
  # # scale_shape_manual(name = "Valley",
  # #                    labels = c("E", "H"),
  # #                    values = c(25,21)) +
  # # scale_fill_manual(name = "Stoat control",
  # #                   labels = c("E-", "H+", "H-"),
  # #                   values = c("white","black", "white")) +


# xlab(expression(paste("Intake"," ", "Rate"," ","(",italic(S[jt]),")" ))) +
xlab(expression(paste("Mouse"," ", "Density"," ","(",italic(N[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))
pd.plot.feb


## ------------------------------------------------------------------------
# data for lines ----------------------------------------------------------
# May - seed
#May filtered (parameters)
# flip dataset
out.para.May <- para.plot.dat %>%
  filter(month == "May") %>%
      mutate(Control = control, Valley = valley) %>%
  select(mean.b, Control, Valley, para) %>%
    droplevels() %>%
  spread(key = para, value = mean.b)

#this gives us 12 times lines to estimate from


# for each group I need to ....

#May filtered (data)
out.dat1.May <- out.full.136 %>%
  filter(month == "May") %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat1.May2 <-  out.full.136 %>%
  filter(month == "May") %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  group_by(Control, Valley) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.rat.mna, na.rm = TRUE),

    MAX.rat = max(lag.rat.mna, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = 0,
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
May.plot <- left_join(out.dat1.May2 ,out.para.May)

# predict estimates for seed lines
# seed.May.plot1 <- May.plot %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
May.plot1 <- May.plot %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * M.seed) + (Density*min.dens) + (Rats*M.dens),
         pred.max = Intercept + (Seed * M.seed) + (Density*MAX.dens) + (Rats*M.dens))

# # r2
# # plot(pred~mean.r, data = seed.May.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.dens.May2 <- May.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.N = ifelse(pred.fact == "pred.min", min.dens, MAX.dens)) %>%
  select(Valley, Control, mean.r, lag.N) %>%
  mutate(treat = paste(Valley, Control),
         lag.N = ifelse(lag.N>0, lag.N, 0.1))
# 
# # dens-May ----------------------------------------------------------------
# #spread
# dens.May.plot1 <- May.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.May2 <- dens.May.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-May -----------------------------------------------------------------
# #spread and predict
# rat.May.plot1 <- May.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.May2 <- rat.May.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
# data ----------------------
### Seed lines (May)
  
pD.plot.3 <- out.full.136 %>%
  filter(month == "May") %>%
  mutate(Control = control,
         Valley = valley,
         lag.sjt = lag(log.seed),
         lag.N = lag(N))

# pc.plot.May <- 
  
  ggplot(pD.plot.3, aes(y = mean.r, x = lag.N)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = valley,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.May2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.dens.May2, aes(y = mean.r, x = lag.N, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.dens.May2, aes(y = mean.r, x = lag.N, shape = Valley, fill = Control, group = treat), size = 5) +

# facet_wrap(~month, scales = "free")+
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  #
  # # scale_colour_manual(name = "Stoat control",
  # #                     labels = c("E-", "H+", "H-"),
  # #                     values = c("black","black", "black")) +
  # # scale_shape_manual(name = "Valley",
  # #                    labels = c("E", "H"),
  # #                    values = c(25,21)) +
  # # scale_fill_manual(name = "Stoat control",
  # #                   labels = c("E-", "H+", "H-"),
  # #                   values = c("white","black", "white")) +


# xlab(expression(paste("Intake"," ", "Rate"," ","(",italic(S[jt]),")" ))) +
xlab(expression(paste("Mouse"," ", "Density"," ","(",italic(N[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))


## ------------------------------------------------------------------------
# data for lines ----------------------------------------------------------
# Aug - density
#Aug filtered (parameters)
# flip dataset
out.para.Aug <- para.plot.dat %>%
  filter(month == "Aug") %>%
      mutate(Control = control, Valley = valley) %>%
  select(mean.b, Control, Valley, para) %>%
    droplevels() %>%
  spread(key = para, value = mean.b)

#this gives us 12 times lines to estimate from


# for each group I need to ....

#Aug filtered (data)
out.dat1.Aug <- out.full.136 %>%
  filter(month == "Aug") %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat1.Aug2 <-  out.full.136 %>%
  filter(month == "Aug") %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  group_by(Control, Valley) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.rat.mna, na.rm = TRUE),

    MAX.rat = max(lag.rat.mna, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = 0,
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
Aug.plot <- left_join(out.dat1.Aug2 ,out.para.Aug)

# predict estimates for seed lines
# seed.Aug.plot1 <- Aug.plot %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
Aug.plot1 <- Aug.plot %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * M.seed) + (Density*min.dens) + (Rats*M.dens),
         pred.max = Intercept + (Seed * M.seed) + (Density*MAX.dens) + (Rats*M.dens))

# # r2
# # plot(pred~mean.r, data = seed.Aug.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.dens.Aug2 <- Aug.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.N = ifelse(pred.fact == "pred.min", min.dens, MAX.dens)) %>%
  select(Valley, Control, mean.r, lag.N) %>%
  mutate(treat = paste(Valley, Control),
         lag.N = ifelse(lag.N>0, lag.N, 0.1))
# 
# # dens-Aug ----------------------------------------------------------------
# #spread
# dens.Aug.plot1 <- Aug.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.Aug2 <- dens.Aug.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-Aug -----------------------------------------------------------------
# #spread and predict
# rat.Aug.plot1 <- Aug.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.Aug2 <- rat.Aug.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
# data ----------------------
### Seed lines (Aug)
  
pD.plot.3 <- out.full.136 %>%
  filter(month == "Aug") %>%
  mutate(Control = control,
         Valley = valley,
         lag.sjt = lag(log.seed),
         lag.N = lag(N))

# pc.plot.Aug <- 
  
  ggplot(pD.plot.3, aes(y = mean.r, x = lag.N)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = valley,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.Aug2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.dens.Aug2, aes(y = mean.r, x = lag.N, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.dens.Aug2, aes(y = mean.r, x = lag.N, shape = Valley, fill = Control, group = treat), size = 5) +

# facet_wrap(~month, scales = "free")+
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  #
  # # scale_colour_manual(name = "Stoat control",
  # #                     labels = c("E-", "H+", "H-"),
  # #                     values = c("black","black", "black")) +
  # # scale_shape_manual(name = "Valley",
  # #                    labels = c("E", "H"),
  # #                    values = c(25,21)) +
  # # scale_fill_manual(name = "Stoat control",
  # #                   labels = c("E-", "H+", "H-"),
  # #                   values = c("white","black", "white")) +


# xlab(expression(paste("Intake"," ", "Rate"," ","(",italic(S[jt]),")" ))) +
xlab(expression(paste("Mouse"," ", "Density"," ","(",italic(N[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))


## ------------------------------------------------------------------------
# data for lines ----------------------------------------------------------
# Nov - seed
#Nov filtered (parameters)
# flip dataset
out.para.Nov <- para.plot.dat %>%
  filter(month == "Nov") %>%
      mutate(Control = control, Valley = valley) %>%
  select(mean.b, Control, Valley, para) %>%
    droplevels() %>%
  spread(key = para, value = mean.b)

#this gives us 12 times lines to estimate from


# for each group I need to ....

#Nov filtered (data)
out.dat1.Nov <- out.full.136 %>%
  filter(month == "Nov") %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat1.Nov2 <-  out.full.136 %>%
  filter(month == "Nov") %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N)) %>%
  group_by(Control, Valley) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.rat.mna, na.rm = TRUE),

    MAX.rat = max(lag.rat.mna, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = 0,
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
Nov.plot <- left_join(out.dat1.Nov2 ,out.para.Nov)

# predict estimates for seed lines
# seed.Nov.plot1 <- Nov.plot %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
Nov.plot1 <- Nov.plot %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * M.seed) + (Density*min.dens) + (Rats*M.dens),
         pred.max = Intercept + (Seed * M.seed) + (Density*MAX.dens) + (Rats*M.dens))

# # r2
# # plot(pred~mean.r, data = seed.Nov.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.dens.Nov2 <- Nov.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.N = ifelse(pred.fact == "pred.min", min.dens, MAX.dens)) %>%
  select(Valley, Control, mean.r, lag.N) %>%
  mutate(treat = paste(Valley, Control),
         lag.N = ifelse(lag.N>0, lag.N, 0.1))
# 
# # dens-Nov ----------------------------------------------------------------
# #spread
# dens.Nov.plot1 <- Nov.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.Nov2 <- dens.Nov.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-Nov -----------------------------------------------------------------
# #spread and predict
# rat.Nov.plot1 <- Nov.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.Nov2 <- rat.Nov.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
# data ----------------------
### Seed lines (Nov)
  
pD.plot.3 <- out.full.136 %>%
  filter(month == "Nov") %>%
  mutate(Control = control,
         Valley = valley,
         lag.sjt = lag(log.seed),
         lag.N = lag(N))

# pc.plot.Nov <- 
  
  ggplot(pD.plot.3, aes(y = mean.r, x = lag.N)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = valley,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.Nov2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.dens.Nov2, aes(y = mean.r, x = lag.N, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.dens.Nov2, aes(y = mean.r, x = lag.N, shape = Valley, fill = Control, group = treat), size = 5) +

# facet_wrap(~month, scales = "free")+
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  #
  # # scale_colour_manual(name = "Stoat control",
  # #                     labels = c("E-", "H+", "H-"),
  # #                     values = c("black","black", "black")) +
  # # scale_shape_manual(name = "Valley",
  # #                    labels = c("E", "H"),
  # #                    values = c(25,21)) +
  # # scale_fill_manual(name = "Stoat control",
  # #                   labels = c("E-", "H+", "H-"),
  # #                   values = c("white","black", "white")) +


# xlab(expression(paste("Intake"," ", "Rate"," ","(",italic(S[jt]),")" ))) +
xlab(expression(paste("Mouse"," ", "Density"," ","(",italic(N[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))


## ------------------------------------------------------------------------
# data for lines ----------------------------------------------------------
# Feb - rats
#feb filtered (parameters)
# flip dataset
para.plot.dat1 <- para.plot.dat %>%
      mutate(Control = control, Valley = valley)

out.para1 <- para.plot.dat %>%
      mutate(Control = control, Valley = valley) %>%
  select(mean.b, Control, Valley, para, month) %>%
    droplevels() %>%
  spread(key = para, value = mean.b)

# for each group I need to ....

#feb filtered (data)
out.dat1 <- out.full.136 %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N),
         lag.R = lag.rat.mna) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat2 <-  out.full.136 %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N),
         lag.R = lag.rat.mna) %>%
  group_by(Control, Valley, month) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.R, na.rm = TRUE),

    MAX.rat = max(lag.R, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = min(lag.N, na.rm = TRUE),
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
dens.plots <- left_join(out.dat2, out.para1, by = c("Control", "Valley", "month"))

# predict estimates for seed lines
# seed.plot1 <- seed.plots %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
dens.plot1 <- dens.plots %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*min.rat),
         pred.max = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*MAX.rat))

# # r2
# # plot(pred~mean.r, data = seed.feb.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.dens2 <- dens.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.R = ifelse(pred.fact == "pred.min", min.rat, MAX.rat)) %>%
  select(Valley, Control, mean.r, lag.R, month, pred.fact) %>%
  mutate(lag.R = ifelse(lag.R>0, lag.R, 0.1),
         treat = paste(Valley,Control))


# 
# # dens-feb ----------------------------------------------------------------
# #spread
# dens.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.feb2 <- dens.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-feb -----------------------------------------------------------------
# #spread and predict
# rat.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.feb2 <- rat.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
# data ----------------------
  
pC.plot.3 <- out.full.136 %>%
  mutate(Control = control,
         Valley = valley)

out.dat1 <- out.full.136 %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N),
         lag.R = lag.rat.mna) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

ggplot(out.dat1, aes(y = mean.r, x = lag.R)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = valley,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.dens2, aes(y = mean.r, x = lag.R, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.dens2, aes(y = mean.r, x = lag.R, shape = Valley, fill = Control, group = treat), size = 5) +

facet_grid(Control~month, scales = "free") +
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  #
  # # scale_colour_manual(name = "Stoat control",
  # #                     labels = c("E-", "H+", "H-"),
  # #                     values = c("black","black", "black")) +
  # # scale_shape_manual(name = "Valley",
  # #                    labels = c("E", "H"),
  # #                    values = c(25,21)) +
  # # scale_fill_manual(name = "Stoat control",
  # #                   labels = c("E-", "H+", "H-"),
  # #                   values = c("white","black", "white")) +


# xlab(expression(paste("Mouse"," ", "Density"," ","(",italic(N[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))



## ------------------------------------------------------------------------
# data for lines ----------------------------------------------------------
# Feb - rats
#feb filtered (parameters)
# flip dataset
para.plot.dat1 <- para.plot.dat %>%
  filter(month == "Feb") %>%
      mutate(Control = control, Valley = valley)

out.para1 <- para.plot.dat %>%
  filter(month == "Feb") %>%
      mutate(Control = control, Valley = valley) %>%
  select(mean.b, Control, Valley, para, month) %>%
    droplevels() %>%
  spread(key = para, value = mean.b)

# for each group I need to ....

#feb filtered (data)
out.dat1 <- out.full.136 %>%
  filter(month == "Feb") %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N),
         lag.R = lag.rat.mna) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

out.dat2 <-  out.full.136 %>%
  filter(month == "Feb") %>%
      mutate(Control = control, Valley = as.character(valley),
             lag.sjt = lag(log.seed),
         lag.N = lag(N),
         lag.R = lag.rat.mna) %>%
  group_by(Control, Valley, month) %>%
    summarise(
    M.seed = mean(lag.sjt, na.rm = TRUE),
    M.dens = mean(lag.N, na.rm = TRUE),
    M.rat = mean(lag.R, na.rm = TRUE),

    MAX.rat = max(lag.R, na.rm = TRUE),
    MAX.seed = max(lag.sjt, na.rm = TRUE),
    MAX.dens = max(lag.N, na.rm = TRUE),
    MAX.r = max(mean.r, na.rm = TRUE),

    min.seed = 0,
    min.dens = min(lag.N, na.rm = TRUE),
    min.rat = 0,
    min.r = min(mean.r, na.rm = TRUE)) %>%
  ungroup()

# #merge two datasets
dens.plots <- left_join(out.dat2, out.para1, by = c("Control", "Valley", "month"))

# predict estimates for seed lines
# seed.plot1 <- seed.plots %>%
#   mutate(pred.mean = b0 + (b.seed * M.seed) + (b.dens*M.dens) + (b.rat*M.rat),
#          pred.min = b0 + (b.seed * min.seed) + (b.dens*min.dens) + (b.rat*min.rat),
#          pred.max = b0 + (b.seed * MAX.seed) + (b.dens*MAX.dens) + (b.rat*MAX.rat))
dens.plot1 <- dens.plots %>%
  mutate(pred.mean = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*M.rat),
         pred.min = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*min.rat),
         pred.max = Intercept + (Seed * M.seed) + (Density*M.dens) + (Rats*MAX.rat))

# # r2
# # plot(pred~mean.r, data = seed.feb.plot1)
# 
# 
# # data in tidyverse for ggplot2
out.dens2 <- dens.plot1 %>%
  gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
  mutate(lag.R = ifelse(pred.fact == "pred.min", min.rat, MAX.rat)) %>%
  select(Valley, Control, mean.r, lag.R, month, pred.fact) %>%
  mutate(lag.R = ifelse(lag.R>0, lag.R, 0.1),
         treat = paste(Valley,Control))


# 
# # dens-feb ----------------------------------------------------------------
# #spread
# dens.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.mice.min),
#          pred.max = (b0 + b.dens * dat.mice.max))
# 
# #tidyverse
# out.dens.feb2 <- dens.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.mice.min, dat.mice.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))
# 
# 
# # rat-feb -----------------------------------------------------------------
# #spread and predict
# rat.feb.plot1 <- feb.plot %>%
#   mutate(pred.min = b0 + (b.dens * dat.rat.min),
#          pred.max = (b0 + b.dens * dat.rat.max))
# 
# #tidyverse
# out.rat.feb2 <- rat.feb.plot1 %>%
#   gather(key = pred.fact, value = mean.r, pred.min:pred.max) %>%
#   mutate(lag.sjt = ifelse(pred.fact == "pred.min", dat.rat.min, dat.rat.max)) %>%
#   select(valley, control, mean.r, lag.sjt) %>%
#   mutate(treat = paste(valley, control))



## ------------------------------------------------------------------------
# data ----------------------
  
pC.plot.3 <- out.full.136 %>%
  filter(month == "Feb") %>%
  mutate(Control = control,
         Valley = valley)

out.dat1 <- out.full.136 %>%
  filter(month == "Feb") %>%
  mutate(lag.sjt = lag(log.seed),
         lag.N = lag(N),
         lag.R = lag.rat.mna) %>%
  select(-var) %>%
    droplevels()%>%
      mutate(Control = control, Valley = valley)

ggplot(out.dat1, aes(y = mean.r, x = lag.R)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = valley,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +

  # ggplot(out.seed.feb2, aes(y = mean.r, x = lag.sjt)) +
    geom_line(data = out.dens2, aes(y = mean.r, x = lag.R, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.dens2, aes(y = mean.r, x = lag.R, shape = Valley, fill = Control, group = treat), size = 5) +

# facet_grid(Control~month, scales = "free") +
  scale_shape_manual(name = "Valley",
                         labels = c("E", "H"),
                         values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  #
  # # scale_colour_manual(name = "Stoat control",
  # #                     labels = c("E-", "H+", "H-"),
  # #                     values = c("black","black", "black")) +
  # # scale_shape_manual(name = "Valley",
  # #                    labels = c("E", "H"),
  # #                    values = c(25,21)) +
  # # scale_fill_manual(name = "Stoat control",
  # #                   labels = c("E-", "H+", "H-"),
  # #                   values = c("white","black", "white")) +


# xlab(expression(paste("Mouse"," ", "Density"," ","(",italic(N[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))



## ------------------------------------------------------------------------

paras.plot <- para.plot.erors %>%
                mutate(month = factor(month, levels = c("Feb", "May", "Aug", "Nov"), labels = c("February", "May", "August", "November")),
                       combo = paste(valley, Control))


ggplot(data = paras.plot, aes(y = mean.b, x = control, colour = valley,
                                                              shape = valley, 
                                                              fill = control, 
                                   group = paste(control, valley))) +
  geom_abline(intercept = 0,slope = 0, col = "red", size = 0.85) +
    geom_errorbar(aes(ymin = lcl.b, ymax = ucl.b, width = 0), lwd = 1.1, position = position_dodge(width = 0.25)) +
       geom_point(stroke = 1.5, size = 2, alpha = 0.8, width = 0.25, position = position_dodge(width = 0.25)) + 
  facet_grid(month~para, scales = "free")


# para.plot.all <- 
  ggplot(paras.plot, aes(y = mean.b, x = combo, col = Control, shape = valley, fill = Control)) +
  geom_errorbar(aes(ymin = lcl.b, ymax = ucl.b), lwd = 0.75, alpha = 1, width = 0.1, position =position_dodge(width = 1)) +
  
  geom_abline(intercept = 0, slope = 0) +
  geom_point(size =4, alpha = 0.7, position = position_dodge(width = 1)) +
  facet_wrap(month~para, scales = "free") +
  # facet_wrap(combo~month, scales = "free") +
  
   scale_color_manual(name = "Stoat Control",
                     values = c("white", "black", "white")) +

  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  scale_size_manual(name = "Rat Control", values = c(2.5, 3, 2.5)) +
  scale_fill_manual(name = "Stoat Control",
    values = c("cornflowerblue", "darkorange", "cornflowerblue")) +
     # Remove fill legend and replace the fill legend using the newly created size
  guides(
    col = "none",
    size = guide_legend(override.aes = list(
  shape = c(15,0),alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      shape = c(24, 21), size = 4
    )),
    fill = guide_legend(override.aes = list(
      col = c("cornflowerblue", "darkorange"),shape = c("square"),
      size = 4
    ))
  )



## ----include=FALSE-------------------------------------------------------
pc.plot

# Save plot
# export plot for example vignette
png("./figs/fig-5-1.png")
pc.plot
dev.off()


## ---- out.width="100%", out.height="50%",,fig.align='center'-------------
knitr::include_graphics(c("./figs/fig-5-1.png"))


## ----include=FALSE-------------------------------------------------------
pd.plot.feb

# Save plot
# export plot for example vignette
png("./figs/fig-6-1.png")
pd.plot.feb
dev.off()


## ---- out.width="100%", out.height="50%",,fig.align='center'-------------
knitr::include_graphics(c("./figs/fig-6-1.png"))


## ------------------------------------------------------------------------

para.rats <- para.plot.erors %>%
              filter(para == "b.rat") %>%
                mutate(month = factor(month, levels = c("Feb", "May", "Aug", "Nov"), labels = c("February", "May", "August", "November")))



para.plot.rat <- ggplot(para.rats, aes(y = mean.b, x = month, col = Control, shape = valley, fill = Control)) +
  geom_errorbar(aes(ymin = lcl.b, ymax = ucl.b), lwd = 0.75, alpha = 1, width = 0.1, position =position_dodge(width = 1)) +
  
  geom_abline(intercept = 0, slope = 0) +
  geom_point(size =4, alpha = 0.7, position = position_dodge(width = 1)) +
  # facet_wrap(month~para, scales = "free") +
  facet_wrap(~month, scales = "free") +
  
   scale_color_manual(name = "Stoat Control",
                     values = c("white", "black", "white")) +

  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  scale_size_manual(name = "Rat Control", values = c(2.5, 3, 2.5)) +
  scale_fill_manual(name = "Stoat Control",
    values = c("cornflowerblue", "darkorange", "cornflowerblue")) +
     # Remove fill legend and replace the fill legend using the newly created size
  guides(
    col = "none",
    size = guide_legend(override.aes = list(
  shape = c(15,0),alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      shape = c(24, 21), size = 4
    )),
    fill = guide_legend(override.aes = list(
      col = c("cornflowerblue", "darkorange"),shape = c("square"),
      size = 4
    ))
  ) +
  

# xlab(expression(paste("Intake"," ", "Rate"," ","(",italic(S[jt]),")" ))) +
xlab("Month") +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))
para.plot.rat



## ------------------------------------------------------------------------
rat.plot.all <- ggplot(out.dat1, aes(y = mean.r, x = lag.R)) +
    
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.2, position=position_dodge(width=30), width = 0) +
    
  geom_point(aes(colour = Control,shape = valley, fill = Control),
             stroke = 1.5, size = 2, alpha = 0.7) +
    geom_line(data = out.dens2, aes(y = mean.r, x = lag.R, group = treat),size = 0.75, alpha = 0.7) +
  geom_point(data = out.dens2, aes(y = mean.r, x = lag.R, shape = Valley, fill = Control, group = treat), size = 5) +
 scale_color_manual(name = "Stoat Control",
                     values = c("white", "black", "white")) +

  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  scale_size_manual(name = "Rat Control", values = c(2.5, 3, 2.5)) +
  scale_fill_manual(name = "Stoat Control",
    values = c("cornflowerblue", "darkorange", "cornflowerblue")) +
     # Remove fill legend and replace the fill legend using the newly created size
  guides(
    col = "none",
    size = guide_legend(override.aes = list(
  shape = c(15,0),alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      shape = c(24, 21), size = 4
    )),
    fill = guide_legend(override.aes = list(
      col = c("cornflowerblue", "darkorange"),shape = c("square"),
      size = 4
    ))
  ) +

# xlab(expression(paste("Mouse"," ", "Density"," ","(",italic(N[jt]),")" ))) +
# xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+

ylab(expression(atop(paste("Rate"," ", "of"," ",
                           "increase"),paste(" ", "of"," ",
                                             "mice"," ","(",italic(r[jt]),")"))) ) +
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
theme_tufte() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),

        plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
        plot.subtitle=element_text(size=16, face="italic", color="black"),

        legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill="white", size=1),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(colour = "black", size =16, family = "Times"),
        legend.title = element_text(colour = "black", size =16, family = "Times"),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
        axis.title.x = element_text(colour = "black", size =20, family = "Times"),
        axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
        axis.text.x = element_text(colour = "black", size =20, family = "Times"),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))

rat.plot.all


## ------------------------------------------------------------------------
# Save plot
# export plot for example vignette
png("./figs/fig-7-1.png")
para.plot.rat
dev.off()

# Save plot
# export plot for example vignette
png("./figs/fig-7-2.png")
rat.plot.all
dev.off()


## ----message=FALSE, warning=FALSE----------------------------------------
# export data
### ????

# # export plot for example vignette
# jpeg("./figs/fig-2.jpeg")
# fig.2.plot.design
# dev.off()


## ------------------------------------------------------------------------
# # Low abundance
# # export plot for example vignette
# png("./figs/fig-4-1.png")
# low.plot.time
# dev.off()
#
# #High abundance
# # export plot for example vignette
# png("./figs/fig-4-2.png")
# high.plot.time
# dev.off()



## ----eval=FALSE, include=FALSE-------------------------------------------
## # data-outputs ------------------------------------------------------------
## 
## #r-bind document
## # write.csv(out.final.2, "./Data/allprediction-output-data.csv")
## 
## # Rate of increase between Autumn and Winter
## # (May and August rate of change)
## # May mice abundace, seed and rat data
## # Overall Data
## 
## # data --------------------------------------------------------------------
## # reduced data
## # out.pC.1 <- filter(out.final.2, month == "Nov")
## 
## #main data from manuscript main file
## # but needs valley, control and Conditions
## 
##  ##dataset136 HAS CHANGED AND ISSUES WILL BE SOLVED WHEN THIS IS CORRECTED !!!!
## glimpse(out.full.136)
## # Think this is the last big hurdle
## 
## # UP TO HERE#
## 
## out.final.3 <- out.full.136 %>%
##   mutate(mean.r = as.numeric(N),
##     lag.sjt = as.numeric(lag.sjt),
##     lcl.r = as.numeric(lcl.r),
##     ucl.r = as.numeric(ucl.r))
## 
## 
## glimpse(out.final.3$month)
## # glimpse(out.final.3$controlT)
## # names(out.final.2)
## # levels(out.final.3$month)
## # out.final.3$month
## # levels(out.final.3$month)
## 
## # used to be
## # out.r <- read_csv("./Data/CR_output_pred.csv")
## 
## # lines summary -----------------------------------------------------------
## 
## pred.lines.seed <- out.final %>%
##   drop_na() %>%
##   # group_by(valley, control, Conditions, month) %>%
##   mutate(Control = control) %>%
##   group_by(Control, month) %>%
##   summarise(
##     b0 = mean(b0),
##     b.seed = mean(b.seed),
##     b.dens = mean(b.dens),
##     b.rat = mean(b.rat),
##     se.r = mean(se.r),
## 
##     M.seed = mean(lag.sjt),
##     M.dens = mean(lag.N),
##     M.rat = mean(lag.rat.mna),
## 
##     MAX.rat = max(lag.rat.mna),
##     MAX.seed = max(lag.sjt),
##     MAX.dens = max(lag.N),
##     MAX.r = max(mean.r),
## 
##     min.seed = min(lag.sjt),
##     min.dens = min(lag.N),
##     min.rat = min(lag.rat.mna),
##     min.r = min(mean.r),
## 
##     min.pt = b0 + (b.seed * min.seed),
##     max.pt = b0 + (b.seed * MAX.seed)
##   ) %>%
##   ungroup()
## 
## # %>%
## #   mutate(month = factor(as.character(month),
## #                         levels = c("Feb","May","Aug","Nov")))
## pred.lines.1 <- pred.lines.seed
## # table(pred.lines.s5$month)
## # str(pred.lines.s5$month)
## # str(out.final.3$month)
## # # lines data for seed -----------------------------------------------------
## # Seed lines (12x) --------------------------------------------------------
## 
## pred.lines.s2 <- pred.lines.seed %>%
##   select(month,controlT, min.r, MAX.r, min.seed, MAX.seed) %>%
##   droplevels() %>%
##   gather(value = mean.r, key = pt.lines, min.r:MAX.seed)
## 
## 
## 
## pred.lines.s2.1 <-  filter(pred.lines.s2, pt.lines == "MAX.seed" |
##            pt.lines == "min.seed") %>%
##   droplevels() %>%
##   mutate(log.cum.seed = mean.r) %>%
##   drop_na()
## 
## log.cum.seed <- pred.lines.s2.1$log.cum.seed
## # controlT <- pred.lines.s2.1$controlT
## 
## pred.lines.s3 <- cbind(pred.lines.s2[1:24, ], log.cum.seed)
## 
## pred.lines.s4 <- pred.lines.s3 %>%
##   mutate(pt.lines = factor(pt.lines)) %>%
##   drop_na()
## 
## pred.lines.s5 <- pred.lines.s4 %>%
##   mutate(lag.sjt = log.cum.seed,
##     valley = factor(rep(c("egl", "hol", "hol"), 2, each = 4)),
##     control = factor(rep(c("Yes", "Yes", "No"), 2, each = 4))) %>%
##   drop_na()
## 
## # glimpse(pred.lines.1)
## ####FUKED!!!!!!!!!!!!!###########
## # factor sort!
## month.refactor <- factor(as.numeric(pred.lines.s5$month))
## # ?recode_factor
## 
## pred.lines.s5$month <- recode(month.refactor, "1" = "May", "2" = "Nov", "3" = "Feb", "4" = "Aug")
## 
## 
## # lines data for density --------------------------------------------------
## pred.lines.dens <- out.final.3 %>%
##   drop_na() %>%
##   # group_by(valley, control, Conditions, month) %>%
##   group_by(controlT, month) %>%
##   summarise(
##     b0 = mean(b0),
##     b.seed = mean(b.seed),
##     b.dens = mean(b.dens),
##     b.rat = mean(b.rat),
##     se.r = mean(se.r),
## 
##     M.seed = mean(lag.sjt),
##     M.dens = mean(lag.N),
##     M.rat = mean(lag.rat.mna),
## 
##     MAX.rat = max(lag.rat.mna),
##     MAX.seed = max(lag.sjt),
##     MAX.dens = max(lag.N),
##     MAX.r = max(mean.r),
## 
##     min.seed = min(lag.sjt),
##     min.dens = min(lag.N),
##     min.rat = min(lag.rat.mna),
##     min.r = min(mean.r),
## 
##     min.pt = b0 + (b.dens * min.dens),
##     max.pt = b0 + (b.dens * MAX.dens)
##   ) %>%
##   ungroup()
## 
## pred.lines.d2 <- pred.lines.dens %>%
##   select(month,controlT, min.r, MAX.r, min.dens, MAX.dens) %>%
##   droplevels() %>%
##   gather(value = mean.r, key = pt.lines, min.r:MAX.dens)
## 
## 
## 
## pred.lines.d2.1 <-  filter(pred.lines.d2, pt.lines == "MAX.dens" |
##                              pt.lines == "min.dens") %>%
##   droplevels() %>%
##   mutate(lag.N = mean.r) %>%
##   drop_na()
## 
## lag.N <- pred.lines.d2.1$lag.N
## # controlT <- pred.lines.s2.1$controlT
## 
## pred.lines.d3 <- cbind(pred.lines.d2[1:24, ], lag.N)
## 
## pred.lines.d4 <- pred.lines.d3 %>%
##   mutate(pt.lines = factor(pt.lines)) %>%
##   drop_na()
## 
## pred.lines.d5 <- pred.lines.d4 %>%
##   mutate(lag.N = lag.N,
##          valley = factor(rep(c("egl", "hol", "hol"), 2, each = 4)),
##          control = factor(rep(c("Yes", "Yes", "No"), 2, each = 4))) %>%
##   drop_na()

