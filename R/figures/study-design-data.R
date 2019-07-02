# study design plot
# march 2019
#anthony

# need the manuscript code to run before this
# data
# dataset 1
# summary by treatment
# plot.dat.sum <- abund.dat5 %>%
#     mutate(counter = 1) %>%
#       group_by(Conditions, valley, control) %>%
#         summarise(reps = sum(counter), 
#                   max.date = max(true.date),
#                   min.date = min(true.date))
# 
# plot.dat.sum1 <- plot.dat %>%
#           gather(key = limit, 
#                  value = true.date, 
#                  max.date:min.date) %>%
#   mutate(limit = factor(ifelse(limit == "max.date", "max", "min")),
#          treat.six = factor(paste(valley, control, Conditions))) %>%
#       ungroup()
# 
# #get all levels right
# levels(plot.dat.sum1$treat.six) <- c("hol no control rats.removed",
#                                  "hol control rats.present", 
#                                  "hol control rats.removed",
#                                  "egl control rats.present",
#                                  "egl control rats.removed" ,    
#                                  "hol no control rats.present")
# 
# names(plot.dat.sum1)
# 
# full replicates dataset (unique trips and grids)
plot.dat.all <- abund.dat5 %>%
  distinct(trip, grid, .keep_all = TRUE) 

# %>%
    # select(Conditions, valley, control, true.date,treat.six, grid, N, N.seed,cum.seed)

#get all levels right

levels(plot.dat.all$treat.six) <- c("hol no control rats.removed",
                                     "hol control rats.present",
                                     "hol control rats.removed",
                                     "egl control rats.present",
                                     "egl control rats.removed" ,
                                     "hol no control rats.present")


# saving data -------------------------------------------------------------
# csv the fuker!
# write.csv(plot.dat.all, "study_design_plot.csv")
# plot.study <- read.csv("study_design_plot.csv"

# plot labels
points.dat <- tibble(
  prediction = as.character(c("A", "B", "A")),
  valley = factor(c("egl", "hol", "hol")),
  true.date = as.Date(c("2000-08-01","2001-08-01","2003-08-01")))

# getting factor levels correct for once

plot.dat.all1 <-  plot.dat.all %>%
    mutate(Rats = factor(Conditions, labels = c("Full", "Reduced")),
           Control = factor(control, labels = c("Yes", "No")),
           Valley = factor(valley, labels = c("Eglinton", "Hollyford")),
           Date = as.Date(true.date), 
           Treatments = factor(treat.six, levels = c("hol no control rats.removed",
                                                             "hol control rats.present", 
                                                             "hol control rats.removed",
                                                             "egl control rats.present",
                                                             "egl control rats.removed" ,    
                                                             "hol no control rats.present")),
           Prediction = NA) %>%
      mutate(Prediction = ifelse(Date == "2000-08-01", "A", Prediction),
             Prediction = ifelse(Date == "2001-08-01" | 
                                   Date == "2003-08-01" | 
                                   Date == "2003-08-01" | 
                                   Date == "2003-08-01" , "B", Prediction),
             Prediction = ifelse(Date == "2001-08-01" | 
                                   Date == "2003-08-01" | 
                                   Date == "2003-08-01" | 
                                   Date == "2003-08-01" , "B", Prediction),
             Prediction = ifelse(Date == "2001-08-01" | 
                                   Date == "2003-08-01" | 
                                   Date == "2003-08-01" | 
                                   Date == "2003-08-01" , "B", Prediction),
             Prediction = ifelse(Date == "2001-08-01" | 
                                   Date == "2003-08-01" | 
                                   Date == "2003-08-01" | 
                                   Date == "2003-08-01" , "B", Prediction))

# overall larger points
plot.dat.sum <- plot.dat.all1 %>%
    mutate(counter = 1) %>%
    group_by(Rats, Valley, Control) %>%
    summarise(reps = sum(counter), 
              max.date = max(Date),
              min.date = min(Date))
  
plot.dat.sum1 <- plot.dat.sum %>%
    gather(key = limit, 
           value = Date, 
           max.date:min.date) %>%
    mutate(limit = factor(ifelse(limit == "max.date", "max", "min")),
           treat = paste(Valley,Control, Rats)) %>%
    ungroup()

# table(plot.dat.sum1$Control)
# plot labels
#find in overall plot.dat by filtering on A?
#april 2019

labels.dat <- filter(plot.dat.all1, Prediction == "A" | Prediction == "B") 

# %>% 
#   distinct(Prediction, .keep_all = TRUE) %>%
#     mutate(Valley = c("Hollyford", "Hollyford"))






# glimpse(plot.dat.all1$Rats)

# table(plot.dat.all1$Rats,plot.dat.all1$Control, plot.dat.all1$Valley)

p.design <- plot.dat.all1 %>%
  mutate(grid = as.numeric(factor(grid)))

# glimpse(plot.dat.all1)

# export study design data
write.csv(plot.dat.all1, "C://Code/data/study-design-plot-input-data.csv")
