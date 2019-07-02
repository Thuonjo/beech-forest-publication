# a cleaner way to deal with code
# source("./R_publication/figure_R/figure_one_data.R")
# data
sim.dat <- read_csv("C://Code/data/simulated_data.csv")
# str(sim.dat)

#treatment
control <- as.factor(c(rep(c("no.stoats"),4),rep(c("stoats"),4)))

#labels
labels1 <-  c("Summer", "Autumn", "Winter", "Spring", "Summer", "Autumn", "Winter", "Spring")

labels2 <-  c("", "", "Non-mast year", "", "", "", "Mast year", "")

#date
date <- as.Date(as.character(c("1999-02-01","1999-05-01","1999-08-01","1999-11-01","2000-02-01","2000-05-01","2000-08-01","2000-11-01", "2001-02-01","2001-05-01","2001-08-01","2001-11-01")))

# date1 <- as.Date(as.character(c(NA,"","1999-08-01","","","","","", "","","","")))

#plot 
pseed <- ggplot(sim.dat, aes(y = beech.seed, x = date)) +
  geom_rect(aes(xmin=ymd("2000-12-31"),xmax = ymd('1999-12-31'), ymin = -Inf, ymax = Inf), fill = "grey90")+
  geom_line(aes(y = beech.seed, x = date), size = 1.1) +
  geom_point(aes(y = beech.seed, x = date, fill = control), fill = "black",stroke = 1.5, shape = "square", size = 6) +
  xlab("") +
  ylab(expression(paste("Seed ", "(" ,italic(S[jt]),")"))) +
  
  scale_y_continuous(expand = c(0.05, 0.05)) +
  scale_x_date(breaks = date, date_labels =  labels1, expand = c(0.05, 0.05)) +
  # scale_fill_manual(value ss = c("black")) +
  
  annotate("text", x=as.Date('2000-05-15'), y = 2000,
           label = "Masting year", size = 4, colour = "black", family = "Times") +
  annotate("text", x=as.Date('1999-07-30'), y = 2000,
           label = "Non-mast year", size = 4, colour = "black", family = "Times") +
  annotate("text", x=as.Date('2001-03-30'), y = 2000,
           label = "Non-mast", size = 4, colour = "black", family = "Times") +
  
  theme_new() +
  theme(
    
    # strip.background = element_blank(),
    #     strip.text.y = element_blank(),
    #     plot.title = element_text(hjust = 0, size=14, family = "Times", color="black"),

        plot.margin = unit(c(0.1,0.1,0.1,0.1), units = "cm"),

        # plot.margin=margin(t=0.5, r = 0, l = 0, -0.05, unit="cm"),
        # legend.position = "none",
        # legend.key = element_blank(),
        # legend.background = element_rect(fill="white", size=1),
        # legend.key.size=unit(1,"cm"),
        # legend.text = element_text(colour = "black", size =12, family = "Times"),
        # legend.title = element_text(colour = "black", size =12, family = "Times"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),
        # axis.title.y = element_text(colour = "black",size =12, family = "Times", angle = 90),
        axis.text.y=element_text(colour = "black",size = 14, family = "Times"),

        axis.title.x = element_blank(),
        axis.text.x = element_blank(),

        axis.ticks.x = element_line(size = 1),
        axis.line.x = element_line(size = 1),

        axis.ticks.y = element_line(size = 1),

        axis.line.y = element_line(size = 1))


# pseed


# build points data
# tibble my life
arrow.length <- 10
touchoff.distance <- 10 # distance between data and start of arrow
arrowhead.size <- 3 # in millimeters
time.loc <- as.character()

# "1999-09-31", "2000-05-31", "2000-07-31", "2000-12-31"

points.dat <- tibble(
  prediction = as.factor(c("A", "C", "B", "D")),
  value = as.numeric(c(15, 88, 108, 60)),
  date = as.Date(c("1999-08-20", "2000-05-01", "2000-09-13", "2000-12-25")))

# c("1999-02-01","1999-05-01","1999-08-01","1999-11-01","2000-02-01","2000-05-01","2000-08-01","2000-11-01", "2001-02-01","2001-05-01","2001-08-01","2001-11-01")

pmice <- ggplot(sim.dat, aes(y = value, x = date)) +
  geom_rect(aes(xmin=ymd("2000-12-31"),xmax = ymd('1999-12-31'), ymin = -Inf, ymax = Inf), fill = "grey90") +
  
  geom_line(aes(group = control, col = control), size = 1.1) +
  geom_point(aes(fill = control, colour = control), stroke = 1.5, shape = "square", size = 4.5) +
  
  xlab("Time (t)") + 
  ylab(expression(paste("Mice"," ", "(" ,italic(N[jt]),")"))) +
  
  scale_y_continuous(expand = c(0.05, 0.05)) +
  scale_x_date(breaks = date, date_labels =  labels1, expand = c(0.05, 0.05)) +
  
  # scale_fill_manual(values = c("white", "black")) +
  
  scale_colour_manual(name = "Stoat control",
                      labels = c("Yes", "No"),
                      values = c("cornflowerblue","darkorange")) +

  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No"),
                    values = c("cornflowerblue","darkorange")) +
  
  
  geom_segment(data = points.dat, aes(x = date, y = value,
                               xend = date, yend = value + touchoff.distance + arrow.length),
                 arrow = arrow(length = unit(arrowhead.size, "mm"), ends = "first"), size = 1.25, colour = "red") +
  
  geom_label(data = points.dat, aes(x = date, y = value, label = prediction),
             nudge_x = 10,
             nudge_y = 30) +
  
  theme_new()+ 
  theme(legend.position = c(0.1,0.5),

        # plot.margin=margin(t=0, r = 0, l = 0, 0, unit="cm"),

        plot.margin = unit(c(0.1,0.1,0.1,0.1), units = "cm"),

        # # legend.position = "none",
        # legend.key = element_blank(),
        # legend.background = element_rect(fill="white", size=1),
        # legend.key.size=unit(1,"cm"),
        # legend.text = element_text(colour = "black", size =16, family = "Times"),
        # legend.title = element_text(colour = "black", size =16, family = "Times"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),

        axis.title.y = element_text(colour = "black",size =14, family = "Times", angle = 90, vjust =  1),
        axis.title.x = element_blank(),
        axis.text.y=element_text(colour = "black",size = 14, family = "Times"),
        axis.text.x = element_text(colour = "black",size = 14, family = "Times", vjust = 1),

        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))

# pmice

# makes graphs smaller
  result.plot <- cowplot::plot_grid(pseed, pmice ,nrow = 2, 
                                    align="v", 
                                    labels = c("a)", "b)"),
                                    rel_heights = c(0.7, 1.3))

 result.plot

# Save plot

jpeg("./figs/fig-1.jpeg", width = 20, height = 20, units = 'cm', res = 400)
result.plot
dev.off()