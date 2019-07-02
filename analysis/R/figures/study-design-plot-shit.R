# study design plot
# may 2019
#anthony

source("./R/davidson_2019_theme.r")
# source("./R/figures/study-design-data.R")

p.design <- plot.dat.all1 %>%
  mutate(grid = as.numeric(factor(grid)))

# glimpse(plot.dat.all1)

# adding NA grid to plot

bd.row <- p.design[1,]
bd.row$grid <- "blank"
bd.row$grid <- "blank"
bd.row$grid <- "blank"
bd.row$grid <- "blank"
bd.row$Date <- NA

# bind row to plot data
p.design145 <- rbind(p.design,bd.row)
# glimpse(p.design145$grid)

# labels
rat.labs <- c("No", "Reduced")

#re-factoring
p.design1 <- p.design145  %>%
                mutate(grid = factor(grid, levels = c("1","2","3","4","blank","5","6","7","8")),
                       Rats = factor(Rats, labels = rat.labs))


#checking
# tail(p.design1)
# tail(filter(p.design1, grid == "blank"))
# levels(p.design1$grid)
# labels() <- c("1","2","3","4"," ","5","6","7","8")

# p.design1$grid <- factor(p.design1$grid,  labels = c("1","2","3","4"," ","1","2","3","4"))

#plot
fig.2.plot.design <- ggplot(
  p.design1,
  aes(y = cum.seed,
    x = Date),
  size = 4
) +
  # geom_line(col = "grey50") +
  # geom_point(aes(
  #   shape = Valley,
  #   fill = Control,col = Rats, size = Rats), stroke = 1.25, size = 4,alpha = 0.4) +
  # 
  scale_color_manual(name = "Stoat Control",
                     values = c("white", "black", "white")) +
  
  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  
  # manually define the fill colours
  
  scale_fill_manual(name = "Stoat Control",
                    values = c("cornflowerblue", "darkorange","cornflowerblue")) +
  geom_abline(intercept = 5, slope = 0, size = 1) +
  # theme
  theme_new() +
  
  
  # labels
  # scale_y_discrete(labels = c("1","2","3","4"," ","1","2","3","4")) +
  # scale_x_date() +
  # defining size with 2 marginally different values
  # scale_size_manual(name = "Rat Control", values = c(4, 3)) +
  
  # Remove fill legend and replace the fill legend using the newly created size
  guides(col = "none",
    size = guide_legend(override.aes = list(shape = c(16, 1))),
    shape = guide_legend(override.aes = list(
      shape = c(24, 21), size = 4
    )),
    fill = guide_legend(override.aes = list(
      col = c("cornflowerblue", "darkorange"), size = 4
    ))) +
  xlab("Timing of sample") +   
  
  # geom_line(data = seed.mean, aes(y = mean.s,
  #                                    x = Date), size = 1, col = "grey50") +
    
    geom_point(data = seed.mean, aes(y = cum.seed,
                                   x = Date, col = Control, shape = Valley), size = 5) +
      geom_line(data = seed.mean, aes(y = cum.seed,
                                   x = Date, group = gp.treat), size = 1) +
  ylab("Grid Location")

fig.2.plot.design


r# export plot for example vignette
jpeg("./figs/fig-2-study.jpeg")
fig.2.plot.design
dev.off()