# By grid plot
# Base plot code
# May2019 fully sorted


# # Can I turn this into a function I can use to select either one of 3 different estimates of abundance into
# # seed, density, rats?
# meanS <- dat.msr.1 %>%
#   filter(spp == "seed") %>%
#   mutate(true.date = as.Date(true.date),
#          mean.s = est.dat)
#
# #comes from summary Rscript.
# # source(./R/figures/summar...)
# meanS.1 <- mean1 %>%
#   filter(spp == "seed")  %>%
#   mutate(true.date = as.Date(true.date))

# Cleaning up total dataset
# plot.dat.all1 # already done!

# summaries
seed.mean <- plot.dat.all1 %>%
  group_by(Control, Valley, Date) %>%
  summarise(
    mean.s = mean(cum.seed),
    sd.s = sd(cum.seed),
    se.s = sd(cum.seed) / sqrt(length(cum.seed)) * 1.96,
    lcl.s = mean(cum.seed) - (sd(cum.seed) / sqrt(length(cum.seed)) *
                                1.96),
    ucl.s = mean(cum.seed) + (sd(cum.seed) / sqrt(length(cum.seed)) *
                                1.96)
  ) %>%
  ungroup()

seed.mean <- seed.mean %>%
  mutate(
    gp.treat = factor(paste(Valley, Control)),
    cum.seed = mean.s,
    Rats = factor("Full")
  )


# levels(seed.mean$gp.treat)
# note that plot uses both meanS and meanS.1
# seed.aver.plot <-
ggplot(data = plot.dat.all1,
       aes(
         y = cum.seed,
         x = Date,
         shape = Valley,
         fill = Control,
         col = Rats)) +
  
  # extra goodies
  geom_rect(aes(
    xmin = ymd('2000-01-01'),
    xmax = ymd('2000-12-31'),
    ymin = -Inf,
    ymax = Inf
  ),
  colour = "grey90",
  fill = "grey90") +
  
  geom_rect(aes(
    xmin = ymd('2002-01-01'),
    xmax = ymd('2002-12-31'),
    ymin = -Inf,
    ymax = Inf
  ),
  colour = "grey90",
  fill = "grey90") +
  
  geom_rect(aes(
    xmin = ymd('2004-01-01'),
    xmax = ymd('2004-12-31'),
    ymin = -Inf,
    ymax = Inf
  ),
  colour = "grey90",
  fill = "grey90") +
  
  geom_point(aes(fill = Control), stroke = 1.15, size = 3) +
  
  #   geom_line(data = seed.mean, aes(y = mean.s,
  #                                   x = Date, group = gp.treat), size = 0.9) +
  #
  # # +
  #   # geom_errorbar(data = meanS.1, mapping = aes(ymin = lcl.s, ymax = ucl.s), width = 0, alpha = 0.5, lwd = 0.75) +
  #
  geom_line(data = seed.mean,
            aes(y = mean.s,
                x = Date),
            size = 1,
            col = "grey50") +
  
  geom_point(data = seed.mean, aes(y = mean.s,
                                   x = Date), size = 5) +
  
  
  
  scale_color_manual(name = "Stoat Control",
                     values = c("white", "black")) +
  
  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  
  # manually define the fill colours
  #
  scale_fill_manual(
    name = "Stoat Control",
    values = c("cornflowerblue", "darkorange", "white", "black")
  ) +
  
  # defining size with 2 marginally different values
  scale_size_manual(name = "Rat Control", values = c(5, 4)) +
  
  # geom_abline(intercept = 5, slope = 0, size = 1) +
  # theme
  theme_new() +
  
  
  # labels
  # scale_y_discrete(labels = c("1","2","3","4"," ","1","2","3","4")) +
  # scale_x_date() +
  # defining size with 2 marginally different values
  # scale_size_manual(name = "Rat Control", values = c(4, 3)) +
  
  # Remove fill legend and replace the fill legend using the newly created size
  guides(
    col = "none",
    shape = guide_legend(override.aes = list(shape = c(24, 21))),
    fill = guide_legend(override.aes = list(col = c(
      "cornflowerblue", "darkorange"
    )))
  )



# study design plot
# may 2019
#anthony

source("./R/davidson_2019_theme.r")
# source("./R/figures/study-design-data.R")

p.design <- plot.dat.all1 %>%
  mutate(grid = as.numeric(factor(grid)))

# glimpse(plot.dat.all1)

# adding NA grid to plot

bd.row <- p.design[1, ]
bd.row$grid <- "blank"
bd.row$grid <- "blank"
bd.row$grid <- "blank"
bd.row$grid <- "blank"
bd.row$Date <- NA

# bind row to plot data
p.design145 <- rbind(p.design, bd.row)
# glimpse(p.design145$grid)

# labels
rat.labs <- c("No", "Reduced")

#re-factoring
p.design1 <- p.design145  %>%
  mutate(grid = factor(grid, levels = c(
    "1", "2", "3", "4", "blank", "5", "6", "7", "8"
  )),
  Rats = factor(Rats, labels = rat.labs))


#checking
# tail(p.design1)
# tail(filter(p.design1, grid == "blank"))
# levels(p.design1$grid)
# labels() <- c("1","2","3","4"," ","5","6","7","8")

# p.design1$grid <- factor(p.design1$grid,  labels = c("1","2","3","4"," ","1","2","3","4"))

#plot raw seed
fig.3.plot.seed <- ggplot(p.design1,
                          aes(
                            y = cum.seed,
                            col = Rats,
                            shape = Valley,
                            fill = Control,
                            x = Date
                          ),
                          size = 4) +
  # geom_line(col = "grey50") +
  geom_point(aes(size = Rats,
                 group = grid), stroke = 1.25) +
  
  scale_color_manual(name = "Stoat Control",
                     values = c("white", "black")) +
  
  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  
  # manually define the fill colours
  
  scale_fill_manual(name = "Stoat Control",
                    values = c("cornflowerblue", "darkorange")) +
  # geom_abline(intercept = 5, slope = 0, size = 1) +
  # theme
  theme_new() +
  
  scale_size_manual(name = "Rat Control", values = c(4, 3)) +
  # labels
  # scale_y_discrete(labels = c("1","2","3","4"," ","1","2","3","4")) +
  # scale_x_date() +
  # defining size with 2 marginally different values
  
  # Remove fill legend and replace the fill legend using the newly created size
  guides(
    col = "none",
    size = guide_legend(override.aes = list(shape = c(16, 1))),
    shape = guide_legend(override.aes = list(
      shape = c(24, 21), size = 4
    )),
    fill = guide_legend(override.aes = list(
      col = c("cornflowerblue", "darkorange"),
      size = 4
    ))
  ) +
  #   col = "none",
  #   # size = guide_legend(override.aes = list(shape = c(16, 1))),
  #   shape = guide_legend(override.aes = list(
  #     shape = c(24, 21), size = 4
  #   ))
  #   # fill = guide_legend(override.aes = list(
  #     # col = c("cornflowerblue", "darkorange"), size = 4
  #   # ))
  # ) +
  
  #   geom_hline(yintercept = 0,
#              lty = 5,
#              alpha = 0.7)  +
#
xlab(expression(paste("Time", "(", italic(t), ")"))) +
  
  ylab(expression(paste("Available seed ", "(", italic(Seed[jt]), ")")))

fig.3.plot.seed


# sorting summary dataset for the last time -------------------------------
#summarising seed
table(is.na(p.design1$grid))
#remove NA for making space in last plot
p.design1 <- p.design1[1:144, ]
#no grid na anymore
table(is.na(p.design1$grid))

#making datasest
p.design2 <- p.design1 %>%
  group_by(Control, Valley, Date) %>%
  summarise(cum.seed = mean(cum.seed),
            Rats = factor("Full", levels = c("Full", "Reduced"))) %>%
  ungroup() %>%
  mutate(grid = factor(paste(Control, Valley)))

# grouping of grid correct?
levels(p.design2$grid)
p.design2$Rats

# glimpse(p.design2)
#plot summary seed

# Add NA so legend works
row.input.leg <- p.design2[1, ]
row.input.leg$Control <- NA
row.input.leg$Valley <- NA
row.input.leg$Date <- NA
row.input.leg$cum.seed <- NA
row.input.leg$Rats <- "Reduced"
row.input.leg$grid <- NA

p.design3 <- rbind(p.design2, row.input.leg)
# glimpse(p.design3)
levels(p.design3$Rats)
col = c("cornflowerblue", "darkorange")

#plot raw seed
fig.3.plot.seed.sum <- ggplot(p.design2,
                              aes(
                                y = cum.seed,
                                col = Rats,
                                shape = Valley,
                                fill = Control,
                                x = Date
                              ),
                              size = 4) +
  geom_line(col = "grey50") +
  geom_point(aes(size = Rats,
                 group = grid), stroke = 1.25) +
  
  scale_color_manual(name = "Stoat Control",
                     values = c("white", "black")) +
  
  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  
  scale_size_manual(name = "Rat Control", values = c(4, 3)) +
  # manually define the fill colours
  
  scale_fill_manual(name = "Stoat Control",
                    values = c("cornflowerblue", "darkorange")) +
  # geom_abline(intercept = 5, slope = 0, size = 1) +
  # theme
  theme_new() +
  
  
  # labels
  # scale_y_discrete(labels = c("1","2","3","4"," ","1","2","3","4")) +
  # scale_x_date() +
  # defining size with 2 marginally different values
  
  # Remove fill legend and replace the fill legend using the newly created size
  guides(
    col = "none",
    size = "none",
    shape = guide_legend(override.aes = list(
      shape = c(24, 21), size = 4
    )),
    fill = guide_legend(override.aes = list(
      col = c("cornflowerblue", "darkorange"),shape = c("square"),
      size = 4
    ))
  ) +
  #   col = "none",
  #   # size = guide_legend(override.aes = list(shape = c(16, 1))),
  #   shape = guide_legend(override.aes = list(
  #     shape = c(24, 21), size = 4
  #   ))
  #   # fill = guide_legend(override.aes = list(
  #     # col = c("cornflowerblue", "darkorange"), size = 4
  #   # ))
  # ) +
  
  #   geom_hline(yintercept = 0,
#              lty = 5,
#              alpha = 0.7)  +
#
xlab(expression(paste("Time", "(", italic(t), ")"))) +
  
  ylab(expression(paste("Available seed ", "(", italic(Seed[jt]), ")")))

fig.3.plot.seed.sum



# Combine them ------------------------------------------------------------

#plot raw seed
fig.3.seed <- ggplot(p.design1,
                     aes(
                       y = cum.seed,
                       col = Rats,
                       shape = Valley,
                       fill = Control,
                       x = Date
                     )) +
  # geom_line(col = "grey50") +
  geom_point(aes(size = Rats,
                 group = grid),
             stroke = 1.25,
             alpha = 0.5) +
  
  scale_color_manual(name = "Stoat Control",
                     values = c("white", "black", "white")) +
  
  scale_shape_manual(name = "Ecosystem",
                     values = c(24, 21)) +
  
  scale_size_manual(name = "Rat Control", values = c(2.5, 3, 2.5)) +
  # manually define the fill colours
  
  scale_fill_manual(
    name = "Stoat Control",
    values = c("cornflowerblue", "darkorange", "cornflowerblue")
  ) +
  
  
  geom_line(
    data = p.design2,
    aes(y = cum.seed,
        x = Date),
    size = 0.95,
    col = "grey50"
  ) +
  
  geom_point(
    data = p.design2,
    aes(
      y = cum.seed,
      col = Rats,
      shape = Valley,
      fill = Control,
      x = Date
    ),
    size = 7
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
  
  xlab(expression(paste("Timing of sample", "(", italic(t), ")"))) +
  
  ylab(expression(paste("Available seed ", "(", italic(Seed[jt]), ")")))



fig.3.seed

# export plot for example vignette
jpeg("./figs/fig-3.1-study.jpeg")
fig.2.plot.design
dev.off()
