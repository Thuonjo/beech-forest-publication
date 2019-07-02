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

# # Cleaning up total dataset
# plot.dat.all1 # already done!
# 
# # summaries
# mouse.mean <- plot.dat.all1 %>%
#   group_by(Control, Valley, Date) %>%
#   summarise(
#     mean.s = mean(N),
#     sd.s = sd(N),
#     se.s = sd(N) / sqrt(length(N)) * 1.96,
#     lcl.s = mean(N) - (sd(N) / sqrt(length(N)) *
#                                 1.96),
#     ucl.s = mean(N) + (sd(N) / sqrt(length(N)) *
#                                 1.96)
#   ) %>%
#   ungroup()
# 
# mouse.mean <- mouse.mean %>%
#   mutate(
#     gp.treat = factor(paste(Valley, Control)),
#     N = mean.s,
#     Rats = factor("Full")
#   )

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
  summarise(N = mean(N),
            Rats = factor("Full", levels = c("Full", "Reduced"))) %>%
  ungroup() %>%
  mutate(grid = factor(paste(Control, Valley)))

# grouping of grid correct?
levels(p.design2$grid)
p.design2$Rats

# glimpse(p.design2)


levels(seed.mean$gp.treat)


# plots -------------------------------------------------------------------
#plot raw seed
fig.3.N <- ggplot(p.design1,
                     aes(
                       y = N,
                       col = Rats,
                       shape = Valley,
                       fill = Control,
                       x = Date
                     )) +
  # geom_line(col = "grey50") +
  geom_point(aes(size = Rats,
                 group = grid),
             stroke = 1.25,
             alpha = 0.3) +
  
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
    aes(y = N,
        x = Date),
    size = 0.95,
    col = "grey50"
  ) +
  
  geom_point(
    data = p.design2,
    aes(
      y = N,
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
  
  xlab(expression(paste("Time", "(", italic(t), ")"))) +

  ylab(expression(atop(paste("Mouse "," ", " Abundance"," "),
                       paste("(",italic(N[jt]),")"))))



fig.3.N

# export plot for example vignette
jpeg("./figs/fig-3.1-study.jpeg")
fig.2.plot.design
dev.off()
