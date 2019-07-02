# By grid plot
# Base plot code
# May2019 fully sorted


# # Can I turn this into a function I can use to select either one of 3 different estimates of abundance into
# # seed, density, rats?
# meanS <- dat.msr.1 %>%
#   filter(spp == "seed") %>%
#   mutate(true.date = as.Date(true.date),
#          mean.rat = est.dat)
#
# #comes from summary Rscript.
# # source(./R/figures/summar...)
# meanS.1 <- mean1 %>%
#   filter(spp == "seed")  %>%
#   mutate(true.date = as.Date(true.date))

# Cleaning up total dataset
plot.dat.all1  <- plot.dat.all1 %>%
  mutate(trip = as.character(trip))# already done!

# rats have only 80 estimates not 144
meanR <- read_csv("C://Code/data/mna_allrat.csv") %>%
  select (valley, grid ,trip.no ,n) %>%
  # select (grid ,trip.no ,n) %>%
  mutate(trip = as.character(trip.no),
            grid = grid,
            rat.mna = n,
         Valley = factor(valley, labels = c("Eglinton", "Hollyford")))

joined.rats <- left_join(plot.dat.all1, meanR, by = c("grid", "Valley", "trip"))
# glimpse(joined.rats)

# joined.rats <- inner_join(plot.dat.all1,meanR, by = c("grid1", "valley", "trip"))
# glimpse(joined.rats)
# table(joined.rats$rat.mna)

# names(plot.dat.all1)
# names(meanR)

# joined.rats$Valley
# joined.rats$Valley.y

# summaries
rat.mean <- joined.rats %>%
  group_by(Control, Valley, Date) %>%
  summarise(mean.rat = mean(rat.mna, na.rm = TRUE),
    sd.rat = sd(rat.mna, na.rm = TRUE),
    se.rat = sd.rat / sqrt(length(rat.mna)) * 1.96,
    lcl.rat = mean.rat - (sd.rat / sqrt(length(rat.mna)) *
                                1.96),
    ucl.rat = mean.rat + (sd.rat / sqrt(length(rat.mna)) *
                                1.96)
  ) %>%
  ungroup()

rat.mean <- rat.mean %>%
  mutate(Valley = Valley,
    gp.treat = factor(paste(Valley, Control)),
    N = mean.rat,
    Rats = factor("Full")
  )

# glimpse(joined.rats)

# sorting summary dataset for the last time -------------------------------
#summarising seed
# table(is.na(p.design1$grid))
# #remove NA for making space in last plot
# p.design1 <- joined.rats[1:144, ]
# 
# 
# p.design1 <- left_join(p.design1, meanR, by = c("grid1", "valley", "trip"))
# 
# #no grid na anymore
# table(is.na(p.design1$grid))
# 
# #making datasest
# p.design2 <- rat.mean %>%
#   group_by(Control, Valley, Date) %>%
#   summarise(N = mean(N),
#             Rats = factor("Full", levels = c("Full", "Reduced"))) %>%
#   ungroup() %>%
#   mutate(grid = factor(paste(Control, Valley)))
# 
# # grouping of grid correct?
# levels(p.design2$grid)
# p.design2$Rats
# 
# glimpse(p.design2)
# 
# 
# levels(rat.mean$gp.treat)


# plots -------------------------------------------------------------------
#plot raw seed
fig.3.rat <- ggplot(joined.rats,
                     aes(
                       y = rat.mna,
                       col = Rats,
                       shape = Valley,
                       fill = Control,
                       x = Date
                     )) +
  # geom_line(col = "grey50") +
  geom_point(aes(size = Rats),
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
    data = rat.mean,
    aes(y = mean.rat,
        x = Date),
    size = 0.95,
    col = "grey50"
  ) +
  
  geom_point(
    data = rat.mean,
    aes(
      y = mean.rat,
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
  
  ylab(expression(atop(paste("Minimum "," ", " number"," "),
                       paste(" ", "of"," ",
                             "rats"," ","(",italic(R[jt]),")"))) )



fig.3.rat

# export plot for example vignette
jpeg("./figs/fig-3.1-study.jpeg")
fig.2.plot.design
dev.off()
