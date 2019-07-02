# Prediction B plot
# April 2019
# Anthony Davidson


high.plot.time <-  high.cond.sum %>%
  select(mean.seed.c, valley, control, Conditions) %>%
   ggplot(aes(y = mean.seed.c, 
              x = control)) +
  # geom_line(alpha = 0.5) +
  geom_jitter(aes(colour = valley,shape = valley, fill = control),
             stroke = 1.5, size = 4, alpha = 0.8, width = 0.25) +
  geom_point(data = high.con, aes(y = mean.seed.c, x = control), shape = "square",  size = 5) +
  geom_errorbar(data = high.con, aes(ymin = lcl.high, ymax = ucl.high, width = 0), lwd = 1.1) +
  
  scale_shape_manual(name = "Valley",
                     labels = c("E", "H"),
                     values = c(25,21)) +
  
  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford"),
                      values = c("darkgoldenrod","black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No"),
                    values = c("darkgoldenrod","black")) +
  xlab(expression(paste("Time","(",italic(t),")"))) +
  
  ylab(expression(atop(paste("Relative "," ","mouse "," ", "abundance"," "),
                       paste(italic(N[jt])/italic(S[jt]))))) +
  # geom_hline(yintercept = 0,
  #            lty = 5,
  #            alpha = 0.7) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 12))

log.high.plot.time <-  ggplot(high.abund.dat, aes(y = log(N), x = trip, group = grid)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(fill = Conditions, shape = valley, colour = Conditions),
             stroke = 1.5, size = 4, alpha = 0.7) +
  # geom_smooth(aes(colour = Conditions), lty = 3, size = 0.9) +
  scale_shape_manual(name = "Valley",
                     labels = c("Eglinton", "Hollyford"),
                     values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  xlab(expression(paste("Time","(",italic(t),")"))) +

  ylab(expression(atop(paste("log(Mouse Abundance)"," "),
                       paste("(",italic(N[jt]),")"))))+
  geom_hline(yintercept = 0,
             lty = 5,
             alpha = 0.7) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

