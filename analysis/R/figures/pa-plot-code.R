# Prediction A plot
# April 2019
# Anthony Davidson

# head(low.abund.dat)

low.plot.time <-  low.abund.dat %>%
  select(N, valley, control) %>%
  ggplot(aes(y = N, x = control)) +
  # geom_line(alpha = 0.5) +
  geom_jitter(aes(colour = valley,
                  shape = valley, 
                  fill = control),
             stroke = 1.5, size = 2, alpha = 0.8, width = 0.25) +
  geom_point(data = low.con, aes(y = N, x = control), shape = "square",  size = 5) +
  geom_errorbar(data = low.con1, aes(ymin = lcl.low, ymax = ucl.low, width = 0), lwd = 1.1) +

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
  
  ylab(expression(atop(paste("Mouse "," ", " Abundance"," "),
                       paste("(",italic(N[jt]),")"))))+
  # geom_hline(yintercept = 0,
  #            lty = 5,
  #            alpha = 0.7) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 12))

# log.low.plot.time <-  ggplot(low.abund.dat, aes(y = log(N), x = trip, group = grid)) +
#   geom_line(alpha = 0.5) +
#   geom_point(aes(fill = Conditions, shape = valley, colour = Conditions),
#              stroke = 1.5, size = 4, alpha = 0.7) +
#   # geom_smooth(aes(colour = Conditions), lty = 3, size = 0.9) +
#   scale_shape_manual(name = "Valley",
#                      labels = c("Eglinton", "Hollyford"),
#                      values = c(25,21)) +
#   
#   scale_colour_manual(name = "Stoat control",
#                       labels = c("Eglinton", "Hollyford", "Hollyford"),
#                       values = c("darkgoldenrod","black", "black")) +
#   scale_fill_manual(name = "Stoat control",
#                     labels = c("Yes", "No", "Yes"),
#                     values = c("darkgoldenrod","black", "darkgoldenrod")) +
#   xlab(expression(paste("Time","(",italic(t),")"))) +
#   
#   ylab(expression(atop(paste("log(Mouse Abundance)"," "),
#                        paste("(",italic(N[jt]),")"))))+
#   geom_hline(yintercept = 0,
#              lty = 5,
#              alpha = 0.7) +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 12))
# 
