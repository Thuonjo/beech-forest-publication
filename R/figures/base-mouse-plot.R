# By grid plot
# Base plot code
# Can I turn this into a function I can use to select either one of 3 different estimates of abundance into
# seed, density, rats?
meanM <- dat.msr.1 %>%
  filter(spp == "mice") %>%
  mutate(true.date = as.Date(true.date),
         mean.s = est.dat)

meanM.1 <- mean1 %>%
  filter(spp == "mice")  %>%
  mutate(true.date = as.Date(true.date))

mice.aver.plot <-
  ggplot(data = meanM.1,
         aes(y = mean.s,
             x = true.date,col = Conditions, shape = valley, fill = Conditions)) +
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
  
  geom_point(data = meanM, aes(col = Conditions, shape = valley, fill = Conditions), size = 2, alpha = 0.3) + 
  geom_line(data = meanM.1, aes(group = Conditions, col = Conditions), size = 0.9, alpha = 0.8) + 
  # geom_errorbar(data = meanM.1, mapping = aes(ymin = lcl.s, ymax = ucl.s), width = 0, alpha = 0.5, lwd = 0.75) +
  
  geom_point(data = meanM.1, aes(y = mean.s,
                                 x = true.date,shape = valley), stroke = 1.5, size = 3) + 
  
  scale_shape_manual(name = "Valley",
                     labels = c("E", "H"),
                     values = c(25,21)) +
  
  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("black","black", "darkgoldenrod")) +
  
  geom_hline(yintercept = 0,
             lty = 5,
             alpha = 0.7)  +
  
  xlab(expression(paste("Time","(",italic(t),")"))) +
  
  ylab(expression(atop(paste("Mouse "," ", " Abundance"," "),
                       paste("(",italic(N[jt]),")"))))

mice.aver.plot <- mice.aver.plot +  theme(strip.background = element_blank(),
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
