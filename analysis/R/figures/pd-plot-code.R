# Rate of increase between Autumn and Winter 
# (May and August rate of change)
# May mice abundace, seed and rat data
# Overall Data
# source("./R/manuscript-source-code.R", echo = FALSE)

# data --------------------------------------------------------------------
# out.final.3
# glimpse(out.final.3$month)
# 
# table(out.final.3$month)
# pred.lines.s5
# 
# table(pred.lines.s5$month)
# glimpse(pred.lines.s5$month)
# 

# just most interesting increasing months ---------------------------------
out.final.3.red <- out.final.3 %>% 
                    filter(month == "Feb")

pred.lines.d5.red <- pred.lines.d5 %>%
  filter(month == "Feb") 

# %>%
#     mutate(pt.lines = ifelse(as.character(pt.lines) == "min.r", "MAX.r", "min.r"))
# 
# %>%
#     mutate(month = "Feb")

pd.plot.aug <-  ggplot(out.final.3.red, aes(y = mean.r, x = lag.N)) +
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.1, position=position_dodge(width=30), width = 0) +
  geom_point(aes(colour = valley,shape = valley, fill = control),
             stroke = 1.5, size = 4, alpha = 0.8) +
  
  geom_point(
    data = pred.lines.d5.red,
    aes(
      y = mean.r,
      x = lag.N,
      colour = valley,
      shape = valley,
      fill = control,
      group = controlT),
    size = 3
  ) +
  
  geom_line(
    data = pred.lines.d5.red,
    aes(
      y = mean.r,
      x = lag.N,
      colour = valley, group = controlT),
    size = 1
  ) + scale_shape_manual(name = "Valley",
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
  # xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
#
# ylab(expression(atop(paste("Rate"," ", "of"," ",
#                            "increase"),paste(" ", "of"," ",
#                                              "mice"," ","(",italic(r[jt]),")"))) ) +
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


# final plot --------------------------------------------------------------

pd.dens.full <-  ggplot(out.final.3, aes(y = mean.r, x = lag.N)) +
geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.1, position=position_dodge(width=30), width = 0) +
  geom_point(aes(colour = valley,shape = valley, fill = control),
             stroke = 1.5, size = 4, alpha = 0.8) +

  geom_point(
    data = pred.lines.s5,
    aes(
      y = mean.r,
      x = lag.N,
      colour = valley,
      shape = valley,
      fill = control,
      group = controlT),
    size = 3
  ) +
  
  geom_line(
    data = pred.lines.s5,
    aes(
      y = mean.r,
      x = lag.N,
      colour = valley, group = controlT),
    size = 1
  ) +
  facet_wrap(~month, scales = "free")




