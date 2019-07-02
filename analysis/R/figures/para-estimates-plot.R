# All parameter estimates for tables and plots

# Overall Data
# source("./R/manuscript-source-code.R", echo = FALSE)

# data --------------------------------------------------------------------

# plot
#plot code
glimpse(out.final)

para.plot.dat <- out.final %>%
  mutate_if(is.factor, as.character) %>%
  # filter(month == "Feb") %>%
    droplevels()
  
                  # mutate(Estimate = mean.b,
                  #        Lower = lcl.b,
                  #        Upper = ucl.b,
                  #        Control = control,
                  #        Valley = valley,)

para.plot.dat$para <- factor(para.plot.dat$para, labels = c("Density", "Rats", "Seed", "Intercept"))
 
# plot
para.plot <- ggplot(data = para.plot.dat, aes(y = mean.b, x = control, colour = valley,
                                                              shape = valley, 
                                                              fill = control, 
                                   group = paste(control, valley))) +
  geom_abline(intercept = 0,slope = 0, col = "red", size = 0.85) +
    geom_errorbar(aes(ymin = lcl.b, ymax = ucl.b, width = 0), lwd = 1.1, position = position_dodge(width = 0.25)) +
       geom_point(stroke = 1.5, size = 2, alpha = 0.8, width = 0.25, position = position_dodge(width = 0.25)) + 
  facet_grid(month~para, scales = "free") + 
  
  #to DO
  #parameters
  #bayes output and compare
  #using tidybayes package
  
  scale_shape_manual(name = "Valley",
                     labels = c("E", "H"),
                     values = c(25,21)) +
  
  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford","Eglinton", "Hollyford"),
                      values = c("darkgoldenrod","black","darkgoldenrod","black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes","No"),
                    values = c("darkgoldenrod","black","darkgoldenrod","black")) +
  # theme_stata()#
# # scale_colour_manual(name = "Stoat control",
# #                     labels = c("E-", "H+", "H-"),
# #                     values = c("black","black", "black")) +
# # scale_shape_manual(name = "Valley",
# #                    labels = c("E", "H"),
# #                    values = c(25,21)) +
# # scale_fill_manual(name = "Stoat control",
# #                   labels = c("E-", "H+", "H-"),
# #                   values = c("white","black", "white")) +
ylab("Parameter estimate") + 
# scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
# theme_stata()
#   
#   +
#   theme_bw() +
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

        strip.text = element_text(face="bold",colour = "black",size =14, family = "Times")) +
    theme_stata()
  
para.plot

#   ggplot(out.final, aes(y = mean.b, x = control, colour = valley, shape = valley, fill = control)) +
#   geom_jitter(size = 4, width = 0.3) +
#   # geom_line() +
#   facet_grid(para~month, scale = "free") +
# 
#   scale_shape_manual(name = "Valley",
#                      labels = c("E", "H"),
#                      values = c(25,21)) +
# 
#   scale_colour_manual(name = "Stoat control",
#                       labels = c("Eglinton", "Hollyford","Eglinton", "Hollyford"),
#                       values = c("darkgoldenrod","black","darkgoldenrod","black")) +
#   scale_fill_manual(name = "Stoat control",
#                     labels = c("Yes", "No", "Yes","No"),
#                     values = c("darkgoldenrod","black","darkgoldenrod","black")) +
#   # theme_stata()#
# # # scale_colour_manual(name = "Stoat control",
# # #                     labels = c("E-", "H+", "H-"),
# # #                     values = c("black","black", "black")) +
# # # scale_shape_manual(name = "Valley",
# # #                    labels = c("E", "H"),
# # #                    values = c(25,21)) +
# # # scale_fill_manual(name = "Stoat control",
# # #                   labels = c("E-", "H+", "H-"),
# # #                   values = c("white","black", "white")) +
# # xlab(expression(atop(paste("Minimum"," ", "number"," "),paste("of", " ", "rats"," ","(",italic(R[jt]),")"))) )+
# #
# # ylab(expression(atop(paste("Rate"," ", "of"," ",
# #                            "increase"),paste(" ", "of"," ",
# #                                              "mice"," ","(",italic(r[jt]),")"))) ) +
# # scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
# theme_tufte() +
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         strip.text.y = element_blank(),
# 
#         plot.title = element_text(hjust = 0, size=24, family = "Times", color="black", margin = margin(t = 10, b = 10)),
#         plot.subtitle=element_text(size=16, face="italic", color="black"),
# 
#         legend.position = "none",
#         legend.key = element_blank(),
#         legend.background = element_rect(fill="white", size=1),
#         legend.key.size=unit(1,"cm"),
#         legend.text = element_text(colour = "black", size =16, family = "Times"),
#         legend.title = element_text(colour = "black", size =16, family = "Times"),
# 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(2, "lines"),
#         panel.border = element_blank(),
# 
#         axis.title.y = element_text(colour = "black",size =20, family = "Times", angle = 90),
#         axis.title.x = element_text(colour = "black", size =20, family = "Times"),
#         axis.text.y=element_text(colour = "black",size = 20, family = "Times"),
#         axis.text.x = element_text(colour = "black", size =20, family = "Times"),
# 
#         axis.ticks.x = element_line(size = 1),
#         axis.ticks.y = element_line(size = 1),
#         axis.line.x = element_line(size = 1),
#         axis.line.y = element_line(size = 1),
# 
#         strip.text = element_text(face="bold",colour = "black",size =14, family = "Times"))
