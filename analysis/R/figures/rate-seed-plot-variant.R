# Rate of increase between Autumn and Winter 
# (May and August rate of change)
# May mice abundace, seed and rat data


# data --------------------------------------------------------------------
# reduced data
# out.pC.1 <- filter(out.final.2, month == "Nov")

#main data from manuscript main file
# but needs valley, control and Conditions
glimpse(out.final.2)
names(out.final.2)

out.final.3 <- out.final.2 %>%
  transmute(mean.r = as.numeric(mean.r),
            lag.sjt = as.numeric(lag.sjt),
            valley = factor(valley),
            control = factor(ifelse(controlT == "holN", "No", "Yes")),
            Conditions = factor(ifelse(grid.id == "M1" | grid.id =="M2",
                                "rats.removed", "rats.present")),
            lcl.r = as.numeric(lcl.r),
            ucl.r = as.numeric(ucl.r),
            month = month)

levels(out.final.3$control)

# used to be 
# out.r <- read_csv("C://Code/data/CR_output_pred.csv") 


# lines data for seed -----------------------------------------------------


# Seed lines (12x) --------------------------------------------------------

names(pred.lines.1)
# month.overall <- "Aug"
#  month.overall <- unique(model$month)
#  data = filter(model, month == month.overall)
pred.lines.2 <- pred.lines.1 %>%
  mutate(valley = factor(valley),
            control = factor(ifelse(controlT == "holN", "No", "Yes")))
# levels(model$month)

pred.lines.s2 <- pred.lines.2 %>%
  select(valley, control, month, min.r, MAX.r, min.seed, MAX.seed) %>%
  droplevels() %>%
  gather(value = mean.r, key = pt.lines, min.r:MAX.seed)


glimpse(pred.lines.s2)
str(pred.lines.1$month)
str(pred.lines.s2$month)
str(pred.lines.s2$month)
str(pred.lines.s2$month)

# table(pred.lines.s2.1$pt.lines)

pred.lines.s2.1 <- filter(pred.lines.s2, 
                          pt.lines == "MAX.seed" | pt.lines == "min.seed") %>%
  droplevels() %>%
  select(mean.r, valley, control, month) %>%
  transmute(lag.sjt = mean.r,
            valley = factor(valley),
            control = factor(control),
            month  = factor(month)) %>%
    drop_na()

str(out.final.1$month)
lag.sjt <- pred.lines.s2.1$lag.sjt

pred.lines.s3 <- cbind(pred.lines.s2[1:24,], lag.sjt)

glimpse(pred.lines.s3)

pred.lines.s4 <- pred.lines.s3 %>%
  mutate(pt.lines = factor(pt.lines))

pred.lines.s5 <- pred.lines.s4 %>%
                  drop_na()

# final plot --------------------------------------------------------------

# pc.seed.plot <-  
 ggplot(out.final.3, aes(y = mean.r, x = lag.sjt)) +
  geom_point(data = pred.lines.s5, aes(y = mean.r, x = lag.sjt, shape = valley, colour = valley, fill = control), size = 3) +
  geom_line(data = pred.lines.s5, aes(y = mean.r, x = lag.sjt,colour = valley, group = pt.lines), size = 1)+
# ggplot(out.r.1, aes(y = mean.r, x = lag.sjt)) +
  # geom_line(data = filter(model.e, seed < 4), aes(y = mice, x = seed), 
  #           size = 1.05, color = "grey50", lty = 5) + 
  # geom_line(data = filter(model.hc, seed < 4), aes(y = mice, x = seed), 
  #           size = 1.05, colour = "black", lty = 5) + 
  # geom_line(data = filter(model.hs, seed < 4), aes(y = mice, x = seed), 
  #           colour = "black", size = 1.25) + 
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.1, position=position_dodge(width=30), width = 0) +
   geom_point(aes(colour = valley,shape = valley, fill = control),
               stroke = 1.5, size = 4, alpha = 0.8) +


 facet_wrap(~month, scales = "free") +
  
   scale_shape_manual(name = "Valley",
                      labels = c("E", "H"),
                      values = c(25,21)) +
   # 
   scale_colour_manual(name = "Stoat control",
                       labels = c("Eglinton", "Hollyford"),
                       values = c("darkgoldenrod","black")) +
   scale_fill_manual(name = "Stoat control",
                     labels = c("Yes", "No"),
                     values = c("darkgoldenrod","black")) +
  xlab(expression(paste("Intake rate","("," ", S[jt-1],")"))) +
  
  ylab(expression(atop(paste("Rate"," ", "of"," ", 
                             "increase"),paste(" ", "of"," ",
                                               "mice"," ","(",r[jt],")"))) ) +
  
  # scale_x_continuous(limits = c(0, max(out.r.1$lag.N)+3),
  #                    expand = c(0,0.1), 
  #                    breaks = round(seq(min(out.r.1$lag.N), max(out.r.1$lag.N),
  #                                       by = max(out.r.1$lag.N)/6),0)) +
  # 
  # scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
  # theme_tufte() +
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

# + ggtitle("a)")
# warnings()

# sub-plot figure4
# jpeg("./Figs/H1a_plot.jpeg", 
#      width = 20, height = 15, units = 'cm', res = 400)
# 
# h1.1.final
# 
# dev.off()