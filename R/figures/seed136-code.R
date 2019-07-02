# Rate of increase vs seed
# May to August - rate of change
# May mice abundace, seed and rat data

# import data
out.r.1 <- out.r <- dat.pred.log
# out.r.1 <- filter(out.r, month == "Aug") %>%
#             droplevels()

# --------------------- Seed model = Average all but predicted ---------------

names(out.r.1)

var.select <- c("trip","grid","grid.id","mean.r","se.r", "lcl.r", "ucl.r",  "var",   "grid.n","valley", "min_ind","lag.N", "lag.rat.mna" , "controlT",  "month", "year",  "total","log.cum.seed", "lag.S.L" , "lag.S", "b0","b.seed","b.dens","b.rat", "pred")

model <- out.r.1 %>%
  select(var.select) %>%
  mutate(
    trip = as.numeric(trip),
    grid = factor(grid),
    month = factor(month, levels = c("Feb","May","Aug","Nov")),
         valley = factor(valley, levels = c("egl", "hol")),
         controlT = factor(controlT),
         ctrl = ifelse(valley == "hol" & trip > 12, "control", "no control"),
         ctrl = ifelse(valley == "egl", "control", ctrl),
         Conditions = as.factor(paste(ctrl, valley)))

table(model$var)


pred.lines.1 <- model %>%
               # select()
  # filter(month == "Aug") %>%
  droplevels() %>%
  group_by(Conditions, month) %>%
  summarise(b0 = mean(b0),
            b.seed = mean(b.seed),
            b.dens = mean(b.dens),
            b.rat = mean(b.rat),
            se.r = mean(se.r),
            
            M.seed = mean(log.cum.seed),
            M.dens = mean(lag.N),
            M.rat = mean(lag.rat.mna),
            
            MAX.rat = max(lag.rat.mna),             
            MAX.seed = max(log.cum.seed),
            MAX.dens = max(lag.N),
            MAX.r = max(mean.r),
            
            min.seed = min(log.cum.seed),
            min.dens = min(lag.N),
            min.rat = min(lag.rat.mna),
            min.r = min(mean.r),
            
            min.pt = b0 + (b.seed*min.seed) + (b.dens*M.dens) + (b.rat*M.rat), 
            max.pt = b0 + (b.seed*MAX.seed) + (b.dens*M.dens) + (b.rat*M.rat)) %>% 
  ungroup()

head(pred.lines.1)
# error in 
# control e~ NA,NA,NA  NA   NA  0.283 NA,107.   3.25   7    NA 146.   0.760


# Seed lines (12x) --------------------------------------------------------

# month.overall <- "Aug"
#  month.overall <- unique(model$month)
#  data = filter(model, month == month.overall)

# levels(model$month)

pred.lines.s2 <- pred.lines.1 %>%
  select(Conditions, month, min.r, MAX.r, min.seed, MAX.seed) %>%
  droplevels() %>%
  gather(value = mean.r, key = pt.lines, min.r:MAX.seed)


glimpse(pred.lines.s2)
str(pred.lines.1$month)
str(pred.lines.s2$month)
str(pred.lines.s2$month)
str(pred.lines.s2$month)

# table(pred.lines.s2.1$pt.lines)

pred.lines.s2.1 <- filter(pred.lines.s2, pt.lines == "MAX.seed" | pt.lines == "min.seed") %>%
  droplevels() %>%
  select(mean.r, Conditions) %>%
  transmute(log.cum.seed = mean.r,
            Conditions = Conditions)

log.cum.seed <- pred.lines.s2.1$log.cum.seed

pred.lines.s3 <- cbind(pred.lines.s2[1:24,], log.cum.seed)

glimpse(pred.lines.s3)

pred.lines.s4 <- pred.lines.s3 %>%
  mutate(pt.lines = factor(pt.lines))

pred.lines.s5 <- pred.lines.s4
# month.overall <- "Aug"
#  month.overall <- unique(model$month)
#  data = filter(model, month == month.overall)

# levels(model$month)







p1 <- 
  
  ggplot(model, aes(y = mean.r, x = log.cum.seed)) +
  geom_point(aes(col = Conditions, fill = Conditions, shape = valley), stroke = 1.1, size = 3) +
  geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), lwd = 0.75, alpha = 0.1, position=position_dodge(width=30), width = 0) +  
  geom_point(data = pred.lines.s5, aes(y = mean.r, x = log.cum.seed), size = 3, shape = "square") +
  geom_line(data = pred.lines.s5, aes(y = mean.r, x = log.cum.seed, group = Conditions), size = 1, shape = "square")+
  scale_alpha_manual(values = c(0.7,1,0.7)) + 
  
  scale_shape_manual(name = "Valley",
                     labels = c("E", "H"),
                     values = c(25,21)) +
  
  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +
  facet_wrap(~month) + 
  
  # scale_colour_manual(name = "Stoat control",
  #                     labels = c("E-", "H+", "H-"),
  #                     values = c("black","black", "black")) +
  # scale_shape_manual(name = "Valley",
  #                    labels = c("E", "H"), 
  #                    values = c(25,21)) +
  # scale_fill_manual(name = "Stoat control",
  #                   labels = c("E-", "H+", "H-"), 
  #                   values = c("white","black", "white")) +
  
xlab(expression(paste("Log","(", "seed"," ",italic(m^2),")"))) +
  
  ylab(expression(atop(paste("Rate"," ", "of"," ", 
                             "increase"),paste(" ", "of"," ",
                                               "mice"," ","(",italic(r[jt]),")"))) ) +
  scale_y_continuous(expand = c(0,0.01),breaks = seq(-4,4,1)) +
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
















# names(pred.lines.1)
# 
# table(pred.lines.1$month)
# str(pred.lines.1$MAX.rat)

# pred.lines.2 <- pred.lines.1 %>%
#                   select(Conditions, month, min.r, MAX.r, min.seed, MAX.seed) %>%
#                     gather(value = mean.r, key = pt.lines, min.r:MAX.seed)
# 
# pred.lines.2.1 <- filter(pred.lines.2, pt.lines == "MAX.seed" | pt.lines == "min.seed") %>%
#   select(mean.r, Conditions) %>%
#     transmute(log.cum.seed = mean.r,
#               Conditions = Conditions)
#   
#   
# pred.lines.3 <- bind_cols(pred.lines.2[1:24,], pred.lines.2.1)
# 
# pred.lines.4 <- pred.lines.3 %>%
#                   bind_rows(pred.lines.3, pred.lines.3)
# 
# pred.lines.5 <- pred.lines.4 %>% 
#   mutate(pt.lines = factor(pt.lines)) 
# 
