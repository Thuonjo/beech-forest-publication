# Rate of increase between Autumn and Winter
# (May and August rate of change)
# May mice abundace, seed and rat data
# Overall Data

# data --------------------------------------------------------------------
# reduced data
# out.pC.1 <- filter(out.final.2, month == "Nov")

#main data from manuscript main file
# but needs valley, control and Conditions
out.final.3 <- out.full.136 %>%
  mutate(mean.r = as.numeric(mean.r),
    lag.sjt = as.numeric(lag.sjt),
    valley = factor(valley),
    control = factor(ifelse(controlT == "holN", "No", "Yes")),
    Conditions = factor(
      ifelse(grid.id == "M1" | grid.id == "M2",
             "rats.removed", "rats.present")
    ),
    lcl.r = as.numeric(lcl.r),
    ucl.r = as.numeric(ucl.r),
    month = factor(month, levels = c("Feb", "May", "Aug", "Nov")))


# glimpse(out.final.3$month)
# glimpse(out.final.3$controlT)
# names(out.final.2)
# levels(out.final.3$month)
# out.final.3$month
# levels(out.final.3$month)

# used to be
# out.r <- read_csv("C://Code/data/CR_output_pred.csv")

# lines summary -----------------------------------------------------------

pred.lines.seed <- out.final.3 %>%
  drop_na() %>%
  # group_by(valley, control, Conditions, month) %>%
  group_by(controlT, month) %>%
  summarise(
    b0 = mean(b0),
    b.seed = mean(b.seed),
    b.dens = mean(b.dens),
    b.rat = mean(b.rat),
    se.r = mean(se.r),
    
    M.seed = mean(lag.sjt),
    M.dens = mean(lag.N),
    M.rat = mean(lag.rat.mna),
    
    MAX.rat = max(lag.rat.mna),
    MAX.seed = max(lag.sjt),
    MAX.dens = max(lag.N),
    MAX.r = max(mean.r),
    
    min.seed = min(lag.sjt),
    min.dens = min(lag.N),
    min.rat = min(lag.rat.mna),
    min.r = min(mean.r),
    
    min.pt = b0 + (b.seed * min.seed),
    max.pt = b0 + (b.seed * MAX.seed)
  ) %>%
  ungroup()

# %>%
#   mutate(month = factor(as.character(month),
#                         levels = c("Feb","May","Aug","Nov")))
pred.lines.1 <- pred.lines.seed
# table(pred.lines.s5$month)
# str(pred.lines.s5$month)
# str(out.final.3$month)
# # lines data for seed -----------------------------------------------------
# Seed lines (12x) --------------------------------------------------------

pred.lines.s2 <- pred.lines.seed %>%
  select(month,controlT, min.r, MAX.r, min.seed, MAX.seed) %>%
  droplevels() %>%
  gather(value = mean.r, key = pt.lines, min.r:MAX.seed)



pred.lines.s2.1 <-  filter(pred.lines.s2, pt.lines == "MAX.seed" |
           pt.lines == "min.seed") %>%
  droplevels() %>%
  mutate(log.cum.seed = mean.r) %>%
  drop_na()

log.cum.seed <- pred.lines.s2.1$log.cum.seed
# controlT <- pred.lines.s2.1$controlT

pred.lines.s3 <- cbind(pred.lines.s2[1:24, ], log.cum.seed)

pred.lines.s4 <- pred.lines.s3 %>%
  mutate(pt.lines = factor(pt.lines)) %>%
  drop_na()

pred.lines.s5 <- pred.lines.s4 %>%
  mutate(lag.sjt = log.cum.seed,
    valley = factor(rep(c("egl", "hol", "hol"), 2, each = 4)),
    control = factor(rep(c("Yes", "Yes", "No"), 2, each = 4))) %>%
  drop_na()

# glimpse(pred.lines.1)
####FUKED!!!!!!!!!!!!!###########
# factor sort!
month.refactor <- factor(as.numeric(pred.lines.s5$month))
# ?recode_factor

pred.lines.s5$month <- recode(month.refactor, "1" = "May", "2" = "Nov", "3" = "Feb", "4" = "Aug")


# lines data for density --------------------------------------------------
pred.lines.dens <- out.final.3 %>%
  drop_na() %>%
  # group_by(valley, control, Conditions, month) %>%
  group_by(controlT, month) %>%
  summarise(
    b0 = mean(b0),
    b.seed = mean(b.seed),
    b.dens = mean(b.dens),
    b.rat = mean(b.rat),
    se.r = mean(se.r),
    
    M.seed = mean(lag.sjt),
    M.dens = mean(lag.N),
    M.rat = mean(lag.rat.mna),
    
    MAX.rat = max(lag.rat.mna),
    MAX.seed = max(lag.sjt),
    MAX.dens = max(lag.N),
    MAX.r = max(mean.r),
    
    min.seed = min(lag.sjt),
    min.dens = min(lag.N),
    min.rat = min(lag.rat.mna),
    min.r = min(mean.r),
    
    min.pt = b0 + (b.dens * min.dens),
    max.pt = b0 + (b.dens * MAX.dens)
  ) %>%
  ungroup()

pred.lines.d2 <- pred.lines.dens %>%
  select(month,controlT, min.r, MAX.r, min.dens, MAX.dens) %>%
  droplevels() %>%
  gather(value = mean.r, key = pt.lines, min.r:MAX.dens)



pred.lines.d2.1 <-  filter(pred.lines.d2, pt.lines == "MAX.dens" |
                             pt.lines == "min.dens") %>%
  droplevels() %>%
  mutate(lag.N = mean.r) %>%
  drop_na()

lag.N <- pred.lines.d2.1$lag.N
# controlT <- pred.lines.s2.1$controlT

pred.lines.d3 <- cbind(pred.lines.d2[1:24, ], lag.N)

pred.lines.d4 <- pred.lines.d3 %>%
  mutate(pt.lines = factor(pt.lines)) %>%
  drop_na()

pred.lines.d5 <- pred.lines.d4 %>%
  mutate(lag.N = lag.N,
         valley = factor(rep(c("egl", "hol", "hol"), 2, each = 4)),
         control = factor(rep(c("Yes", "Yes", "No"), 2, each = 4))) %>%
  drop_na()
