######################################
#how to estimate seeds
# 0.049087385
# Bucket radius - 0.125m
# 1. Bucket area = 0.049087385m2
# 4 buckets per grid = total catch area = 0.19635 m2
# So seeds /m2  =# Seeds counted/0.1963     ...  so if 800 seeds were counted on a grid, this will equate to 4000 /m2.

#import data
# read in the seed data
seed <- read.csv("C://Code/data/seed_data_jan2017.csv", strip.white = T)
# glimpse(seed)

#month levels to get it right
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# use seed in the previous quarter to predict rate of increase
# so add one to trip to align with r
n.seed <- mutate(seed, mountain = ifelse(is.na(mountain), 0, mountain),
                 red = ifelse(is.na(red), 0, red),
                 silver = ifelse(is.na(silver), 0, silver),
                 total = red + silver + mountain,
                 valley = ifelse(valley == "E", "egl", "hol"),
                 grid = paste(valley, grid.id),
                 seedm2 = total/0.19635,
                 logseed = log(seedm2)) %>%
  select(valley,red,silver,seedm2, grid.id, grid, trip.no, year, month, total) %>%
  filter(trip.no > 0)

# exp(log10(6))
# names(n.seed)

# for each year calculate the cumulative number of seeds
n.seed.1 <- n.seed %>%
            group_by(grid, year) %>%
              mutate(cum.seed = cumsum(seedm2)) %>%
                filter(trip.no > 0) %>%
                  # arrange(grid, trip.no) %>%
                    subset(!(grid.id %in% c("R1", "R2")))

#these are trips that seed was collected but trapping was not done on these grids
a <- bind_rows(subset(n.seed.1, grid == "egl M2" & trip.no == "13"),
               subset(n.seed.1, grid == "egl M2" & trip.no == "14"),
               subset(n.seed.1, grid == "hol M1" & trip.no == "13"))

n.seed.2 <- anti_join(n.seed.1,a) %>%
              mutate(trip = trip.no,
                     month = factor(month, month_levels),
                     log.cum.seed = log10(cum.seed + 1))

# old skol
# n.seed.1$month <- factor(n.seed.1$month, levels = c("Feb", "May", "Aug", "Nov"))
# colnames(n.seed.1)[2] <- "trip"
# factor(n.seed.1$trip)
# names(n.seed.1)

#need to reduce dataset to 144 not 147 reps

# n.seed.1 data variations--------------------------------------------------------------------

#model1
#total
total <- tapply(n.seed.2$total, list(n.seed.2$trip.no, n.seed.2$grid), mean) + 1

#model2
seedm2 <- tapply(n.seed.2$seedm2, list(n.seed.2$trip.no, n.seed.2$grid), mean) + 1

#model3
#cum.seed
cum.seed <- tapply(n.seed.2$cum.seed, list(n.seed.2$trip.no, n.seed.2$grid), mean) +1

#model4
# log.cum.seed
log.cum.seed <- log10(tapply(n.seed.2$cum.seed, list(n.seed.2$trip.no, n.seed.2$grid), mean) + 1)

#model5 - totalIR
IR <- rep(NA,length(n.seed.2$seedm2))
for(i in 1:length(n.seed.2$seedm2)) {IR[i] <- 1042.1 * (1 - exp(-(n.seed.2$seedm2[i] * 0.00139)))}
IR_tseed <- tapply(IR, list(n.seed.2$trip.no, n.seed.2$grid), mean)

# sorting month and valley vectors... -------------------------------------
# trips by month for mark-recapture data
a1 <- table(ind$trip.no, factor(ind$month, levels = c("Feb", "May", "Aug", "Nov")))


# and for the seed data
a <- table(n.seed.2$trip.no, factor(n.seed.2$month, levels = c("Feb", "May", "Aug", "Nov")))
a

# this is the issue I do not understand
levels(n.seed.2$month)
labels(n.seed.2$month) 

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  
labels.month <- c(as.character(factor(n.seed.2$month, levels = month_levels)))

# factor(ind$month, levels = c("Feb", "May", "Aug", "Nov"))

month <- apply(a1, 1, function(x) which(x > 0))
valley <- c(1, 1, 1, 1, 2, 2, 2, 2)

labels <- c("egl M1" = "Grid one", 
            "egl M2" = "Grid two", 
            "egl MR1" = "Grid three",
            "egl MR2" = "Grid four",
            "hol M1" = "Grid five",
            "hol M2" = "Grid six",
            "hol MR1" = "Grid seven",
            "hol MR2" = "Grid eight")

labels2 <- c(egl = "Eglinton valley", hol = "Hollyford valley")

#export plotdata
# write.csv(n.seed.2, 'C://Code/data/seedjoining-data.csv')

# plot 1 ------------------------------------------------------------------

ggplot(n.seed.2, aes(y = cum.seed ,x = year, fill = valley)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  scale_x_continuous(breaks=seq(1999,2004,1)) +
  labs(x = "Year", y = "Total seed accumulation") +
  scale_fill_discrete(labels=c("Eglinton valley","Hollyford valley")) +
  #same color all the way through...
  #not working
  scale_color_manual(values = c("black", "blue")) +
  ggtitle ("Raw beech seed") +
  theme_bw()

#  plot2 ------------------------------------------------------------------

##data
mast <- select(n.seed.2,valley,month,year,cum.seed,grid.id,grid)
mast$month <- factor(mast$month, levels=c("Feb", "May", "Aug","Nov"))
dodge <- position_dodge(width=0)

#plot2
ggplot(mast, aes(y = cum.seed, shape = grid.id, colour = grid.id)) +
  geom_point(aes(y = cum.seed, x = month),size = 2 , position = dodge, alpha = 1) +
  geom_hline(yintercept = 0) +
  facet_grid(valley ~ year, labeller = labeller(valley = labels2)) +
  theme_bw() +
  labs(x = "Year", y = "Total seed accumulation") +
  ggtitle ("Accumulative beech seed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_hc()