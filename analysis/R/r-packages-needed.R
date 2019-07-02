# file of R library dependancies
# Davidson 2019
# 1/03/2019

# install packages guide
# lib.loc="C:/Program Files/R/R-3.5.2/library"

# libraries
library(deSolve)
library(lubridate)
library(jagsUI)
library(cowplot)
library(Matrix)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(coda)
library(tidyverse)
library(kableExtra)
library(reshape2)
library(knitr)
library(knitcitations)
library(jtools)
library(flextable)
library(rmarkdown)
library(tinytex)
library(citr)
# library(rbbt)
library(stringr)

# sorting out fonss
# marh 2019

library(extrafont)

# add font to database if needed
# font_import()

# sorting fonts
windowsFonts()
loadfonts(device = "win")

# this just simply reduces long name to "Times" as in code below
windowsFonts(Times = windowsFont("TT Times New Roman"))


###################### Old work #####################################
# installing new packages
# install.packages("ggplot2")
# install.packages("plyr")
# install.packages("gapminder")
# installed.packages("lubridate")
# install.packages("cowplot")
# install.packages("jagsUI")
# install.packages("tidyverse")
# install.packages("Matrix")
# install.packages("ggthemes")
# install.packages("gridExtra")
# install.packages("coda")

# tidybayes
# library(tidybayes)
# library(magrittr)
# library(dplyr)
# library(forcats)
# library(ggplot2)
# library(ggstance)
# library(emmeans)
# library(broom)
# library(lme4)
# library(rstan)
# library(rstanarm)
# library(brms)
# library(modelr)
# library(bayesplot)
# library(MCMCglmm)
# library(tidybayes)
# library(cowplot)
# library(RColorBrewer)
# library(gganimate)