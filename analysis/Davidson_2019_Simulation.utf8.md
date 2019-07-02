---
title: "Beech forest dynamics"
subtitle: "A simulation"
author: "Anthony Davidson"
date: '2019-05-16'
output:
  word_document: 
    reference_docx: 
      template.docx
bibliography: Beech-forests.bib
---



This simple simulation model generates Figure 1 of this publication. This figure explains how I expect population dynamics to 'deterministically work'play-out" given our current understanding of Beech forest dynamics [@choquenot2000; @ruscoe2001; @blackwell2001; @blackwell2003; @ruscoe2005; @tompkins2006 @ tompkins2013; @holland2015;  @latham2017], primarily seed availability (Figure 1).

## Setup



# Overview



New Zealand beech forests exhibit boom-bust dynamics orginally (after many edits looks like this vignette and Figure 1 below.

![Each arrow and label represents a prediction we tested (Prediction B to D). Each prediction represents a collection of previous studies that have suggested how mouse populations may respond to seed availability in the presence or absence of stoats. A) during the years when no seed is available (non-mast years; Panel A); B) at the peak of mouse abundance (winter or spring); C) the season when mice populations are responding rapidly to increasing seed abundance (summer to winter in mast years; Panel B); D) when mouse abundance declines (spring to summer; Panel B).  The top panel represents the average seed availability cycle in New Zealand Native Beech Forests between non-mast (no shading) and mast years (shaded grey). The bottom panel represents the expected response of mouse abundance ($N_{j,t}$) to the variation in seed availability ($Seed_{j,t}) above where solid yellow symbols represent locations where stoats are un-controlled](../figs/unnamed-chunk-52-1.png)

Where, beech trees mast in spatial synchronised but annually variable years dependant [@wardle1991]. Mice populations have invaded these systems and studies have shown that populations response numerically to changes in resources (beech seed) and mice have been modelled under a range of both functional and numerical responses [@king1983]. 

# Introduction

I have made a very simple simulation of the expected relationships from the literature. *For more detail see thesis drafts [here](https://www.ssnhub.com/phd-thesis/).*

## Season



## Mouse Abundance


```r
#Abundance
no.stoats <- c(25,15,25,15,10,160,200,200,25,15,25,20)
stoats <- c(12,10,12,5,7,75,100,100,10,3,5,4)
```

## Beech Seed 


```r
#seed
beech.seed <- c(0,0,0,0,0,rnorm(1,1550,1),rnorm(1,1800,1),rnorm(1,2000,1),0,0,0,0)
lcl.seed <- c(0,0,0,0,rnorm(1,10,2),rnorm(1,1000,50),rnorm(1,3000,100),0,0,0,0,0)
ucl.seed <- c(0,0,0,0,rnorm(1,190,10),rnorm(1,3000,50),rnorm(1,5000,100),0,0,0,0,0)
stata <- seq(1,12,1)
```

## Stoat control


```r
#control <- as.factor(c(rep(c("no.stoats"),4),rep(c("stoats"),4)))
control <- factor(rep(1,12))
# kable(table(control))
```

## Date


```r
#date
date <- as.Date(as.character(c("1999-02-01","1999-05-01","1999-08-01","1999-11-01",
                               "2000-02-01","2000-05-01","2000-08-01","2000-11-01",
                               "2001-02-01","2001-05-01","2001-08-01","2001-11-01")))
```

### Labelling


```r
#date
date <- as.Date(as.character(c("1999-02-01","1999-05-01","1999-08-01","1999-11-01",
                               "2000-02-01","2000-05-01","2000-08-01","2000-11-01",
                               "2001-02-01","2001-05-01","2001-08-01","2001-11-01")))
```

## Create dataset

It is always nice to export csv data with meaningful labels.

### Labelling


```r
labels1 <-  c("Summer", "Autumn", "Winter", "Spring", "Summer", "Autumn", "Winter", "Spring")

labels2 <-  c("", "", "Non-mast year", "", "", "", "Mast year", "")
```

## Build dataset



### Save data



# Results

### Plot data



### Plot symbols/labels


```r
# build points data
# tibble my life
arrow.length <- 40
touchoff.distance <- 10 # distance between data and start of arrow
arrowhead.size <- 3 # in millimeters
time.loc <- as.character()

# "1999-09-31", "2000-05-31", "2000-07-31", "2000-12-31"

points.dat <- tibble(
  prediction = as.factor(c("A", "C", "B", "D")),
  value = as.numeric(c(15, 88, 108, 60)),
  date = as.Date(c("1999-08-20", "2000-05-01", "2000-09-13", "2000-12-25")))

# c("1999-02-01","1999-05-01","1999-08-01","1999-11-01","2000-02-01","2000-05-01","2000-08-01","2000-11-01", "2001-02-01","2001-05-01","2001-08-01","2001-11-01")
```

### Seed plot

![](../figs/mod-code-seed-new-1.png)<!-- -->![](../figs/mod-code-seed-new-2.png)<!-- -->

### Mouse plot

![](../figs/unnamed-chunk-61-1.png)<!-- -->

```
png 
  2 
```

### Joint plot

![](../figs/unnamed-chunk-62-1.png)<!-- -->

```
png 
  2 
```

#### Old plot

![](../figs/orginal-plot-1.png)<!-- -->

```
png 
  2 
```



# Discussion

### Saving


```
png 
  2 
```

# Appendix 1
## Old code 



# References


