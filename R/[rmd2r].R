#' --- 	
#' title: "Introduction"	
#' author: "Anthony Davidson"	
#' date: "`r Sys.Date()`"	
#' site: bookdown::bookdown_site	
#' documentclass: book	
#' output: 	
#'   bookdown::gitbook:	
#'   bookdown::word_document2:	
#'     reference_docx: template2.docx	
#' bibliography: [Beech-forests.bib, packages.bib]	
#' biblio-style: apalike	
#' link-citations: yes	
#' github-repo: davan690/beech-forest-dynamics	
#' description: "An example of using the 'bookdown' and 'rmarkdown' for manuscript and thesis construction"	
#' ---	
#' 	
#' # Introduction {#intro}	
#' 	
#' Worldwide, but particularly on islands, introduced mammalian predators can have significant impacts on native species [@towns2006]. New Zealand's (NZ) remaining native forests commonly contain four introduced mammalian predators; stoats (*Mustela erminea*; [@veale2015a]), brushtail possums (*Trichosurus vulpecula Kerr*; [@clout1984]), ship rats (*Rattus rattus*; [@innes2005] referred to as rats), and house mice (*Mus mus musculus*; [@allen2006]). In NZ forests, stoats are the top predator following their deliberate introduction in the late nineteenth century [@king2017]. Stoat control is now commonly undertaken to protect native birds that are vulnerable to predation [@white2006], in particular, hole-nesting species like mohua (*Mohoua ochrocephala* [@odonnellPredictingIncidenceMohua1996]). However, the primary food source for stoats in NZ forests are rats and mice [@white2006] and there is a concern that reducing stoat populations to protect native birds may allow rodent populations to increase (e.g [@rayner2007]). An increase in the number of rats and mice could offset the benefits of stoat control because rodents are known to consume the eggs and chicks of native birds [@allen2006], directly compete with native species for food resources such as flowers and seeds [@mcqueen2008] and predate on invertebrates [@ruscoe2012]. In this paper we address the question: "does stoat control lead to increased abundance of rodents, particularly mice, in NZ beech forests?	
#' 	
#' Studies elsewhere in the world have shown that removing or reducing the abundance of a top predator can often lead to an increase in the numbers of predators at a lower trophic level (termed mesopredator release), which in turn, can lead to unintended and often negative outcomes for native species (for a review see [@prugh2009] and for examples see [@rayner2007; @robles2002]). While mesopredator release has been widely documented elsewhere, it is unclear if stoat control in NZ forests will cause rodent populations to increase. Rodent populations in NZ forests are known to respond strongly to variation in food supply [@choquenot2000; @ruscoe2001; @blackwell2001; @blackwell2003; @ruscoe2005; @tompkins2006 @ tompkins2013; @holland2015;  @latham2017], primarily seed availability (Figure 1).	
#' 	
#' ![Expected changes in mouse populations over time in New Zealand forests (bottom panel) in response to changes in seed avaliability (top panel) during mast and non-mast years. Where the predicted response of mouse populations to stoat control. In the bottom panel, labelled arrows (A-D) show the four previously predicted outcomes of stoat removal that we have tested in this paper. A) during non-mast years when little seed is avaliable, B) at the peak of mouse abundance (during winter and spring in mast years), C) mouse populations should increase in size more rapidly in response to increased seed avaliability in mast years with stoat control than without; D) mouse populations should decline from peak abundance more slowly in mast years with stoat control than without.](./figs/fig-1-all.png)	
#' 	
#' 	
# #data file	
# source("./R/ecosystem-simulation/sim-raw-data.R", echo = FALSE)	
# 	
#file code	
# source("./analysis/Davidson_2019_Simulation.Rmd", echo = FALSE)	
# 	
# #plot	
# result.plot	
here::here("./analysis/Davidson_2019_Simulation.Rmd")	
knitr::include_graphics(path = "./figs/fig-1-all.png")	
	
cap.pred <- c("Expected changes in mouse populations over time in New Zealand forests (bottom panel) in response to changes in seed avaliability (top panel) during mast and non-mast years. Where the predicted response of mouse populations to stoat control. In the bottom panel, labelled arrows (A-D) show the four previously predicted outcomes of stoat removal that we have tested in this paper. A) during non-mast years when little seed is avaliable, B) at the peak of mouse abundance (during winter and spring in mast years), C) mouse populations should increase in size more rapidly in response to increased seed avaliability in mast years with stoat control than without; D) mouse populations should decline from peak abundance more slowly in mast years with stoat control than without.")	
#' 	
#' 	
#' 	
#' 	
knitr::include_graphics("./figs/fig-1-all.png", dpi = 1200)	
#' 	
#' 	
#' This is particularly pronounced in beech forests where between years, beech seed production is highly variable with little seed produced in most years (Figure 1: non-mast years) and occasional years of high seed production (Figure 1: mast years). Mouse populations are low in non-mast years, due to low food availability [(@choquenot2000; @king1983]. In mast years, when seed becomes abundant, mouse populations can increase rapidly following a predictable seasonal cycle. Seed begins to fall and accumulate on the forest floor in late summer allowing mouse populations to increase, with mouse populations typically remaining high through winter and into the following spring. Beech seed that is not consumed by mice and other seed predators germinates in spring to early summer, meaning this food resource disappears and mouse populations begin to decline. If the following year is a non-mast year with little seed available, mouse populations fall to low levels. It is unclear whether stoat populations can increase rapidly enough to exert sufficiently strong predation pressure to alter these food-driven population eruptions.	
#' 	
#' Previous studies have investigated the likely response of mouse populations to stoat control by modelling the outcome of interactions between stoats, mice and seed availability. @blackwell2001 made four predictions regarding the likely effects of stoat predation on mouse dynamics (see Figure 1) with a subsequent field study concluding that stoat predation should have minimal effects on the population dynamics of mice, identifying three different phases in the eruption cycle where stoats could have an effect (Figure 1). The subsequent study [@blackwell2003] specifically examined how stoat control could influence mouse populations at the peak (Prediction B), decline (**Prediction C**) or low (**Prediction A**) phases of the beech eruption cycle.	
#' 	
#' Subsequent modelling work reached similar conclusions [@tompkins2006; @tompkins2013] but identified that the response of mice to stoat control should depend on interactions with rats. Specifically, [@tompkins2013] concluded that, where rats were present, stoat control alone should allow rats to increase, which would have a suppressive effect on mouse populations through either predation or competition. In contrast, when both stoats and rats were controlled, mouse populations would increase to higher levels than in the absence of control (see Figure 1).	
#' 	
#' Our aim was to test the predictions outlined in both @blackwell2003 and @tompkins2006 using data from a large-scale field study. Specifically, we measured the abundance of mice and rats on trapping grids over six years in beech forest in two adjacent valleys, one with intensive stoat trapping and one without. In each valley we also manipulated rat densities by including trapping grids where rats were removed and compared these to grids without rat removal. This allowed us to examine if the response of mouse populations to stoat control (see Figure 1) was influenced by interactions with rats.	
