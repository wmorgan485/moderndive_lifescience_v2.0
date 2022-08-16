## ---- eval=FALSE--------------------------------------------------------------
## library(ggplot2)






## ----message=FALSE------------------------------------------------------------
library(rfishbase)
library(dplyr)
library(knitr)
library(readr)


## ----eval=FALSE---------------------------------------------------------------
## fishbase <- load_taxa()




## ----eval=FALSE---------------------------------------------------------------
## all_fishdata <- species()
## all_fishdata








## -----------------------------------------------------------------------------
glimpse(all_fishdata)






## ----eval=FALSE---------------------------------------------------------------
## all_fishdata$Species






## ----eval=FALSE---------------------------------------------------------------
## ?species






## ----message=FALSE, eval=FALSE------------------------------------------------
## blackbird <- read_csv("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter12/chap12e2BlackbirdTestosterone.csv")

