## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(carData)
library(fivethirtyeight)




## ----message=FALSE, eval=FALSE------------------------------------------------
## library(readr)
## blackbird <- read_csv("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter12/chap12e2BlackbirdTestosterone.csv")








## -----------------------------------------------------------------------------
drinks_smaller <- drinks %>% 
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia")) %>% 
  select(-total_litres_of_pure_alcohol) %>% 
  rename(beer = beer_servings, spirit = spirit_servings, wine = wine_servings)
drinks_smaller




















## -----------------------------------------------------------------------------
drinks_smaller


## -----------------------------------------------------------------------------
drinks_smaller_tidy <- drinks_smaller %>% 
  pivot_longer(names_to = "type", 
               values_to = "servings", 
               cols = -country)
drinks_smaller_tidy


## ---- eval=FALSE--------------------------------------------------------------
## drinks_smaller %>%
##   pivot_longer(names_to = "type",
##                values_to = "servings",
##                cols = c(beer, spirit, wine))


## ---- eval=FALSE--------------------------------------------------------------
## drinks_smaller %>%
##   pivot_longer(names_to = "type",
##                values_to = "servings",
##                cols = beer:wine)


## ----eval=FALSE---------------------------------------------------------------
## ggplot(drinks_smaller_tidy, aes(x = country, y = servings, fill = type)) +
##   geom_col(position = "dodge")








## -----------------------------------------------------------------------------
blackbird_smaller <- blackbird %>% 
  select(!starts_with("log"))
blackbird_smaller




## -----------------------------------------------------------------------------
ControlGroup <- WeightLoss %>% 
  group_by(group) %>% 
  summarise(mean_wl1 = mean(wl1), 
            mean_wl2 = mean(wl2), 
            mean_wl3 = mean(wl3)) %>% 
  filter(group == "Control") 
ControlGroup


## -----------------------------------------------------------------------------
ControlGroup_tidy <- ControlGroup %>% 
  rename(`1` = mean_wl1, `2` = mean_wl2, `3` = mean_wl3) %>% 
  pivot_longer(names_to = "month", 
               values_to = "weightloss", 
               cols = -group,
               names_transform = list(month = as.integer)) 
ControlGroup_tidy


## ----guat-dem-tidy, fig.cap="Weight loss scores of control group.", fig.height=3----
ggplot(ControlGroup_tidy, aes(x = month, y = weightloss)) +
  geom_line() +
  labs(x = "Month", y = "Weight Loss Score")

