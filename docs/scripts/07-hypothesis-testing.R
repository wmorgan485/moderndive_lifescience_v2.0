## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(infer)
library(moderndive)
library(abd)
library(ggmosaic)




## -----------------------------------------------------------------------------
GreatTitMalaria


## ----eval=FALSE---------------------------------------------------------------
## ggplot(GreatTitMalaria, aes(x = treatment, fill = response)) +
##   geom_bar(position = position_fill()) +
##   labs(x = "Treatment")




## ----eval=FALSE---------------------------------------------------------------
## ggplot(GreatTitMalaria) +
##   geom_mosaic(aes(x = product(treatment),
##                   fill = response)) +
##   labs(x = "Treatment", y = "Relative frequency")












## ----eval=FALSE---------------------------------------------------------------
## ggplot(GreatTitMalaria_shuffled,
##        aes(x = shuffled_treatment, fill = response)) +
##   geom_bar() +
##   labs(x = "Shuffled treatment")



## -----------------------------------------------------------------------------
GreatTitMalaria_shuffled %>% 
  group_by(shuffled_treatment, response) %>% 
  tally() # Same as summarize(n = n())









## ----eval=FALSE---------------------------------------------------------------
## obs_diff_prop <- BirdMalaria %>%
##   specify(response ~ treatment, success = "infected") %>%
##   calculate(stat = "diff in props", order = c("male", "female"))
## obs_diff_prop


## ----echo=FALSE, eval=FALSE---------------------------------------------------
## set.seed(2019)
## tactile_permutes <- BirdMalaria %>%
##   specify(response ~ treatment, success = "infected") %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 33, type = "permute") %>%
##   calculate(stat = "diff in props", order = c("male", "female"))
## ggplot(data = tactile_permutes, aes(x = stat)) +
##   geom_histogram(binwidth = 0.05, boundary = -0.2, color = "white") +
##   geom_vline(xintercept = pull(obs_diff_prop), color = "blue", size = 2) +
##   scale_y_continuous(breaks = 0:10)














## -----------------------------------------------------------------------------
GreatTitMalaria %>% 
  specify(formula = response ~ treatment, success = "Malaria") 


## -----------------------------------------------------------------------------
GreatTitMalaria %>% 
  specify(formula = response ~ treatment, success = "Malaria") %>% 
  hypothesize(null = "independence")




## ----eval=FALSE---------------------------------------------------------------
## GreatTitMalaria_generate <- GreatTitMalaria %>%
##   specify(formula = response ~ treatment, success = "Malaria") %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute")
## nrow(GreatTitMalaria_generate)




## ----eval=FALSE---------------------------------------------------------------
## null_distribution <- GreatTitMalaria %>%
##   specify(formula = response ~ treatment, success = "Malaria") %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute") %>%
##   calculate(stat = "diff in props", order = c("Egg removal", "Control"))
## null_distribution




## -----------------------------------------------------------------------------
obs_diff_prop <- GreatTitMalaria %>% 
  specify(response ~ treatment, success = "Malaria") %>% 
  calculate(stat = "diff in props", order = c("Egg removal", "Control"))
obs_diff_prop


## ----null-distribution-infer, fig.show="hold", fig.cap="Null distribution.", fig.height=1.8----
visualize(null_distribution, bins = 10)


## ----null-distribution-infer-2, fig.cap="Shaded histogram to show $p$-value."----
visualize(null_distribution, bins = 10) + 
  shade_p_value(obs_stat = obs_diff_prop, direction = "right")


## -----------------------------------------------------------------------------
null_distribution %>% 
  get_p_value(obs_stat = obs_diff_prop, direction = "right")



## ----eval=FALSE---------------------------------------------------------------
## null_distribution <- GreatTitMalaria %>%
##   specify(formula = response ~ treatment, success = "Malaria") %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute") %>%
##   calculate(stat = "diff in props", order = c("Egg removal", "Control"))


## ----eval=FALSE---------------------------------------------------------------
## bootstrap_distribution <- GreatTitMalaria %>%
##   specify(formula = response ~ treatment, success = "Malaria") %>%
##   # Change 1 - Remove hypothesize():
##   # hypothesize(null = "independence") %>%
##   # Change 2 - Switch type from "permute" to "bootstrap":
##   generate(reps = 1000, type = "bootstrap") %>%
##   calculate(stat = "diff in props", order = c("Egg removal", "Control"))




## -----------------------------------------------------------------------------
percentile_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci


## ----eval=FALSE---------------------------------------------------------------
## visualize(bootstrap_distribution) +
##   shade_confidence_interval(endpoints = percentile_ci)



## -----------------------------------------------------------------------------
se_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "se", 
                          point_estimate = obs_diff_prop)
se_ci


## ----eval=FALSE---------------------------------------------------------------
## visualize(bootstrap_distribution) +
##   shade_confidence_interval(endpoints = se_ci)





## ----eval=FALSE---------------------------------------------------------------
## library(moderndive)
## library(infer)
## null_distribution_cean <- GreatTitMalaria %>%
##   specify(formula = response ~ treatment, success = "Malaria") %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute") %>%
##   calculate(stat = "diff in means", order = c("Egg removal", "Control"))






















## -----------------------------------------------------------------------------
HornedLizards %>% tibble()




## ----killed-living-boxplot, fig.cap="Boxplot of squamosal horn length vs. group.", fig.height=2.7----
ggplot(data = HornedLizards, aes(x = group, y = horn.length)) +
  geom_boxplot() +
  labs(y = "Squamosal horn length")


## -----------------------------------------------------------------------------
HornedLizards <- HornedLizards %>% tidyr::drop_na()


## -----------------------------------------------------------------------------
HornedLizards %>% 
  group_by(group) %>% 
  summarize(n = n(), mean_length = mean(horn.length), std_dev = sd(horn.length))





## -----------------------------------------------------------------------------
HornedLizards %>% 
  specify(formula = horn.length ~ group)


## -----------------------------------------------------------------------------
HornedLizards %>% 
  specify(formula = horn.length ~ group) %>% 
  hypothesize(null = "independence")


## ----eval=FALSE---------------------------------------------------------------
## HornedLizards %>%
##   specify(formula = horn.length ~ group) %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute") %>%
##   View()




## ----eval=FALSE---------------------------------------------------------------
## null_distribution_HornedLizards <- HornedLizards %>%
##   specify(formula = horn.length ~ group) %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute") %>%
##   calculate(stat = "diff in means", order = c("killed", "living"))
## null_distribution_HornedLizards




## -----------------------------------------------------------------------------
obs_diff_means <- HornedLizards %>% 
  specify(formula = horn.length ~ group) %>% 
  calculate(stat = "diff in means", order = c("killed", "living"))
obs_diff_means


## ----eval=FALSE---------------------------------------------------------------
## visualize(null_distribution_HornedLizards, bins = 10) +
##   shade_p_value(obs_stat = obs_diff_means, direction = "both")




## -----------------------------------------------------------------------------
null_distribution_HornedLizards %>% 
  get_p_value(obs_stat = obs_diff_means, direction = "both")

## ----echo=FALSE---------------------------------------------------------------
p_value_HornedLizards <- null_distribution_HornedLizards %>%
  get_p_value(obs_stat = obs_diff_means, direction = "both") %>%
  mutate(p_value = round(p_value, 3))










## -----------------------------------------------------------------------------
HornedLizards %>% 
  group_by(group) %>% 
  summarize(n = n(), mean_rating = mean(horn.length), std_dev = sd(horn.length))



## ----eval=FALSE---------------------------------------------------------------
## # Construct null distribution of xbar_a - xbar_r:
## null_distribution_HornedLizards <- HornedLizards %>%
##   specify(formula = horn.length ~ group) %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute") %>%
##   calculate(stat = "diff in means", order = c("killed", "living"))
## visualize(null_distribution_HornedLizards, bins = 10)


## ----eval=FALSE---------------------------------------------------------------
## # Construct null distribution of t:
## null_distribution_HornedLizards_t <- HornedLizards %>%
##   specify(formula = horn.length ~ group) %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute") %>%
##   # Notice we switched stat from "diff in means" to "t"
##   calculate(stat = "t", order = c("killed", "living"))
## visualize(null_distribution_HornedLizards_t, bins = 10)






## ----t-stat-3, fig.cap="Null distribution using t-statistic and t-distribution.", fig.height=2.2----
visualize(null_distribution_HornedLizards_t, bins = 10, method = "both")


## -----------------------------------------------------------------------------
obs_two_sample_t <- HornedLizards %>% 
  specify(formula = horn.length ~ group) %>% 
  calculate(stat = "t", order = c("killed", "living"))
obs_two_sample_t


## ----t-stat-4, fig.cap="Null distribution using t-statistic and t-distribution with $p$-value shaded.", warning=TRUE, fig.height=1.7----
visualize(null_distribution_HornedLizards_t, method = "both") +
  shade_p_value(obs_stat = obs_two_sample_t, direction = "both")


## -----------------------------------------------------------------------------
null_distribution_HornedLizards_t %>% 
  get_p_value(obs_stat = obs_two_sample_t, direction = "both")

