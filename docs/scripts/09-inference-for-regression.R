## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(moderndive)
library(infer)
library(abd)




## -----------------------------------------------------------------------------
glimpse(LionNoses)



## ----regline, fig.cap="Relationship with regression line.", message=FALSE-----
ggplot(LionNoses, 
       aes(x = age, y = proportion.black)) +
  geom_point() +
  labs(x = "Age (years)", 
       y = "Proportion of black nose",
       title = "Scatterplot of relationship of relative coloration and age of male lions.") +  
  geom_smooth(method = "lm", se = FALSE)


## ----eval=FALSE---------------------------------------------------------------
## # Fit regression model:
## lion_model <- lm(proportion.black ~ age, data = LionNoses)
## # Get regression table:
## get_regression_table(lion_model)















## -----------------------------------------------------------------------------
# Fit regression model:
lion_model <- lm(proportion.black ~ age, data = LionNoses)
# Get regression points:
regression_points <- get_regression_points(lion_model)
regression_points




## ----eval=FALSE---------------------------------------------------------------
## ggplot(regression_points, aes(x = residual)) +
##   geom_histogram(binwidth = 0.05, color = "white") +
##   labs(x = "Residual")





## ----eval=FALSE---------------------------------------------------------------
## ggplot(regression_points, aes(x = age, y = residual)) +
##   geom_point() +
##   labs(x = "Age (years)", y = "Residual") +
##   geom_hline(yintercept = 0, col = "blue", size = 1)









## ----eval=FALSE---------------------------------------------------------------
## bootstrap_distn_slope <- LionNoses %>%
##   specify(formula = proportion.black ~ age) %>%
##   generate(reps = 1000, type = "bootstrap") %>%
##   calculate(stat = "slope")
## bootstrap_distn_slope



## ----bootstrap-distribution-slope, fig.show="hold", fig.cap="Bootstrap distribution of slope.", fig.height=2.2----
visualize(bootstrap_distn_slope)


## -----------------------------------------------------------------------------
percentile_ci <- bootstrap_distn_slope %>% 
  get_confidence_interval(type = "percentile", level = 0.95)
percentile_ci


## -----------------------------------------------------------------------------
observed_slope <- LionNoses %>% 
  specify(proportion.black ~ age) %>% 
  calculate(stat = "slope")
observed_slope


## -----------------------------------------------------------------------------
se_ci <- bootstrap_distn_slope %>% 
  get_ci(level = 0.95, type = "se", point_estimate = observed_slope)
se_ci


## ----eval=FALSE---------------------------------------------------------------
## visualize(bootstrap_distn_slope) +
##   shade_confidence_interval(endpoints = percentile_ci, fill = NULL,
##                             linetype = "solid", color = "grey90") +
##   shade_confidence_interval(endpoints = se_ci, fill = NULL,
##                             linetype = "dashed", color = "grey60") +
##   shade_confidence_interval(endpoints = c(0.035, 0.099), fill = NULL,
##                             linetype = "dotted", color = "black")




## ----eval=FALSE---------------------------------------------------------------
## null_distn_slope <- LionNoses %>%
##   specify(proportion.black ~ age) %>%
##   hypothesize(null = "independence") %>%
##   generate(reps = 1000, type = "permute") %>%
##   calculate(stat = "slope")







## -----------------------------------------------------------------------------
null_distn_slope %>% 
  get_p_value(obs_stat = observed_slope, direction = "both")

