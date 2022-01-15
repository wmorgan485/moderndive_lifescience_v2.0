## ---- eval=FALSE--------------------------------------------------------------
## library(tidyverse)
## library(moderndive)
## library(skimr)
## library(abd)
## library(gapminder)

## ---- echo=FALSE, message=FALSE, purl=TRUE------------------------------------
# The code presented to the reader in the chunk above is different than the code
# in this chunk that is actually run to build the book. In particular we do not
# load the skimr package.
# 
# This is because skimr v1.0.6 which we used for the book causes all
# kable() code to break for the remaining chapters in the book. v2 might
# fix these issues:
# https://github.com/moderndive/ModernDive_book/issues/271

# As a workaround for v1 of ModernDive, all skimr::skim() output in this chapter
# has been hard coded.
library(tidyverse)
library(moderndive)
# library(skimr)
library(abd)
library(gapminder)






## -----------------------------------------------------------------------------
LionNoses


## -----------------------------------------------------------------------------
glimpse(LionNoses)


## ---- eval=FALSE--------------------------------------------------------------
## LionNoses %>%
##   sample_n(size = 5)



## -----------------------------------------------------------------------------
LionNoses %>%
  summarize(mean_age = mean(age), mean_proportion.black = mean(proportion.black),
            median_age = median(age), median_proportion.black = median(proportion.black))


## ----eval=FALSE---------------------------------------------------------------
## LionNoses %>% skim()




## -----------------------------------------------------------------------------
LionNoses %>% 
  get_correlation(formula = proportion.black ~ age)


## ---- eval=FALSE--------------------------------------------------------------
## LionNoses %>%
##   summarize(correlation = cor(proportion.black, age))






## ----numxplot3, fig.cap="Regression line.", message=FALSE---------------------
ggplot(LionNoses, aes(x = age, y = proportion.black)) +
  geom_point() +
  labs(x = "Age (years)", 
       y = "Proportion of black nose",
       title = "Scatterplot of relationship of relative coloration and age of male lions.") +  
  geom_smooth(method = "lm", se = FALSE)






## ---- eval=FALSE--------------------------------------------------------------
## # Fit regression model:
## lion_model <- lm(proportion.black ~ age, data = LionNoses)
## # Get regression table:
## get_regression_table(lion_model)




## ---- eval=FALSE--------------------------------------------------------------
## # Fit regression model:
## lion_model <- lm(proportion.black ~ age, data = LionNoses)
## # Get regression table:
## get_regression_table(lion_model)












## ---- eval=FALSE--------------------------------------------------------------
## regression_points <- get_regression_points(lion_model)
## regression_points










## ---- message=FALSE-----------------------------------------------------------
library(gapminder)
gapminder2007 <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, continent, gdpPercap)




## -----------------------------------------------------------------------------
glimpse(gapminder2007)


## ---- eval=FALSE--------------------------------------------------------------
## gapminder2007 %>% sample_n(size = 5)



## ----eval=FALSE---------------------------------------------------------------
## gapminder2007 %>%
##   select(lifeExp, continent) %>%
##   skim()




## ----lifeExp2007hist, echo=TRUE, fig.cap="Histogram of life expectancy in 2007.", fig.height=5.2----
ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies")


## ----eval=FALSE---------------------------------------------------------------
## ggplot(gapminder2007, aes(x = lifeExp)) +
##   geom_histogram(binwidth = 5, color = "white") +
##   labs(x = "Life expectancy",
##        y = "Number of countries",
##        title = "Histogram of distribution of worldwide life expectancies") +
##   facet_wrap(~ continent, nrow = 2)




## ----catxplot1, fig.cap="Life expectancy in 2007.", fig.height=3.4------------
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")


## ---- eval=TRUE---------------------------------------------------------------
lifeExp_by_continent <- gapminder2007 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp), 
            mean = mean(lifeExp))











## ---- eval=FALSE--------------------------------------------------------------
## lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)
## get_regression_table(lifeExp_model)








## ---- eval=FALSE--------------------------------------------------------------
## regression_points <- get_regression_points(lifeExp_model, ID = "country")
## regression_points















## -----------------------------------------------------------------------------
# Fit regression model:
lion_model <- lm(proportion.black ~ age, 
                  data = LionNoses)

# Get regression points:
regression_points <- get_regression_points(lion_model)
regression_points
# Compute sum of squared residuals
regression_points %>%
  mutate(squared_residuals = residual^2) %>%
  summarize(sum_of_squared_residuals = sum(squared_residuals))








## ---- eval=FALSE--------------------------------------------------------------
## # Fit regression model:
## lion_model <- lm(formula = proportion.black ~ age, data = LionNoses)
## # Get regression table:
## get_regression_table(lion_model)




## ---- eval=FALSE--------------------------------------------------------------
## library(broom)
## library(janitor)
## lion_model %>%
##   tidy(conf.int = TRUE) %>%
##   mutate_if(is.numeric, round, digits = 3) %>%
##   clean_names() %>%
##   rename(lower_ci = conf_low, upper_ci = conf_high)



## ---- eval=FALSE--------------------------------------------------------------
## library(broom)
## library(janitor)
## lion_model %>%
##   augment() %>%
##   mutate_if(is.numeric, round, digits = 3) %>%
##   clean_names() %>%
##   select(-c("std_resid", "hat", "sigma", "cooksd", "std_resid"))

