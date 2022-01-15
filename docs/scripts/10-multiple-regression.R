## ---- eval=FALSE--------------------------------------------------------------
## library(tidyverse)
## library(moderndive)
## library(skimr)
## library(abd)

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
library(gapminder)




## -----------------------------------------------------------------------------
glimpse(ToothGrowth)




## ---- eval=FALSE--------------------------------------------------------------
## ToothGrowth %>% sample_n(size = 5)



## ---- eval =FALSE-------------------------------------------------------------
## ToothGrowth %>% skim()


## -----------------------------------------------------------------------------
ToothGrowth %>% 
  get_correlation(formula = len ~ dose)


## ----eval=FALSE---------------------------------------------------------------
## ggplot(ToothGrowth, aes(x = dose, y = len, color = supp)) +
##   geom_point() +
##   labs(x = "Dose", y = "Tooth length", color = "Supplement") +
##   geom_smooth(method = "lm", se = FALSE)








## ---- eval=FALSE--------------------------------------------------------------
## # Fit regression model:
## len_model_interaction <- lm(len ~ dose * supp, data = ToothGrowth)
## 
## # Get regression table:
## get_regression_table(len_model_interaction)





## ----eval=FALSE---------------------------------------------------------------
## ggplot(ToothGrowth, aes(x = dose, y = len, color = supp)) +
##   geom_point() +
##   labs(x = "Dose", y = "Tooth length", color = "Supplement type") +
##   geom_parallel_slopes(se = FALSE)




## ---- eval=FALSE--------------------------------------------------------------
## # Fit regression model:
## len_model_parallel_slopes <- lm(len ~ dose + supp, data = ToothGrowth)
## # Get regression table:
## get_regression_table(len_model_parallel_slopes)



## ----echo=FALSE---------------------------------------------------------------
dose_coef <- get_regression_table(len_model_parallel_slopes) %>%
  filter(term == "dose") %>%
  pull(estimate)










## ---- eval=FALSE--------------------------------------------------------------
## regression_points <- get_regression_points(len_model_interaction)
## regression_points %>% group_by(supp) %>% slice_head(n=5)







## ---- message=FALSE-----------------------------------------------------------
library(abd)
glimpse(IntertidalAlgae)




## ---- eval=FALSE--------------------------------------------------------------
## IntertidalAlgae %>% sample_n(size = 5)



## ---- eval=FALSE--------------------------------------------------------------
## IntertidalAlgae %>% skim()


## ---- eval=FALSE--------------------------------------------------------------
## ggplot(IntertidalAlgae, aes(x = herbivores, y = sqrt.area)) +
##   geom_boxplot() +
##   geom_jitter(width = 0.1) +
##   labs(x = "Herbivore treatment", y = "Square root of area covered",
##        title = "Area coverage and herbivore treatment")
## 
## ggplot(IntertidalAlgae, aes(x = height, y = sqrt.area)) +
##   geom_boxplot() +
##   geom_jitter(width = 0.1) +
##   labs(x = "Height relative to low tide",
##        y = "Square root of area covered",
##        title = "Area coverage and tidal location")




## ---- eval=FALSE--------------------------------------------------------------
## ggplot(IntertidalAlgae, aes(x = herbivores, y = sqrt.area,
##                             color = height, group = height)) +
##   geom_jitter(width = 0.1) +
##   labs(x = "Herbivore treatment", y = "Square root of area covered",
##        title = "Area coverage and herbivore treatment") +
##   geom_smooth(method = "lm", se = FALSE)
##   # geom_parallel_slopes(se = FALSE)








## ---- eval=FALSE--------------------------------------------------------------
## # Fit regression model:
## area_model1 <- lm(sqrt.area ~ herbivores * height, data = IntertidalAlgae)
## # Get regression table:
## get_regression_table(area_model1)







## ---- eval=FALSE--------------------------------------------------------------
## get_regression_points(area_model1) %>%
##   group_by(herbivores, height) %>% slice_head(n=2)



## ---- eval=FALSE--------------------------------------------------------------
## area_model2 <- lm(sqrt.area ~ herbivores + height, data = IntertidalAlgae)
## means_Obs <- IntertidalAlgae %>% group_by(herbivores, height) %>% summarise(mean_Obs=mean(sqrt.area))
## means_Int <- get_regression_points(area_model1) %>%
##   group_by(herbivores, height) %>%
##   summarise(mean_hat_Int = mean(sqrt.area_hat))
## means_Non <- get_regression_points(area_model2) %>%
##   group_by(herbivores, height) %>%
##   summarise(mean_hat_Non = mean(sqrt.area_hat))
## means_table <- means_Obs %>% inner_join(means_Int) %>% inner_join(means_Non)
## means_table







## ---- eval=FALSE--------------------------------------------------------------
## # Interaction model
## ggplot(MoleRats,
##        aes(x = ln.mass, y = ln.energy, color = caste)) +
##   geom_point(alpha = 0.5) +
##   geom_smooth(method = "lm", se = FALSE) +
##   labs(x = "Log body mass", y = "Log energy expenditure",
##        color = "Caste",
##        title = "Interaction model")


## ---- eval=FALSE--------------------------------------------------------------
## # Parallel slopes model
## ggplot(MoleRats,
##        aes(x = ln.mass, y = ln.energy, color = caste)) +
##   geom_point(alpha = 0.5) +
##   geom_parallel_slopes(se = FALSE) +
##   labs(x = "Log body mass", y = "Log energy expenditure",
##        color = "Caste",
##        title = "Parallel slopes model")



## ---- eval=FALSE--------------------------------------------------------------
## model_2_interaction <- lm(ln.energy ~ ln.mass * caste,
##                           data = MoleRats)
## get_regression_table(model_2_interaction)


## ---- eval=FALSE--------------------------------------------------------------
## model_2_parallel_slopes <- lm(ln.energy ~ ln.mass + caste,
##                               data = MoleRats)
## get_regression_table(model_2_parallel_slopes)



## -----------------------------------------------------------------------------
get_regression_points(model_2_interaction) 


## -----------------------------------------------------------------------------
get_regression_points(model_2_interaction) %>% 
  summarize(var_y = var(ln.energy), 
                      var_y_hat = var(ln.energy_hat), 
                      var_residual = var(residual))


## ----model2-r-squared, echo=FALSE---------------------------------------------
variances_interaction <- get_regression_points(model_2_interaction) %>% 
  summarize(var_y = var(ln.energy), 
                      var_y_hat = var(ln.energy_hat), 
                      var_residual = var(residual)) %>% 
  mutate(model = "Interaction", r_squared = var_y_hat/var_y)
variances_parallel_slopes <- get_regression_points(model_2_parallel_slopes) %>% 
  summarize(var_y = var(ln.energy), 
                      var_y_hat = var(ln.energy_hat), 
                      var_residual = var(residual)) %>% 
  mutate(model = "Parallel slopes", r_squared = var_y_hat/var_y)

bind_rows(
  variances_interaction,
  variances_parallel_slopes
) %>% 
  select(model, var_y, var_y_hat, var_residual, r_squared) %>% 
  knitr::kable(
    digits = 3,
    caption = "Comparing variances from interaction and parallel slopes models for mole rats data", 
    booktabs = TRUE,
    linesep = ""
  ) %>% 
  kable_styling(font_size = ifelse(knitr:::is_latex_output(), 10, 16),
                latex_options = c("hold_position"))


## ----model1-r-squared, echo=FALSE---------------------------------------------
variances_interaction <- get_regression_points(area_model1) %>% 
  summarize(var_y = var(sqrt.area), var_y_hat = var(sqrt.area_hat), var_residual = var(residual)) %>% 
  mutate(model = "Interaction", r_squared = var_y_hat/var_y)
variances_parallel_slopes <- get_regression_points(area_model2) %>% 
  summarize(var_y = var(sqrt.area), var_y_hat = var(sqrt.area_hat), var_residual = var(residual)) %>% 
  mutate(model = "Parallel slopes", r_squared = var_y_hat/var_y)

bind_rows(
  variances_interaction,
  variances_parallel_slopes
) %>% 
  select(model, var_y, var_y_hat, var_residual, r_squared) %>% 
  knitr::kable(
    digits = 3,
    caption = "Comparing variances from interaction and parallel slopes models for tooth growth data", 
    booktabs = TRUE,
    linesep = ""
  ) %>% 
  kable_styling(font_size = ifelse(knitr:::is_latex_output(), 10, 16),
                latex_options = c("hold_position"))


## -----------------------------------------------------------------------------
# R-squared for interaction model:
get_regression_summaries(model_2_interaction)
# R-squared for parallel slopes model:
get_regression_summaries(model_2_parallel_slopes)

