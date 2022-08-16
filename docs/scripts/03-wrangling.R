## ---- eval=FALSE--------------------------------------------------------------
## chick1_weight <- filter(Chickbody_wt, Chick == 1)




## ---- message=FALSE-----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(openintro)








## ---- eval=FALSE--------------------------------------------------------------
## chick1_weight <- Chickbody_wt %>%
##   filter(Chick == 1)




## ---- eval=FALSE--------------------------------------------------------------
## predation1 <- mammals %>%
##   filter(predation == 1)
## View(predation1)


## ---- eval=FALSE--------------------------------------------------------------
## predation_notexp1 <- mammals %>%
##   filter((predation == 1 | predation == 2) & exposure != 1)
## predation_notexp1




## ---- eval=FALSE--------------------------------------------------------------
## predation_notexp1 <- mammals %>%
##   filter((predation == 1 | predation == 2) & !exposure == 1)
## predation_notexp1


## ---- eval=FALSE--------------------------------------------------------------
## predation_notexp1 <- mammals %>%
##   filter((predation == 1 | predation == 2), !exposure == 1)
## predation_notexp1


## ---- eval=FALSE--------------------------------------------------------------
## many_species <- mammals %>%
##   filter(species == "Africanelephant" | species == "Asianelephant" | species == "Deserthedgehog" | species == "Europeanhedgehog")








## ---- eval=FALSE--------------------------------------------------------------
## mammals %>% slice(1:5)


## ---- eval=FALSE--------------------------------------------------------------
## mammals %>% slice_max(n = 5, order_by = life_span)








## ---- eval=FALSE--------------------------------------------------------------
## glimpse(mammals)




## -----------------------------------------------------------------------------
many_species


## -----------------------------------------------------------------------------
slim_many_species <- many_species %>% 
  select(species, body_wt, danger)


## -----------------------------------------------------------------------------
slim_many_species


## ---- eval=FALSE--------------------------------------------------------------
## mammals_10cols <- mammals %>% select(-gestation)


## -----------------------------------------------------------------------------
mammals_sleep <- mammals %>% select(species, non_dreaming:total_sleep)
mammals_sleep


## ---- eval=FALSE--------------------------------------------------------------
## mammals_reorder <- mammals %>%
##   mammals_sleep <- mammals %>% select(species, non_dreaming:total_sleep, everything())
## glimpse(mammals_reorder)






## ---- eval=FALSE--------------------------------------------------------------
## mammals %>%
##   rename(danger_faced = danger)


## -----------------------------------------------------------------------------
mammals %>% 
  select(species, danger_faced = danger)






## -----------------------------------------------------------------------------
summary_mammals <- mammals %>% 
  summarize(mean = mean(life_span), std_dev = sd(life_span))
summary_mammals


## -----------------------------------------------------------------------------
summary_mammals <- mammals %>% 
  summarize(mean = mean(life_span, na.rm = TRUE), 
            std_dev = sd(life_span, na.rm = TRUE))
summary_mammals




## ----eval=FALSE---------------------------------------------------------------
## summary2_mammals <- mammals %>%
##   summarize(mean = mean(life_span, na.rm = TRUE)) %>%
##   summarize(std_dev = sd(life_span, na.rm = TRUE))






## -----------------------------------------------------------------------------
summary_pred_life <- mammals %>% 
  group_by(predation) %>% 
  summarize(mean = mean(life_span, na.rm = TRUE), 
            std_dev = sd(life_span, na.rm = TRUE))
summary_pred_life


## -----------------------------------------------------------------------------
mammals


## -----------------------------------------------------------------------------
mammals %>% 
  group_by(predation)




## -----------------------------------------------------------------------------
mammals %>% 
  group_by(predation) %>% 
  summarize(avg_life = mean(life_span, na.rm = TRUE))


## -----------------------------------------------------------------------------
mammals %>% 
  group_by(predation) %>% 
  ungroup()


## -----------------------------------------------------------------------------
by_pred <- mammals %>% 
  group_by(predation) %>% 
  summarize(count = n())
by_pred


## ----message=FALSE, warning=FALSE---------------------------------------------
by_pred_exp <- mammals %>% 
  group_by(predation, exposure) %>% 
  summarize(count = n())
by_pred_exp


## ----message=FALSE, warning=FALSE---------------------------------------------
by_pred_exp_incorrect <- mammals %>% 
  group_by(predation) %>% 
  group_by(exposure) %>% 
  summarize(count = n())
by_pred_exp_incorrect








## ---- eval=TRUE---------------------------------------------------------------
mammals <- mammals %>% 
  mutate(wt_in_lb = body_wt * 2.205)
mammals


## -----------------------------------------------------------------------------
summary_pred_wt <- mammals %>% 
  group_by(predation) %>% 
  summarize(mean_wt_in_kg = mean(body_wt, na.rm = TRUE), 
            mean_wt_in_lb = mean(wt_in_lb, na.rm = TRUE))
summary_pred_wt


## -----------------------------------------------------------------------------
lb_summary <- mammals %>% 
  summarize(
    min = min(wt_in_lb, na.rm = TRUE),
    q1 = quantile(wt_in_lb, 0.25, na.rm = TRUE),
    median = quantile(wt_in_lb, 0.5, na.rm = TRUE),
    q3 = quantile(wt_in_lb, 0.75, na.rm = TRUE),
    max = max(wt_in_lb, na.rm = TRUE),
    mean = mean(wt_in_lb, na.rm = TRUE),
    sd = sd(wt_in_lb, na.rm = TRUE),
    missing = sum(is.na(wt_in_lb))
  )
lb_summary


## ----eval=FALSE---------------------------------------------------------------
## mammals %>% select(wt_in_lb) %>% skim()


## ----depth-range-hist, fig.cap="Histogram of wt_in_lb variable.", message=FALSE, fig.height=3----
ggplot(data = mammals, mapping = aes(x = wt_in_lb)) +
  geom_histogram(color = "white", boundary = 0, binwidth = 200)


## -----------------------------------------------------------------------------
mammals <- mammals %>% 
  mutate(
    wt_in_oz = wt_in_lb * 16,
    oz_per_year = wt_in_oz / life_span
  )






## -----------------------------------------------------------------------------
freq_pred <- mammals %>% 
  group_by(predation) %>% 
  summarize(num_species = n())
freq_pred


## -----------------------------------------------------------------------------
freq_pred %>% 
  arrange(num_species)


## -----------------------------------------------------------------------------
freq_pred %>% 
  arrange(desc(num_species))

