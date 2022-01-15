## ---- eval=FALSE--------------------------------------------------------------
## brackish_fish <- all_fishdata %>%
##   filter(Brack == 1)


## ---- message=FALSE-----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(rfishbase)








## ---- eval=FALSE--------------------------------------------------------------
## brackish_fish <- all_fishdata %>%
##   filter(Brack == "1")




## ---- eval=FALSE--------------------------------------------------------------
## paquarium_fish <- all_fishdata %>%
##   filter(Aquarium == "public aquariums")
## View(paquarium_fish)


## -----------------------------------------------------------------------------
danger_comm_fish <- all_fishdata %>% 
  filter((Aquarium == "commercial" | Aquarium == "highly commercial") & Dangerous != "harmless" & Dangerous != "Harmless")
View(danger_comm_fish)


## ---- eval=FALSE--------------------------------------------------------------
## danger_comm_fish <- all_fishdata %>%
##   filter((Aquarium == "commercial" | Aquarium == "highly commercial") & !(Dangerous == "harmless" | Dangerous == "Harmless"))
## View(danger_comm_fish)


## ---- eval=FALSE--------------------------------------------------------------
## danger_comm_fish <- all_fishdata %>%
##   filter((Aquarium == "commercial" | Aquarium == "highly commercial"), !(Dangerous == "harmless" | Dangerous == "Harmless"))
## View(danger_comm_fish)


## ---- eval=FALSE--------------------------------------------------------------
## many_aquarium_fish <- all_fishdata %>%
##   filter(Aquarium == "commercial" | Aquarium == "highly commercial" | Aquarium == "show aquarium" | Aquarium == "public aquariums")


## ---- eval=FALSE--------------------------------------------------------------
## many_aquarium_fish <- all_fishdata %>%
##   filter(Aquarium %in% c("commercial", "highly commercial", "show aquarium", "public aquariums"))
## View(many_aquarium_fish)






## ---- eval=FALSE--------------------------------------------------------------
## all_fishdata %>% slice(1:100)


## ---- eval=FALSE--------------------------------------------------------------
## all_fishdata %>% slice_max(n = 10, order_by = DepthRangeDeep)








## ---- eval=FALSE--------------------------------------------------------------
## glimpse(all_fishdata)




## -----------------------------------------------------------------------------
View(danger_comm_fish)


## -----------------------------------------------------------------------------
slim_danger_comm_fish <- danger_comm_fish %>% 
  select(Species, Genus, Aquarium, Dangerous)


## -----------------------------------------------------------------------------
slim_danger_comm_fish


## ---- eval=FALSE--------------------------------------------------------------
## slim_danger_comm_fish <- slim_danger_comm_fish %>% select(-Genus)


## -----------------------------------------------------------------------------
slim2_danger_comm_fish <- danger_comm_fish %>% select(SpecCode:Species, Fresh:Saltwater)
slim2_danger_comm_fish


## ---- eval=FALSE--------------------------------------------------------------
## fishdata_reorder <- all_fishdata %>%
##   select(Species, BodyShapeI, DepthRangeShallow, DepthRangeDeep, everything())
## glimpse(fishdata_reorder)






## ---- eval=FALSE--------------------------------------------------------------
## all_fishdata %>%
##   rename(Preferred_Habitat = DemersPelag)


## -----------------------------------------------------------------------------
all_fishdata %>% 
  select(Species, Preferred_Habitat = DemersPelag)






## -----------------------------------------------------------------------------
summary_brackish <- brackish_fish %>% 
  summarize(mean = mean(Length), std_dev = sd(Length))
summary_brackish


## -----------------------------------------------------------------------------
summary_brackish <- brackish_fish %>% 
  summarize(mean = mean(Length, na.rm = TRUE), 
            std_dev = sd(Length, na.rm = TRUE))
summary_brackish




## ----eval=FALSE---------------------------------------------------------------
## summary_wt_d21 <- brackish_fish %>%
##   summarize(mean = mean(Length, na.rm = TRUE)) %>%
##   summarize(std_dev = sd(Length, na.rm = TRUE))






## -----------------------------------------------------------------------------
summary_DP_length <- all_fishdata %>% 
  group_by(DemersPelag) %>% 
  summarize(mean = mean(Length, na.rm = TRUE), 
            std_dev = sd(Length, na.rm = TRUE))
summary_DP_length


## -----------------------------------------------------------------------------
all_fishdata


## -----------------------------------------------------------------------------
all_fishdata %>% 
  group_by(DemersPelag)




## -----------------------------------------------------------------------------
all_fishdata %>% 
  group_by(DemersPelag) %>% 
  summarize(avg_depth = mean(DepthRangeDeep, na.rm = TRUE))


## -----------------------------------------------------------------------------
all_fishdata %>% 
  group_by(DemersPelag) %>% 
  ungroup()


## -----------------------------------------------------------------------------
by_DP <- all_fishdata %>% 
  group_by(DemersPelag) %>% 
  summarize(count = n())
by_DP


## -----------------------------------------------------------------------------
by_Dp_Shape <- all_fishdata %>% 
  group_by(DemersPelag, BodyShapeI) %>% 
  summarize(count = n())
by_Dp_Shape


## -----------------------------------------------------------------------------
by_Dp_Shape_incorrect <- all_fishdata %>% 
  group_by(DemersPelag) %>% 
  group_by(BodyShapeI) %>% 
  summarize(count = n())
by_Dp_Shape_incorrect








## ---- eval=TRUE---------------------------------------------------------------
all_fishdata <- all_fishdata %>% 
  mutate(wt_in_oz = Weight / 28.35)


## -----------------------------------------------------------------------------
summary_DP_wt <- all_fishdata %>% 
  group_by(DemersPelag) %>% 
  summarize(mean_wt_in_g = mean(Weight, na.rm = TRUE), 
            mean_wt_in_oz = mean(wt_in_oz, na.rm = TRUE))
summary_DP_wt


## ---- eval=FALSE--------------------------------------------------------------
## all_fishdata <- all_fishdata %>%
##   mutate(depth_range = DepthRangeDeep - DepthRangeShallow)


## -----------------------------------------------------------------------------
all_fishdata %>% select(starts_with("Depth")) %>% glimpse()


## -----------------------------------------------------------------------------
all_fishdata <- all_fishdata %>% mutate(DepthRangeShallow = as.numeric(DepthRangeShallow))


## -----------------------------------------------------------------------------
all_fishdata <- all_fishdata %>% 
  mutate(depth_range = DepthRangeDeep - DepthRangeShallow)




## -----------------------------------------------------------------------------
depth_summary <- all_fishdata %>% 
  summarize(
    min = min(depth_range, na.rm = TRUE),
    q1 = quantile(depth_range, 0.25, na.rm = TRUE),
    median = quantile(depth_range, 0.5, na.rm = TRUE),
    q3 = quantile(depth_range, 0.75, na.rm = TRUE),
    max = max(depth_range, na.rm = TRUE),
    mean = mean(depth_range, na.rm = TRUE),
    sd = sd(depth_range, na.rm = TRUE),
    missing = sum(is.na(depth_range))
  )
depth_summary


## ----eval=FALSE---------------------------------------------------------------
## all_fishdata %>% select(depth_range) %>% skim()


## ----depth-range-hist, fig.cap="Histogram of depth_range variable.", message=FALSE, fig.height=3----
ggplot(data = all_fishdata, mapping = aes(x = depth_range)) +
  geom_histogram(color = "white", boundary = 0, binwidth = 200)


## -----------------------------------------------------------------------------
all_fishdata <- all_fishdata %>% 
  mutate(
    depth_range = DepthRangeShallow - DepthRangeDeep,
    range_per_depth = depth_range / DepthRangeDeep
  )






## -----------------------------------------------------------------------------
freq_DP <- all_fishdata %>% 
  group_by(DemersPelag) %>% 
  summarize(num_species = n())
freq_DP


## -----------------------------------------------------------------------------
freq_DP %>% 
  arrange(num_species)


## -----------------------------------------------------------------------------
freq_DP %>% 
  arrange(desc(num_species))

