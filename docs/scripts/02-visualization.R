## ----message=FALSE------------------------------------------------------------
library(openintro)
library(ggplot2)
library(dplyr)
















## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = mammals, mapping = aes(x = brain_wt, y = life_span)) +
##   geom_point()




## ----nolayers, fig.cap="A plot with no layers.", fig.height=2.5---------------
ggplot(data = mammals, mapping = aes(x = brain_wt, y = life_span)) 






## ----alpha, fig.cap="Brain weight vs. life span scatterplot with alpha = 0.2.", fig.height=4.9----
ggplot(data = mammals, mapping = aes(x = brain_wt, y = life_span)) + 
  geom_point(alpha = 0.2)




## ----jitter, fig.cap="Brain weight vs. life span jittered scatterplot.", fig.height=4.7----
ggplot(data = mammals, mapping = aes(x = brain_wt, y = life_span)) + 
  geom_jitter(width = 30, height = 1)






## -----------------------------------------------------------------------------
chick1_weight <- filter(ChickWeight, Chick == 1)






## ----dailyweight, fig.cap="body weights of Chick 1 on Diet 1"-----------------
ggplot(data = chick1_weight, 
       mapping = aes(x = Time, y = weight)) +
  geom_line()






## -----------------------------------------------------------------------------
chick_weight_d21 <- filter(ChickWeight, Time == 21)


## ----weight-on-line, echo=FALSE, fig.height=0.8, fig.cap="Plot of body weights for Chick 1"----
ggplot(data = chick_weight_d21, mapping = aes(x = weight, y = factor("A"))) +
  geom_point() +
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )
hist_title <- "Histogram of Chick Weights on Day 21"




## ----chick-weight-d21-histogram, warning=TRUE, fig.cap="Histogram of chick weights at day 21.", fig.height=2.3----
ggplot(data = chick_weight_d21, mapping = aes(x = weight)) +
  geom_histogram()


## ----chick-weight-d21-histogram-2, message=FALSE, fig.cap="Histogram of chick weights at day 21.", fig.height=3----
ggplot(data = chick_weight_d21, mapping = aes(x = weight)) +
  geom_histogram(color = "white")


## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = chick_weight_d21, mapping = aes(x = weight)) +
##   geom_histogram(color = "white", fill = "steelblue")


## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = chick_weight_d21, mapping = aes(x = weight)) +
##   geom_histogram(bins = 20, color = "white")


## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = chick_weight_d21, mapping = aes(x = weight)) +
##   geom_histogram(binwidth = 20, color = "white")








## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = chick_weight_d21, mapping = aes(x = weight)) +
##   geom_histogram(bins = 20, color = "white") +
##   facet_wrap(~Diet)




## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = chick_weight_d21, mapping = aes(x = weight)) +
##   geom_histogram(bins = 20, color = "white") +
##   facet_wrap(~Diet, ncol = 1)
















## ----Dietweightbox, fig.cap="Side-by-side boxplot of weight split by Diet.", fig.height=2.4----
Time21_ChickWeight <- ChickWeight %>%
  filter(Time == 21)
ggplot(data = Time21_ChickWeight, mapping = aes(x = Diet, y = weight)) +
  geom_boxplot()






## -----------------------------------------------------------------------------
fruits <- tibble(
  fruit = c("apple", "apple", "orange", "apple", "orange")
)
fruits_counted <- tibble(
  fruit = c("apple", "orange"),
  number = c(3, 2)
)






## ----geombar, fig.cap="Barplot when counts are not pre-counted.", fig.height=1.8----
ggplot(data = fruits, mapping = aes(x = fruit)) +
  geom_bar()


## ----geomcol, fig.cap="Barplot when counts are pre-counted.", fig.height=2.5----
ggplot(data = fruits_counted, mapping = aes(x = fruit, y = number)) +
  geom_col()


## ----habitatsbar, fig.cap="(ref:geombar)", fig.height=2.8---------------------
ggplot(data = mammals, mapping = aes(x = predation)) +
  geom_bar() 








## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = mammals, mapping = aes(x = predation)) +
##   geom_bar()


## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = mammals, mapping = aes(x = predation, fill = factor(exposure))) +
##   geom_bar()




## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = mammals, mapping = aes(x = predation, color = factor(exposure))) +
##   geom_bar()




## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = mammals, mapping = aes(x = predation), fill = factor(exposure)) +
##   geom_bar()


## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = mammals, mapping = aes(x = predation, fill = factor(exposure))) +
##   geom_bar(position = "dodge")




## ----eval=FALSE---------------------------------------------------------------
## ggplot(data = mammals, mapping = aes(x = predation)) +
##   geom_bar() +
##   facet_wrap(~ exposure, ncol = 1)






















## ----eval=FALSE---------------------------------------------------------------
## # Segment 1:
## ggplot(data = mammals, mapping = aes(x = predation)) +
##   geom_bar()
## 
## # Segment 2:
## ggplot(mammals, aes(x = predation)) +
##   geom_bar()








## ----eval=FALSE---------------------------------------------------------------
## chick1_weight <- filter(ChickWeight, Chick == 1)

