library(tidyverse)
library(rstanarm)


# z-Werte (Quantile, q) und Verteilungsfunktion F (Wskt, p) berechnen
# aus der Normalverteilung

# Wir suchen Wskt eines z-Werts:
pnorm(q = 2)  # "nach links"
pnorm(q = 1)
pnorm(q = 0)

# Wir suchen z-Wert für bestimmte Wskt:
qnorm(p = .5)
qnorm(p = .977)
qnorm(p = .84)


# Bayes-Regression

# A: Berechnen Sie das Regressionsgewicht für diese Regression: mpg ~ hp
# (Datensatz mtcars)


m1 <- stan_glm(mpg ~ hp, data = mtcars)
coef(m1)
summary(m1)

m1_post  <- 
  m1 %>% as_tibble()


m1_post %>% 
  summarise(hp_median = median(hp),
            hp_q50 = quantile(hp, prob = .5),
            hp_q05 = quantile(hp, prob = .05),
            hp_q95 = quantile(hp, prob = .95))

            