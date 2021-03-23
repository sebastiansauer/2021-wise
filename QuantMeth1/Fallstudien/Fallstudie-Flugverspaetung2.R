
# Fallstudie: Flugverspätungen modellieren --------------------------------

# Forschungsfrage
# 
# Welche Faktoren helfen, die Flugverspätungen vorherzusagen?
# 
# 


# Pakete laden ------------------------------------------------------------

library(tidyverse)
# library(tidymodels)



# Daten laden -------------------------------------------------------------


flights <- read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/nycflights13/flights.csv")



# Modell 1
# 

lm1 <- lm(dep_delay ~ origin, data = flights)

glance(lm1)


# Modell 2: Viele metrische Variablen
# 

lm2 <- lm(dep_delay ~ month + day + dep_time + air_time + distance, 
          data = flights)

glance(lm2)



# Nominale Variablen: 
# 
flights2 <- 
  flights %>% 
  mutate(dest = factor(dest),
         dest = fct_lump_n(dest, n = 30))


flights2 %>%
  count(dest, sort = TRUE)


flights2 %>% 
  select(where(~ !is.numeric(.))) %>% 
  names()


flights2 %>% 
  count(carrier, sort = TRUE)

flights3 <-
  flights2 %>% 
  mutate(carrier = factor(carrier),
         carrier = fct_lump_n(carrier, 9))



flights3 %>% 
  count(carrier, sort = TRUE)




# Modell 3
# 

lm3 <- 
  flights3 %>% 
  drop_na(dep_delay,
          month,
          day,
          dep_time,
          air_time,
          dest,
          distance,
          carrier) %>% 
  lm(dep_delay ~ month + day + dep_time + air_time + distance + dest + carrier, 
          data = .)

glance(lm3)



# Modell 4: Rohe Gewalt, alle Modelle durchprobieren
# 
library(leaps)

lm4 <- regsubsets(dep_delay ~ month + day + dep_time + air_time + distance + dest + carrier, 
           data = flights3,
           nvmax = 15)


# Modellergebnis:
summary(lm4)


# Koeffizienten für das MOdell mit 5 Prädiktoren:
coef(lm4, 5)


# Modellergebnis als Objekt speichern:
lm4_summary <- summary(lm4)


lm4_summary$adjr2


# Vorhersagen
# 
flights4 <- 
  flights3 %>% 
  drop_na(dep_delay,
          month,
          day,
          dep_time,
          air_time,
          dest,
          distance,
          carrier) %>% 
  mutate(pred = predict(lm3))


# Einreichen


flights4 %>% 
  select(id = X1, pred)


# Speichern als CSV
# 

write_csv(flights4, file = "flights4_prediction.csv")
