

# Pakete laden ------------------------------------------------------------


library(tidyverse)
library(lubridate)



# Daten laden -------------------------------------------------------------


flights <- read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/nycflights13/flights.csv")

dim(flights)



# Forschungsfrage ---------------------------------------------------------

print("Prädiktoren von Flugverspätungen")



# Überblick ---------------------------------------------------------------


glimpse(flights)


flights %>% 
  sample_n(size = 1000) %>% 
  ggplot() +
  aes(x = arr_delay, y = dep_delay) +
  geom_point()


flights %>% 
  select(contains("delay")) %>% 
  drop_na() %>% 
  mutate(dep_delay = scale(dep_delay),
         arr_delay = scale(arr_delay)) %>% 
  ggplot() +
  aes(x = arr_delay, y = dep_delay) +
  geom_bin2d() +
  geom_abline(linetype = "dashed",
              color = "grey60")


flights %>%
  select(contains("delay")) %>% 
  drop_na() %>% 
  summarise(cor(dep_delay, arr_delay))



# Verteilung der Zielvariable ---------------------------------------------


flights %>% 
  ggplot() +
  aes(x = dep_delay) %>% 
  geom_density()


flights %>% 
  select(dep_delay) %>% 
  drop_na() %>% 
  summarise(delay_avg = mean(dep_delay),
            delay_md = median(dep_delay),
            delay_sd = sd(dep_delay),
            delay_irq = IQR(dep_delay))


flights2 <- 
  flights %>% 
  select(-year) %>% 
  drop_na(dep_delay) %>% 
  mutate(is_extreme = case_when(
    dep_delay > quantile(dep_delay, prob = .75) + 1.5 * IQR(dep_delay) ~ TRUE,
    TRUE ~ FALSE
  ))



# Fehlende Werte ----------------------------------------------------------


flights %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))) %>% 
  pivot_longer(everything())





# Numerische Korrelate der Verspätung -------------------------------------

flights2 %>% 
  drop_na(dep_time) %>% 
  summarise(cor_delay_deptime = cor(dep_delay, dep_time))


flights2 %>% 
  select(where(is.numeric)) %>% 
  summarise(across(
    .cols = everything(),
    .fns = ~ cor(., flights2$dep_delay, use = "complete.obs")
  )) %>% 
  pivot_longer(everything()) %>% 
  arrange(-abs(value))




# Nominalen Prädiktoren ---------------------------------------------------

flights2 %>% 
  select(where(negate(is.numeric))) %>% 
  names()
  
flights2 %>% 
  count(carrier, sort = TRUE)
  


# flights3 ----------------------------------------------------------------



flights3 <-
  flights2 %>% 
  mutate(carrier = factor(carrier)) %>% 
  mutate(carrier = fct_lump_n(carrier, 9))



# carrier -----------------------------------------------------------------



flights3 %>% 
  filter(!is_extreme) %>% 
  ggplot() +
  aes(x = fct_reorder(carrier, -dep_delay), y = dep_delay) +
  geom_boxplot()

  

# hour --------------------------------------------------------------------


flights3 %>% 
  filter(!is_extreme) %>% 
  mutate(hour = factor(hour)) %>% 
  ggplot() +
  aes(x = fct_reorder(hour, dep_delay), y = dep_delay) +
  geom_boxplot()


# origin ------------------------------------------------------------------


flights3 %>% 
  filter(!is_extreme) %>% 
  ggplot() +
  aes(x = origin, y = dep_delay) +
  geom_boxplot()


flights3 %>% 
  filter(!is_extreme) %>% 
  drop_na(origin, dep_delay) %>% 
  select(origin, dep_delay) %>% 
  group_by(origin) %>% 
  summarise(delay_md = median(dep_delay),
            deplay_iqr = IQR(dep_delay))



# Drei Variablen ----------------------------------------------------------

flights3 %>% 
  filter(!is_extreme) %>% 
  select(origin, hour, dep_delay) %>% 
  mutate(hour = factor(hour)) %>% 
  ggplot() +
  aes(x = hour, y = dep_delay) +
  geom_violin() +
  facet_wrap( ~ origin)




# Vier Variablen ----------------------------------------------------------



flights3 %>% 
  filter(!is_extreme) %>% 
  select(origin, hour, dep_delay, carrier) %>% 
  mutate(hour = factor(hour)) %>% 
  ggplot() +
  aes(x = hour, y = dep_delay) +
  geom_violin() +
  facet_wrap(carrier ~ origin)



# Feature Engineering -----------------------------------------------------

flights4 <- 
  flights3 %>% 
  mutate(month = month(time_hour)) 


flights4 %>% 
  select(month, time_hour) %>% 
  View()



flights4 %>% 
  filter(!is_extreme) %>% 
  select(month, origin, dep_delay) %>% 
  mutate(month = factor(month)) %>% 
  drop_na() %>% 
  ggplot() +
  aes(x = month, y = dep_delay, color = origin) %>% 
  geom_boxplot()


flights4_sum <- 
  flights4 %>% 
  filter(!is_extreme) %>% 
  select(month, origin, dep_delay) %>% 
  drop_na() %>% 
  group_by(month, origin) %>% 
  summarise(delay_md = median(dep_delay),
            delay_iqr = IQR(dep_delay),
            delay_n = n()) %>% 
  mutate(month = factor(month),
         delay_n = as.numeric(delay_n))
  


flights4 %>% 
  filter(!is_extreme) %>% 
  select(month, origin, dep_delay) %>% 
  mutate(month = factor(month)) %>% 
  drop_na() %>% 
  ggplot() +
  aes(x = month, y = dep_delay, color = origin) +
  geom_violin() +
  geom_point(data = flights4_sum, 
             aes(y = delay_md,
                 x = month)) +
  facet_wrap( ~ origin)


