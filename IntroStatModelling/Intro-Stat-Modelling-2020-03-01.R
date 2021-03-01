

library(tidyverse)  # Contrl-Enter
library(nycflights13)  # data


View(flights)

# avg. dep. delay of all fllights starting from NYC in 2013
flights %>%   # "THEN DO NEXT"  Shortcut Control-Shift-M
  drop_na(dep_delay) %>% 
  summarise(mean(dep_delay))


# avg delay for each ... of the 3 airports


flights %>% 
  drop_na(dep_delay) %>% 
  group_by(origin) %>% 
  summarise(dep_delay_avg = mean(dep_delay),
            dep_delay_sd = sd(dep_delay)) %>% 
  arrange(-dep_delay_avg)


flights2 <-  # assignment operator, defines new objects
  flights %>% 
  mutate(distance_km = distance / 1.6) %>% 
  select(dep_time, origin, distance, distance_km)


table1 <-  # assignment operator
flights %>% 
  drop_na(air_time) %>% 
  summarise(n_distinct_destinations = n_distinct(carrier),
            air_time_avg = mean(air_time, na.rm = TRUE))


flights %>%  # "pipe": the "then-do-next operator"
  mutate(dep_delay_ln = log10(dep_delay)) %>% 
  ggplot() +
  aes(x = dep_delay_ln) +
  geom_histogram()


flights %>% 
  ggplot() +
  aes(x = origin, y = dep_delay) +
  geom_boxplot()
  
  
flights %>% 
  drop_na(dep_delay) %>% 
  filter(dep_delay < 100) %>% 
  ggplot() +
  aes(x = origin, y = dep_delay) +
  geom_boxplot()


flights %>% 
  drop_na(dep_delay) %>% 
  filter(dep_delay < 100) %>% 
  ggplot() +
  aes(x = dep_delay, color = origin, fill = origin) +
  geom_density(alpha = .5)
          
cor_depdelay_distance <- 
flights %>% 
  drop_na(dep_delay, distance) %>% 
  summarise(cor_depdelay_distance = cor(dep_delay, distance))


flights %>% 
  filter(month == 1) %>% 
  filter(dep_delay < 100, distance < 3000) %>% 
  ggplot() +
  aes(x = distance, y = dep_delay, color = origin) +
  geom_point() +
  geom_smooth(color = "grey20",
              method = lm) +
  labs(title = "Scatterplot for dep. delay as a function of distance",
       subtitle = "Correlation r=-0.02") +
  facet_wrap( ~ origin)



# exporting

library(writexl)
write_xlsx(x = table1, path = "table1.xlsx")

library(janitor)

table1 %>% 
  clean_names()


write_csv(x = table1, file = "table1.csv")




# Simple regression modelling
# 
# 
# 

# lm(y ~ x, data = my_data)
# 

lm1 <-
  lm(dep_delay ~ hour, data = flights)

flights %>% 
  ggplot() +
  aes(x = hour, y = dep_delay) +
  geom_smooth(method = "lm") +
  #geom_bin2d() +
  coord_cartesian(ylim = c(-10, 23),
                  xlim = c(0, 23)) +
  labs(y = "delay in minutes",
       x = "departure hour")



delay_20h = -9.8 + 1.7*20
delay_20h

library(broom)

tidy(lm1)

glance(lm1)


lm2 <-
  lm(dep_delay ~ origin, data = flights)

tidy(lm2)


flights %>% 
  drop_na(origin, dep_delay) %>% 
  group_by(origin) %>% 
  summarise(dep_delay_avg = mean(dep_delay)) %>% 
  ggplot() +
  aes(x = origin,
      y = dep_delay_avg) +
  geom_point(size = 5, color = "red")


