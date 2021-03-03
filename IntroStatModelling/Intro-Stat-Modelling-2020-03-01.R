# 2020-03-01
# Workshop on intro stats modelling
# find out more at: https://data-se.netlify.app/2020/06/19/introduction-to-statistics-a-modeling-based-approach-course-syllabus
# 


library(tidyverse)  # hit "Contrl-Enter" to execute a command
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



# exporting data:

library(writexl)
write_xlsx(x = table1, path = "table1.xlsx")

library(janitor)

table1 %>% 
  clean_names()


write_csv(x = table1, file = "table1.csv")




# Simple regression modelling

# prototype command for regression:
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

# delay is a function of origin as well as time of departure
lm3 <-
  lm(dep_delay ~ origin + hour, 
     data = flights)

tidy(lm3)

flights %>% 
  filter(month == 1) %>% 
  ggplot() +
  aes(x = hour, y = dep_delay, 
      color = origin) +
  #geom_jitter(alpha = .1) +
  geom_smooth(method = "lm")



lm4 <-
  lm(dep_delay ~ origin + hour + origin:hour, 
     data = flights)

tidy(lm4)
glance(lm4)


# 95% CI
# Intercept: 0.33*2 = .7 -> [-10.7; 9.3]
# 
confint(lm4)

flights_sum1 <- 
  flights %>% 
  drop_na(origin, hour, dep_delay) %>% 
  select(origin, hour, dep_delay) %>% 
  group_by(origin, hour) %>% 
  summarise(delay_avg = mean(dep_delay),
            delay_md = median(dep_delay),
            delay_q5 = quantile(dep_delay, prob = .05),
            delay_q95 = quantile(dep_delay, prob = .95),
            delay_q25 = quantile(dep_delay, prob = .25),
            delay_q75 = quantile(dep_delay, prob = .75)) 

View(flights_sum1)

# note the the mean value is outside the error bars.
flights_sum1 %>% 
  ggplot() +
  aes(x = hour, y = delay_md, color = origin) +
   geom_errorbar(aes(ymin = delay_q25,
                  ymax = delay_q75),
                color = "grey60",
                size = .25) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ origin) +
  labs(y = "Median delay",
       caption = "Error bars indicate 90% intervals")




flights %>% 
  summarise(n_distinct(dest))


lm5 <-
  lm(dep_delay ~ origin + hour + origin:hour + dest, 
     data = flights)

tidy(lm5)


flights_count <- 
  flights %>% 
  count(dest, sort = T)


View(flights_count)

flights2 <-
  flights %>% 
  mutate(dest_lumped = fct_lump(dest, n = 10)) %>% 
  select(dest_lumped, everything())


View(flights2)


flights2 %>% 
  summarise(n_distinct(dest_lumped))



lm6 <-
  lm(dep_delay ~ origin + hour + origin:hour + dest_lumped, 
     data = flights2)

glance(lm6)


flights2 %>% 
  drop_na(month, dep_delay) %>% 
  filter(dep_delay < 30) %>% 
  group_by(month) %>% 
  summarise(delay_md = median(dep_delay),
            delay_avg = mean(dep_delay),
            delay_sd = sd(dep_delay)) %>% 
  ggplot() +
  aes(x = month, y = delay_md) +
  geom_point() +
  geom_errorbar(aes(ymin = delay_avg - delay_sd,
                   ymax = delay_avg + delay_sd),
                color = "grey60") +
  geom_line() +
  scale_x_continuous(breaks = 1:12)
  
  
  

lm7 <-
  lm(dep_delay ~ origin + 
       hour + origin:hour + dest_lumped + month, 
     data = flights2)

glance(lm7)


library(lubridate)

flights3 <- 
  flights2 %>% 
  mutate(day_of_week = wday(time_hour))

View(flights3) 


flights4 <- 
  flights3 %>% 
  mutate(is_weekend = case_when(
    day_of_week %in% c(6, 7) ~ TRUE,
    TRUE ~ FALSE))


flights4 <- 
flights4 %>% 
  drop_na(dep_delay)

flights4 %>% 
  drop_na(is_weekend) %>% 
  group_by(is_weekend) %>% 
  summarise(delay_md = median(dep_delay))



