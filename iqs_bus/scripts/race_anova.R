library(tidyverse)

bus_raw <- read_csv("bus_data.csv")

race_perc <- bus_raw %>%
  select(Q25) %>%
  filter(!is.na(Q25)) %>%
  group_by(Q25) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

bus_perc <- race_perc$perc

jc_perc <- c(22.2, 3.1, 67, 5.7, 2)
# https://www.census.gov/quickfacts/jeffersoncountykentucky

combo <- data.frame(cbind(jc_perc, bus_perc))

stackd <- stack(combo)

anova <- aov(values ~ ind, data = stackd)

summary(anova)