library(tidyverse)

bus_raw <- read_csv("bus_data.csv")

#   ONLINE TRIP PLANNER
planner <- bus_raw %>%
  select(Q21_1) %>%
  filter(!is.na(Q21_1)) %>%
  group_by(Q21_1) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q21_1 = factor(Q21_1, levels = c("Not at all important", "2", "3", "4", "Extremely important"))
  )

ggplot(data = planner) +
  geom_col(aes(x = Q21_1, y = perc)) +
  ggtitle("How important is an online trip planner?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   ELECTRONIC SIGNS AT STOPS SHOWING REAL TIME ARRIVAL
signs <- bus_raw %>%
  select(Q21_2) %>%
  filter(!is.na(Q21_2)) %>%
  group_by(Q21_2) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q21_2 = factor(Q21_2, levels = c("Not at all important", "2", "3", "4", "Extremely important"))
  )

ggplot(data = signs) +
  geom_col(aes(x = Q21_2, y = perc)) +
  ggtitle("How important are signs at stops showing arrival times?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   TEXT ALERTS WHEN BUS IS DELAYED
alert <- bus_raw %>%
  select(Q21_3) %>%
  filter(!is.na(Q21_3)) %>%
  group_by(Q21_3) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q21_3 = factor(Q21_3, levels = c("Not at all important", "2", "3", "4", "Extremely important"))
  )

ggplot(data = alert) +
  geom_col(aes(x = Q21_3, y = perc)) +
  ggtitle("How important are test alerts for delayed busses?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   MAPS AND SCHEDULES AT STOPS
maps <- bus_raw %>%
  select(Q21_4) %>%
  filter(!is.na(Q21_4)) %>%
  group_by(Q21_4) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q21_4 = factor(Q21_4, levels = c("Not at all important", "2", "3", "4", "Extremely important"))
  )

ggplot(data = maps) +
  geom_col(aes(x = Q21_4, y = perc)) +
  ggtitle("How important are maps and schedules at stops?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   NEXT STOP ANNOUNCEMENTS ON BUS
announce <- bus_raw %>%
  select(Q21_5) %>%
  filter(!is.na(Q21_5)) %>%
  group_by(Q21_5) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q21_5 = factor(Q21_5, levels = c("Not at all important", "2", "3", "4", "Extremely important"))
  )

ggplot(data = announce) +
  geom_col(aes(x = Q21_5, y = perc)) +
  ggtitle("How important is a stop announcement system on busses?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   WiFi ON BUSSES
wifi <- bus_raw %>%
  select(Q21_6) %>%
  filter(!is.na(Q21_6)) %>%
  group_by(Q21_6) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q21_6 = factor(Q21_6, levels = c("Not at all important", "2", "3", "4", "Extremely important"))
  )

ggplot(data = wifi) +
  geom_col(aes(x = Q21_6, y = perc)) +
  ggtitle("How important is WiFi on busses?") +
  xlab('') +
  ylab('Percentage of Respondents')
