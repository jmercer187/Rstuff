library(tidyverse)

bus_raw <- read_csv("bus_data.csv")

# TOTAL SATISFACTION  
satisfied <- bus_raw %>%
  select(Q20) %>%
  filter(!is.na(Q20)) %>%
  group_by(Q20) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q20 = factor(Q20, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = satisfied) +
  geom_col(aes(x = Q20, y = perc)) +
  ggtitle("How satisfied are you with BUS?") +
  xlab('') +
  ylab('Percentage of Respondents')


trip_length <- bus_raw %>%
  select(Q6) %>%
  filter(str_detect(Q6, '^[0-9]*$')) %>%
  transmute(
    Q6 = as.integer(Q6)
  )

median(trip_length$Q6)
min(trip_length$Q6)
max(trip_length$Q6)
quantile(trip_length$Q6)

ggplot(data = trip_length, aes(x = Q6)) +
  geom_histogram()

length_ok <- bus_raw %>%
  select(Q7) %>%
  filter(!is.na(Q7))

length_ok_perc <- length_ok %>%
  group_by(Q7) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )


#   SATISFIED WITH BENCHES AT STOPS
benches <- bus_raw %>%
  select(Q18_1) %>%
  filter(!is.na(Q18_1)) %>%
  group_by(Q18_1) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q18_1 = factor(Q18_1, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = benches) +
  geom_col(aes(x = Q18_1, y = perc)) +
  ggtitle("How satisfied are you with bus stop benches?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH LIGHTING
lighting <- bus_raw %>%
  select(Q18_2) %>%
  filter(!is.na(Q18_2)) %>%
  group_by(Q18_2) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q18_2 = factor(Q18_2, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = lighting) +
  geom_col(aes(x = Q18_2, y = perc)) +
  ggtitle("How satisfied are you with bus stop lighting?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH SIDEWALK ACCESS
sidewalk <- bus_raw %>%
  select(Q18_3) %>%
  filter(!is.na(Q18_3)) %>%
  group_by(Q18_3) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q18_3 = factor(Q18_3, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = sidewalk) +
  geom_col(aes(x = Q18_3, y = perc)) +
  ggtitle("How satisfied are you with bus stop sidewalk access?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH BUS CLEANLINESS
clean <- bus_raw %>%
  select(Q18_4) %>%
  filter(!is.na(Q18_4)) %>%
  group_by(Q18_4) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q18_4 = factor(Q18_4, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = clean) +
  geom_col(aes(x = Q18_4, y = perc)) +
  ggtitle("How satisfied are you with bus cleanliness?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH BUS CROWDING?
crowding <- bus_raw %>%
  select(Q18_5) %>%
  filter(!is.na(Q18_5)) %>%
  group_by(Q18_5) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q18_5 = factor(Q18_5, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = crowding) +
  geom_col(aes(x = Q18_5, y = perc)) +
  ggtitle("How satisfied are you with crowding on the bus?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH BUS SAFETY?
safety <- bus_raw %>%
  select(Q18_6) %>%
  filter(!is.na(Q18_6)) %>%
  group_by(Q18_6) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q18_6 = factor(Q18_6, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = safety) +
  geom_col(aes(x = Q18_6, y = perc)) +
  ggtitle("How satisfied are you with bus safety?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH BUS FRIENDLINESS?
friend <- bus_raw %>%
  select(Q18_7) %>%
  filter(!is.na(Q18_7)) %>%
  group_by(Q18_7) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q18_7 = factor(Q18_7, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = friend) +
  geom_col(aes(x = Q18_7, y = perc)) +
  ggtitle("How satisfied are you with friendliness of drivers?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH BUS PHONE SERVICE RESPONSE?
phone <- bus_raw %>%
  select(Q18_8) %>%
  filter(!is.na(Q18_8)) %>%
  group_by(Q18_8) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q18_8 = factor(Q18_8, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = phone) +
  geom_col(aes(x = Q18_8, y = perc)) +
  ggtitle("How satisfied are you with phone service response?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH ROUTES?
routes <- bus_raw %>%
  select(Q19_1) %>%
  filter(!is.na(Q19_1)) %>%
  group_by(Q19_1) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q19_1 = factor(Q19_1, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = routes) +
  geom_col(aes(x = Q19_1, y = perc)) +
  ggtitle("How satisfied are you with availability of routes?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH DIRECTNESS of ROUTES?
direct <- bus_raw %>%
  select(Q19_2) %>%
  filter(!is.na(Q19_2)) %>%
  group_by(Q19_2) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q19_2 = factor(Q19_2, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = direct) +
  geom_col(aes(x = Q19_2, y = perc)) +
  ggtitle("How satisfied are you with directness of routes?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH NUMBER OF STOPS?
stops <- bus_raw %>%
  select(Q19_3) %>%
  filter(!is.na(Q19_3)) %>%
  group_by(Q19_3) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q19_3 = factor(Q19_3, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = stops) +
  geom_col(aes(x = Q19_3, y = perc)) +
  ggtitle("How satisfied are you with the number of stops?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH FREQUENCY OF SERVICE?
stop_freq <- bus_raw %>%
  select(Q19_4) %>%
  filter(!is.na(Q19_4)) %>%
  group_by(Q19_4) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q19_4 = factor(Q19_4, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = stop_freq) +
  geom_col(aes(x = Q19_4, y = perc)) +
  ggtitle("How satisfied are you with the frequency of stops?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH SERVICE TIMES?
times <- bus_raw %>%
  select(Q19_5) %>%
  filter(!is.na(Q19_5)) %>%
  group_by(Q19_5) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q19_5 = factor(Q19_5, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = times) +
  geom_col(aes(x = Q19_5, y = perc)) +
  ggtitle("How satisfied are you with the frequency of stops?") +
  xlab('') +
  ylab('Percentage of Respondents')


#   SATISFIED WITH COST?
cost <- bus_raw %>%
  select(Q19_6) %>%
  filter(!is.na(Q19_6)) %>%
  group_by(Q19_6) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0),
    Q19_6 = factor(Q19_6, levels = c("Not at all satisfied", "2", "3", "4", "Completely satisfied"))
  )

ggplot(data = cost) +
  geom_col(aes(x = Q19_6, y = perc)) +
  ggtitle("How satisfied are you with the cost of fare?") +
  xlab('') +
  ylab('Percentage of Respondents')