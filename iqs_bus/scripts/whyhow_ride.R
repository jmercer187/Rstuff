library(tidyverse)

bus_raw <- read_csv("bus_data.csv")

service_used_wide <- bus_raw %>%
  select(Q1_1, Q1_2, Q1_3, Q1_4)

service_used_long <- gather(service_used_wide, question, service, Q1_1:Q1_4) %>%
  filter(!is.na(service)) 

service_used_long_perc <- service_used_long %>%
  group_by(service) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

unique(bus_raw$Q3Specified)

instead_wide <- bus_raw %>%
  select(Q3_1, Q3_2, Q3_3, Q3_4, Q3_5, Q3_6)

instead_long <- gather(instead_wide, question, method, Q3_1:Q3_6) %>%
  filter(!is.na(method)) 

instead_long_perc <- instead_long %>%
  group_by(method) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

unique(bus_raw$Q4Specified)
table(bus_raw$Q4Specified)

trip_purpose_wide <- bus_raw %>%
  select(Q4_1, Q4_2, Q4_3, Q4_4)

trip_purpose_long <- gather(trip_purpose_wide, question, purpose, Q4_1:Q4_4) %>%
  filter(!is.na(purpose))
  
trip_purpose_perc <- trip_purpose_long %>%
  group_by(purpose) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

unique(bus_raw$Q12_10Specified)
table(bus_raw$Q12_10Specified)

payment_wide <- bus_raw %>%
  select(Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, Q12_7, Q12_8, Q12_9, Q12_10)

payment_long <- gather(payment_wide, question, payment, Q12_1:Q12_10) %>%
  filter(!is.na(payment))

payment_long_perc <- payment_long %>%
  group_by(payment) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

reduced_fare <- bus_raw %>%
  select(Q13) %>%
  filter(!is.na(Q13))

reduced_fare_perc <- reduced_fare %>%
  group_by(Q13) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )
