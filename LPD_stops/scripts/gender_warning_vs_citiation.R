library(tidyverse)
library(lubridate)
library(data.table)

stops_raw <- fread("source_data/LMPD_STOPS_DATA_12.csv")

unique(stops_raw$`ACTIVITY RESULTS`)

stops <- stops_officer_m <- stops_raw %>%
  filter(OFFICER_GENDER != '') %>%
  filter(`ACTIVITY RESULTS` != '') %>%
  filter(ACTIVITY_BEAT != '' & ACTIVITY_BEAT != ' ') %>%
  filter(DRIVER_GENDER != '') %>%
  filter(DRIVER_AGE_RANGE != "") %>%
  filter(DRIVER_RACE != "") %>%
  filter(`NUMBER OF PASSENGERS` != "") %>%
  filter(`NUMBER OF PASSENGERS` <= 6) %>%
  filter(WAS_VEHCILE_SEARCHED != "") %>%
  filter(DRIVER_RACE == 'BLACK'| DRIVER_RACE == 'HISPANIC' | DRIVER_RACE == 'WHITE') %>%
  filter(ACTIVITY_TIME != "" | ACTIVITY_TIME != " ") %>%
  mutate(
    activity_date = mdy(ACTIVITY_DATE),
    date_time = ymd_hms(str_c(activity_date, ACTIVITY_TIME))
  ) %>%
  filter(activity_date < ymd(20160101)) %>%
  mutate(
    cat_month = month(date_time),
    cat_wday = wday(date_time),
    cat_mday = mday(date_time),
    cat_time = cut(hour(date_time), c(0, 4, 8, 12, 16, 20, 24))
  ) %>%
  filter(!is.na(cat_time))

stops_officer_m <- stops %>%
  filter(OFFICER_GENDER == 'M') %>%
  group_by(DRIVER_GENDER, `ACTIVITY RESULTS`) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

p <- ggplot(data = stops_officer_m, mapping = aes(x = DRIVER_GENDER, y = perc))
p + geom_col(aes(fill = `ACTIVITY RESULTS`), position = "dodge") +
  labs(x = "Gender", 
       y = 'Percentage',
       title = "Stops by Male Officers",
       subtitle = "Percentage of Warnings vs Citations given by gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Stop Outcome")
  

stops_officer_f <- stops %>%
  filter(OFFICER_GENDER == 'F') %>%
  group_by(DRIVER_GENDER, `ACTIVITY RESULTS`) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

p <- ggplot(data = stops_officer_f, mapping = aes(x = DRIVER_GENDER, y = perc))
p + geom_col(aes(fill = `ACTIVITY RESULTS`), position = "dodge") +
  labs(x = "Gender", 
       y = 'Percentage',
       title = "Stops by Female Officers",
       subtitle = "Percentage of Warnings vs Citations given by gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Stop Outcome")


unique(stops$ACTIVITY_BEAT)

officer_beats <- stops %>%
  group_by(OFFICER_GENDER, ACTIVITY_BEAT) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

p <- ggplot(data = officer_beats, mapping = aes(x = OFFICER_GENDER, y = perc))
p + geom_col(aes(fill = ACTIVITY_BEAT), position = "dodge2") +
  labs(x = "Gender", 
       y = 'Percentage',
       title = "Beat Distribution by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Beat")

officer_divisions <- stops %>%
  group_by(OFFICER_GENDER, ACTIVITY_DIVISION) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

p <- ggplot(data = officer_divisions, mapping = aes(x = OFFICER_GENDER, y = perc))
p + geom_col(aes(fill = ACTIVITY_DIVISION), position = "dodge2") +
  labs(x = "Gender", 
       y = 'Percentage',
       title = "Division Distribution by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Division")


p <- ggplot(data = stops, mapping = aes(x = DRIVER_AGE_RANGE))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "Age Range of Stops by Officer Gender")

p <- ggplot(data = stops, mapping = aes(x = `NUMBER OF PASSENGERS`))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "Number of Passengers by Officer Gender")

p <- ggplot(data = stops, mapping = aes(x = DRIVER_RACE))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "Driver Race by Officer Gender")

p <- ggplot(data = stops, mapping = aes(x = cat_month))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "Month of Stop by Officer Gender")

p <- ggplot(data = stops, mapping = aes(x = cat_wday))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "Weekday of Stop by Officer Gender")

p <- ggplot(data = stops, mapping = aes(x = cat_mday))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "Month Day of Stop by Officer Gender")

p <- ggplot(data = stops, mapping = aes(x = cat_time))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "Time of Stop by Officer Gender")

vars <- c("DRIVER_AGE_RANGE", "`NUMBER OF PASSENGERS`", "DRIVER_RACE", "cat_month", "cat_wday", "cat_mday", "cat_time")

# for(i in seq_along(vars)){
#   p <- ggplot(data = stops, mapping = aes(x = vars[i]))
#   p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
#     labs(title = str_c("Time of Stop by Officer ", vars[i]))
# }
