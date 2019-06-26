---
  title: "Gender and Traffic Stops"
author: "Jacob Mercer"
date: "6/24/2019"
output: 
  html_document:
  code_folding: hide
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(lubridate)
library(data.table)
library(knitr)

stops_raw <- fread("C:/Users/Jake/Documents/Repos/Rstuff/LPD_stops/source_data/LMPD_STOPS_DATA_12.csv")

```

```{data transform}

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
  filter(ACTIVITY_TIME != "" & ACTIVITY_TIME != " ") %>%
  filter(ACTIVITY_DIVISION != "NON LMPD" & ACTIVITY_DIVISION != "") %>%
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

stops_officer_f <- stops %>%
  filter(OFFICER_GENDER == 'F') %>%
  group_by(DRIVER_GENDER, `ACTIVITY RESULTS`) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

officer_divisions <- stops %>%
  group_by(OFFICER_GENDER, ACTIVITY_DIVISION) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

```

## Officer Gender and Traffic Stops

PARAGRAPH 1

```{graph 1}
p <- ggplot(data = stops_officer_m, mapping = aes(x = DRIVER_GENDER, y = perc))
p + geom_col(aes(fill = `ACTIVITY RESULTS`), position = "dodge") +
  labs(x = "Gender", 
       y = 'Percentage',
       title = "Stops by Male Officers",
       subtitle = "Percentage of Warnings vs Citations given by gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Stop Outcome")
```


PARAGRAPH 2


```{tables}
kable(stops_officer_m)
kable(stops_officer_f)
```


PARAGRAPH 3

```{graph 2}
p <- ggplot(data = stops, mapping = aes(x = DRIVER_AGE_RANGE))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(x = "Age", 
       y = 'Percentage',
       title = "Age Range of Drivers Stopped by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Officer Gender")


p <- ggplot(data = stops, mapping = aes(x = `NUMBER OF PASSENGERS`))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "Number of Passengers by Officer Gender") +
  labs(x = "Number of Passengers", 
       y = 'Percentage',
       title = "Number of Passengers in Vehicle by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Officer Gender")

p <- ggplot(data = stops, mapping = aes(x = DRIVER_RACE))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(title = "") +
  labs(x = "Race", 
       y = 'Percentage',
       title = "Driver Race by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Officer Gender")

```


PARAGRAPH 4


```{graph 3}
p <- ggplot(data = stops, mapping = aes(x = cat_month))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(x = "Month", 
       y = 'Percentage',
       title = "Month of Stop by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Officer Gender") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


p <- ggplot(data = stops, mapping = aes(x = cat_mday))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(x = "Month", 
       y = 'Percentage',
       title = "Day of The Month of Stop by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Officer Gender") +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30))

p <- ggplot(data = stops, mapping = aes(x = cat_wday))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(x = "Day", 
       y = 'Percentage',
       title = "Day of the Week of Stop by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Officer Gender") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),
                     labels = c("Sun", "Mon", "Tues", "Weds", "Thurs", "Fri", "Sat"))

p <- ggplot(data = stops, mapping = aes(x = cat_time))
p + geom_bar(aes(fill = OFFICER_GENDER, y = ..prop.., group = OFFICER_GENDER), position = "dodge") +
  labs(x = "Hour", 
       y = 'Percentage',
       title = "Time of Stop by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Officer Gender") +
  scale_x_discrete(labels = c("0-4", "4-8", "8-12", "12-16", "16-20", "20-24"))


```


PARAGRAPH 5


```{graph 4}
p <- ggplot(data = officer_divisions, mapping = aes(x = OFFICER_GENDER, y = perc))
p + geom_col(aes(fill = ACTIVITY_DIVISION), position = "dodge2") +
  labs(x = "Gender", 
       y = 'Percentage',
       title = "Division Distribution by Officer Gender",
       caption = "Data: LMPD Stops Data (https://data.louisvilleky.gov/dataset/lmpd-stops-data)") +
  scale_fill_discrete(name = "Division")
```


PARAGRAPH 6



CITE DATA SOURCE

