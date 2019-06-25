library(tidyverse)
library(data.table)
library(lubridate)

stops_raw <- fread('source_data/LMPD_STOPS_DATA_12.csv')

stops <- stops_raw %>%
  filter(CITATION_CONTROL_NUMBER != '') %>%
  filter(ACTIVITY_LOCATION != '') %>%
  mutate(
    ACTIVITY_DATE = dmy(ACTIVITY_DATE),
    DAY = wday(ACTIVITY_DATE, label = T)
  ) %>%
  filter(!is.na(ACTIVITY_DATE))


stops %>%
  filter(DRIVER_RACE != '') %>%
  filter(OFFICER_RACE != '' & OFFICER_RACE != 'OTHER') %>%
  ggplot() +
  geom_bar(aes(x = DAY, y = (..count..)/sum(..count..), 
               fill = DRIVER_RACE)) +
  facet_grid(~OFFICER_RACE)
