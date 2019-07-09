library(tidyverse)

bus_raw <- read_csv("bus_data.csv")

route <- bus_raw %>%
  select(Q5Start, Q5End)
