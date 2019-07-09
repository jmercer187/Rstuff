library(tidyverse)

bus_raw <- read_csv("bus_data.csv")

str(bus_raw)

gender_perc <- bus_raw %>%
  select(Q23) %>%
  filter(!is.na(Q23)) %>%
  group_by(Q23) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

age <- bus_raw %>%
  select(Q24) %>%
  filter(!is.na(Q24) & Q24 != "Refuse") %>%
  transmute(
    age = as.integer(Q24)
  )

ggplot(data = age, mapping = aes(x = age)) +
  geom_histogram() +
  ggtitle("Rider Age Histogram")


age_m <- bus_raw %>%
  filter(Q23 == "Male") %>%
  filter(!is.na(Q24) & Q24 != "Refuse") %>%
  select(Q24) %>%
  transmute(
    age = as.integer(Q24)
  )
age_m <- cut(age_m$age, breaks = c(1, 17, 24, 30, 40, 50, 60, 70, 100),
             labels = c("< 18", "18-24", "25-29", "30-39", "40-49", "50-59", "60-69", "70 +"))
table(age_m)
age_m <- as.data.frame(age_m)
age_m$age <- age_m$age_m
age_m$gender <- "Male"
age_m$age_m <- NULL

age_f <- bus_raw %>%
  filter(Q23 == "Female") %>%
  filter(!is.na(Q24) & Q24 != "Refuse") %>%
  select(Q24) %>%
  transmute(
    age = as.integer(Q24)
  )
age_f <- cut(age_f$age, breaks = c(1, 17, 24, 30, 40, 50, 60, 70, 100),
             labels = c("< 18", "18-24", "25-29", "30-39", "40-49", "50-59", "60-69", "70 +"))
table(age_f)
age_f <- as.data.frame(age_f)
age_f$age <- age_f$age_f
age_f$gender <- "Female"
age_f$age_f <- NULL

age_gender <- rbind(age_m, age_f)


ggplot(data = age_gender, aes(x =age, fill = gender)) +
  geom_bar(data = subset(age_gender, gender == "Female"), color = "grey2") +
  geom_bar(data = subset(age_gender, gender == "Male"), aes(y = ..count..*(-1)), color = "grey2") +
  coord_flip() +
  ggtitle("BUS Rider Pyramid Plot") +
  xlab("Count") +
  ylab("Age")

race_perc <- bus_raw %>%
  select(Q25) %>%
  filter(!is.na(Q25)) %>%
  group_by(Q25) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )

in_home <- bus_raw %>%
  select(Q28, Q29) %>%
  filter(!is.na(Q28) & !is.na(Q29)) %>%
  filter(!Q29 > Q28)


ggplot(data = in_home, mapping = aes(x = Q28)) +
  geom_bar(aes(fill = Q29), color = 'grey2') +
  ggtitle("Household Ridership") +
  xlab("People in Household") +
  ylab("Count") +
  scale_fill_discrete(name = "Individuals Who Ride BUS")

in_home_int <- in_home %>%
  mutate(
    Q28 = if_else(Q28 == "8 or more", "9", Q28),
    Q29 = if_else(Q29 == "8 or more", "9", Q29)
  ) %>%
  transmute(
    in_household = as.integer(Q28),
    riders_in_household = as.integer(Q29)
  )

m <- lm(in_home_int$in_household ~ in_home_int$riders_in_household)
summary(m)

ggplot(data = in_home_int, mapping = aes(x = in_household, y = riders_in_household)) +
  geom_point() +
  geom_smooth()

income_perc <- bus_raw %>%
  select(Q30) %>%
  filter(!is.na(Q30)) %>%
  group_by(Q30) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )  


have_bank_perc <- bus_raw %>%
  select(Q31_1) %>%
  filter(!is.na(Q31_1)) %>%
  group_by(Q31_1) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )  

have_debit_perc <- bus_raw %>%
  select(Q31_2) %>%
  filter(!is.na(Q31_2)) %>%
  group_by(Q31_2) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )  

have_credit_perc <- bus_raw %>%
  select(Q31_3) %>%
  filter(!is.na(Q31_3)) %>%
  group_by(Q31_3) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )  

banked <- bus_raw %>% 
  select(Q31_1, Q31_2, Q31_3) %>%
  filter(Q31_1 == 'Yes' |
           Q31_2 == 'Yes' |
           Q31_3 == 'Yes')

banked_perc <- round((317/493) * 100, 0)



have_mobile_perc <- bus_raw %>%
  select(Q31_4) %>%
  filter(!is.na(Q31_4)) %>%
  group_by(Q31_4) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )  

have_desktop_perc <- bus_raw %>%
  select(Q31_5) %>%
  filter(!is.na(Q31_5)) %>%
  group_by(Q31_5) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )  

have_texts_perc <- bus_raw %>%
  select(Q31_6) %>%
  filter(!is.na(Q31_6)) %>%
  group_by(Q31_6) %>%
  summarize(N = n()) %>%
  mutate(
    freq = N / sum(N),
    perc = round((freq * 100), 0)
  )  

connected <- bus_raw %>% 
  select(Q31_4, Q31_5, Q31_6) %>%
  filter(Q31_4 == 'Yes' |
           Q31_5 == 'Yes' |
           Q31_6 == 'Yes')

connected_perc <- round((351/493) * 100, 0)

