# https://www.youtube.com/watch?v=C4N3_XJJ-jU

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header=F)
head(data,5)

colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
)

head(data)
str(data)

# change "?" to NA
data[data == "?"] <- NA

# convert 0 and 1 to female and male and convert col to factor
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

# convert other cols into factors that are supposed to be factors
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

# bc it has "?" in it, R thinks ca has strings, not ints like it should
data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

# do the same thing for thal
data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

# convert hd (heart disease) to a reader friendly factor
data$hd <- ifelse(data$hd == 0, "Healthy", "Unhealthy")
data$hd <- as.factor(data$hd)

str(data)

# see how many rows have NA values
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]

# just remove these instead of imputing new values

nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data)

# need to get a sample where both men and women have heart disease
# create a table to check and see if that's true
# ** do this for all variables that we're gonna use to predict heart disease
xtabs(~ hd + sex, data = data)

# note on xtabs; creates a table using formula syntax

# check to see that all 4 levels of chest pain (cp) are reported
xtabs(~ hd + cp, data = data)
xtabs( ~ hd + fbs, data = data)
xtabs( ~ hd + restecg, data = data)
# restecg could cause trouble .. leave it for now
xtabs( ~ hd + exang, data = data)
xtabs( ~ hd + slope, data = data)
xtabs( ~ hd + ca, data = data)
xtabs( ~ hd + thal, data = data)

# START WITH A SIMPLE MODEL
# predict heart disease only using gender

logistic <- glm(hd ~ sex, data = data, family = "binomial")
summary(logistic)

# NOW LETS USE THEM ALL

logistic <- glm(hd ~ ., data = data, family = "binomial")
summary(logistic)


