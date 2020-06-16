## IMPORTANT
# Download the "city_temperature.csv" file from https://www.kaggle.com/sudalairajkumar/input

#Install packages if needed
pkg_list = c("readr", "dplyr")
for(pk in pkg_list) {
  pkg <- pkg_list[pk]
  if(!(pkg %in% installed.packages())) {
    install.packages(pkg)
  }
}

#Import libraries
library("readr")
library("dplyr")


#Transform "Good" Data to "Bad" data
data <- read_csv("city_temperature.csv") %>%
  filter(City == "Brussels") %>%
  select(Year, Day, Month, AvgTemperature) %>%
  mutate(AvgTemperature = paste(AvgTemperature, "F", sep=""))
for(i in 1:600) {
  randD <- round(runif(1, 1, 28))
  randM <- round(runif(1, 1, 12))
  randY <- round(runif(1, 1995, 2020))
  data <- data %>%
    mutate(AvgTemperature = 
             ifelse(Year == randY & Month == randM & Day == randD,
                    NA, AvgTemperature))
}
for(i in 1:600) {
  randD <- round(runif(1, 1, 28))
  randM <- round(runif(1, 1, 12))
  randY <- round(runif(1, 1995, 2020))
  randT <- round(runif(1, 150, 200), 1) * (round(runif(1, 0, 1))*2-1)
  randT <- paste(randT, "F", sep="")
  data <- data %>%
    mutate(AvgTemperature = 
             ifelse(Year == randY & Month == randM & Day == randD,
                    randT, AvgTemperature))
}
data <- data %>%
  transmute(m = as.factor(Month), 
            y = substring(as.character(Year), 3), 
            d = as.factor(Day),
            t = AvgTemperature)
monthAbb <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")
data <- data %>% 
  mutate(m = monthAbb[m])
data <- data[sample(nrow(data)),]

#Write "bad" data to a file
write_csv(data, "BrusselsTemp_Data.csv")