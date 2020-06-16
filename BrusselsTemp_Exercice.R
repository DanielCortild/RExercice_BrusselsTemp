#Install and import needed packages
pkg_list = c("readr", "dplyr")
for(pk in pkg_list) {
  pkg <- pkg_list[pk]
  if(!(pkg %in% installed.packages())) {
    install.packages(pkg)
  }
}
library("readr")
library("dplyr")


#Load data
data <- read_csv("BrusselsTemp.csv")

#Array with months name/numbers
months <- c("Jan."=1, "Feb."=2, "Mar."=3, "Apr."=4, "May"=5, "Jun."=6, "Jul."=7, "Aug."=8, "Sep."=9, "Oct."=10, "Nov."=11, "Dec."=12)

#Function transforming Fahrenheit temperatures to Celsius temperatures
fahrenheit <- function(celsius) {
  round(5*(celsius-32)/9, 1)
}

#Transform dataframe
data <- data %>%
  
  #More representative column names
  rename(Month = m,
         Year = y,
         Day = d,
         Temperature = t) %>%
  
  #Transform Month names to numbers, 
  #          Temperatures to numbers and 
  #          Years to full year representation
  mutate(Month = months[Month],
         Temperature = as.numeric(gsub("F", "", Temperature)),
         Year = as.numeric(ifelse(as.numeric(Year) < 50, 
                                  paste("20", Year, sep=""),
                                  paste("19", Year, sep="")))) %>%
  
  #Order the data by the date
  arrange(Year, Month, Day) %>%
  
  #Remove to large or to small data
  mutate(Temperature = ifelse(Temperature>140 | Temperature < -30, 
                              NA, Temperature)) %>%
  
  #Group every month together
  group_by(Year, Month) %>%
  
  #Replace missing data by months average
  mutate(Temperature = ifelse(is.na(Temperature),
                              mean(Temperature, na.rm=T),
                              Temperature)) %>%
  
  #Transform Temperature to Celsius degrees
  mutate(Temperature = fahrenheit(Temperature)) %>%
  
  #Remove grouping
  ungroup() %>%
  
  #Replace Year, Month, Day columns by a single Date colum
  transmute(Date = as.Date(paste(Year, Month, Day, sep="-")),
            Temperature = Temperature)

#Function plotting data for a given Year
getPlot <- function(Year) {
  
  #Filter for data of the specific Year
  yearData <- data %>%
    filter(Date >= as.Date(paste(Year, '-01-01', sep="")) & 
             Date <= as.Date(paste(Year, '-12-31', sep="")))
  
  #Plot the found data
  plot(yearData$Date, 
       yearData$Temperature, 
       main = paste("Temperatures in Brussels in", Year),
       xlab = paste("Months of", Year),
       ylab = "Temperature in Celsius")
}