install.packages("readr")
library(readr)

#check working directory
getwd()

#set seed to 1
set.seed (1)

#read .csv file
creditData <- read_delim("LCdata.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

#display contents of imported data
View(creditData)

#display structure of data
str(creditData)

#get summary statistics of data
summary(creditData)

#new Test Serhat