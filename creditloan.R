install.packages("tidyverse")
library(tidyverse)

#check working directory
getwd()

#import .csv file
creditData <- read.csv("LCdata.csv", header = TRUE, sep =",")
creditData
