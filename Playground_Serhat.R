library(readr)
library(openxlsx)
library(tidyverse)

#check working directory
getwd()

#set seed to 1
set.seed (1)

#read .csv file
creditDataSerhat <- read_delim("/Users/serhat/Downloads/LCdata.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#display contents of imported data
View(creditDataSerhat)

#display structure of data
str(creditDataSerhat)

#get summary statistics of data
summary(creditDataSerhat)

#transform categorical to numerical variables2
x <- factor(c("addr_state","application_type","desc","earliest_cr_line","emp_length","emp_title","home_ownership","initial_list_status","purpose",
              "pymnt_plan","term","title","url","verified_status_joint","zip_code"))
# Die Kategorien in Ganzzahlen umwandeln
x_as_integer <- as.integer(x)

# Eine Tabelle erstellen, die die ursprünglichen Kategorien und die Ganzzahlen enthält
data_frame <- data.frame(Original_Kategorie = levels(x), Ganzzahl_Wert = x_as_integer)

# Die erstellte Tabelle anzeigen
print(data_frame)

#deleting several columns with over 400'000 NA's
#creditDataSerhat <- subset(creditDataSerhat,select=-c(inq_last_12m,total_cu_tl,inq_fi,total_rev_hi_lim,all_util,
                                                      #max_bal_bc,open_rv_24m,open_rv_12m,il_util,total_bal_il,
                                                      #mths_since_rcnt_il,open_il_24m,open_il_12m,open_il_6m,
                                                      #open_acc_6m,dti_joint,annual_inc_joint,mths_since_last_major_derog,
                                                      #mths_since_last_record,mths_since_last_delinq))
#creditDataSerhat
#str(creditDataSerhat)
