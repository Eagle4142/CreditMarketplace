install.packages("readr")
library(readr)

#check working directory
getwd()

#set seed to 1
set.seed (1)

#read .csv file
creditData <- read_delim("/Users/serhat/Downloads/LCdata.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

#display contents of imported data
View(creditData)

#display structure of data
str(creditData)

#get summary statistics of data
summary(creditData)

#deleting several columns with over 400'000 NA's (more than 50% -> see article below for reasoning)
#https://towardsdatascience.com/7-ways-to-handle-missing-values-in-machine-learning-1a6326adf79e
creditData2 <- subset(creditData,select=-c(inq_last_12m,total_cu_tl,inq_fi,total_rev_hi_lim,all_util,
                                                      max_bal_bc,open_rv_24m,open_rv_12m,il_util,total_bal_il,
                                                      mths_since_rcnt_il,open_il_24m,open_il_12m,open_il_6m,
                                                      open_acc_6m,dti_joint,annual_inc_joint,mths_since_last_major_derog,
                                                      mths_since_last_record,mths_since_last_delinq))
summary(creditData2)

creditData3 <- subset(creditData2,select=-c(collection_recovery_fee,installment,issue_d,last_pymnt_amnt,
                                                      last_pymnt_d,loan_status,next_pymnt_d,out_prncp,out_prncp_inv,
                                                      pymnt_plan,recoveries,term,total_pymnt,total_pymnt_inv,total_rec_int,
                                                      total_rec_late_fee,total_rec_prncp))

summary(creditData3)
creditData3
