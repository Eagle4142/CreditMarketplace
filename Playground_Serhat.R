library(readr)
library(openxlsx)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(car)
library(lmtest)
library(dbplyr)

#check working directory
getwd()
#set seed to 1
set.seed (1)
#read .csv file
creditDataSerhat <- read_delim("/Users/serhat/Downloads/LCdata.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

creditDataSerhat2 <- subset(creditDataSerhat,select=-c(annual_inc_joint,
                                                       collection_recovery_fee,
                                                       desc,
                                                       dti_joint,
                                                       emp_title,
                                                       id,
                                                       inq_last_6mths,
                                                       installment,
                                                       issue_d,
                                                       last_pymnt_amnt,
                                                       last_pymnt_d,
                                                       loan_status,
                                                       member_id,
                                                       mths_since_last_delinq,
                                                       mths_since_last_major_derog,
                                                       mths_since_last_record,
                                                       next_pymnt_d,
                                                       out_prncp,
                                                       out_prncp_inv,
                                                       policy_code,
                                                       pymnt_plan,
                                                       recoveries,
                                                       term,
                                                       total_acc,
                                                       total_pymnt,
                                                       total_pymnt_inv,
                                                       total_rec_int,
                                                       total_rec_late_fee,
                                                       total_rec_prncp,
                                                       url,
                                                       open_acc_6m,
                                                       open_il_6m,
                                                       open_il_12m,
                                                       open_il_24m,
                                                       mths_since_rcnt_il,
                                                       total_bal_il,
                                                       il_util,
                                                       open_rv_12m,
                                                       open_rv_24m,
                                                       max_bal_bc,
                                                       all_util,
                                                       total_rev_hi_lim,
                                                       inq_fi,
                                                       total_cu_tl,
                                                       inq_last_12m))

###Data Preprocessing revol_util
creditDataSerhat3 <- filter(creditDataSerhat2, ! is.na(revol_util))
creditDataSerhat4 <- creditDataSerhat3
creditDataSerhat4$title <- tolower(creditDataSerhat4$title)

###Data Preprocessing title
#How about titles? Is there one title that dominates the dataset?
max(table(creditDataSerhat4$title))
HäufigkeitTitle <- round(prop.table(table(creditDataSerhat4$title))*100,2)
#Häufigkeitemptitle <- round(prop.table(table(creditDataSerhat2$emp_title))*100,2)
tabelle <- cbind(HäufigkeitTitle)
# Berechnen Sie die Häufigkeit jeder Kategorie
kategorie_haeufigkeit <- table(creditDataSerhat4$title)
# Sortieren Sie die Tabelle absteigend nach Häufigkeit
sortierte_haeufigkeit <- sort(kategorie_haeufigkeit, decreasing = TRUE)
# Erstellen Sie eine Top-10-Liste
top30 <- head(sortierte_haeufigkeit, 30)
# Erstellen Sie einen Vektor mit den Namen der Top-10-Kategorien
top30_namen <- names(top30)
# Ersetzen Sie alle anderen Kategorien mit 'andere'
creditDataSerhat4$title <- ifelse(creditDataSerhat4$title %in% top30_namen, creditDataSerhat4$title, 'andere')
# Überprüfen Sie das Ergebnis
table(creditDataSerhat4$title)
creditDataSerhat4$title <- factor("title")

###Date preprocessing zip_code
# in welchen Staaten die zip codes auftauchen
tab <- table(creditDataSerhat4$zip_code, creditDataSerhat4$addr_state)
# Umwandlung in ein Data Frame und Anzeige im Viewer
#Alle Werte in der variable addr_state
einzigartige_werte <- unique(creditDataSerhat4$addr_state)
# Drucke die einzigartigen Werte
print(einzigartige_werte)
einzigartige_werte2 <- unique(creditDataSerhat4$zip_code)
print(einzigartige_werte2)
#Kombination aus zip_code und Staat
# Zuerst extrahieren Sie die ersten drei Zeichen des ZIP-Codes
creditDataSerhat4$zip_code <- substr(creditDataSerhat4$zip_code, 1, 3)
# Dann fügen Sie diese mit den Staatswerten zusammen, um die neue Spalte 'new_zip' zu erstellen
creditDataSerhat4$combo_zip_addr <- paste0(creditDataSerhat4$zip_code, creditDataSerhat4$addr_state)
# Anzeigen des DataFrames, um die Änderungen zu überprüfen
print(creditDataSerhat4)
creditDataSerhat5 <- subset(creditDataSerhat4,select=-c(zip_code,
                                                       addr_state))
###Data preprocessing tot_coll_amt
zero_in_spalte1 <- sum(creditDataSerhat5$tot_coll_amt == 0, na.rm = TRUE)
print(zero_in_spalte1)

###Data preprocessing tot_coll_bal

###Data preprocessing acc_now_delinq
grösser0 <- sum(creditDataSerhat5$acc_now_delinq > 0, na.rm = TRUE)
print(grösser0)

# Plotting in base R
plot(creditDataSerhat5$tot_coll_amt, creditDataSerhat5$tot_coll_amt,
     xlab = "Another Variable", 
     ylab = "Total Collection Amount",
     main = "Scatter plot of Total Collection Amount vs Another Variable",
     pch = 20,
     col = "blue")

# Plotting in base R
plot(creditDataSerhat5$tot_cur_bal, creditDataSerhat5$tot_cur_bal,
     xlab = "Another Variable", 
     ylab = "Total Collection Amount",
     main = "Scatter plot of Total Collection Amount vs Another Variable",
     pch = 20,
     col = "blue")




















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

