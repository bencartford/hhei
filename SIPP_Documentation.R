library(readstata13)
library(dplyr)
library(data.table)
library(readr)
library(tidyverse)
library(tidyr)

#########
# This function will take in a data.frame and for each column determine if it is a character vector and if so change it to lower case letters
# Inputs - df - the data.frame to be transformed
########
change_lower<-function(df){
  for (i in 1:ncol(df)){
    if (class(df[[i]]) == "character" | class(df[[i]]) == "factor"){
      df[[i]]<-tolower(df[[i]])
    }
  }
  return(df)
}


# INPUT: ADD THE DIRECTORY BELOW
# THE DATA MUST BE HOUSED IN THE FOLDER "SIPP_DATA" AND SORTED INTO
# SUBFOLDERS TITLED "SIPPXXXX" WHERE XXXX IS THE FIRST YEAR OF THAT SIPP
# THEN USE CONTROL F TO REPLACE ALL INSTANCES OF /Users/owner/Downloads/SIPP_DATA WITH YOUR DIRECTORY LOCATION
data_location<-"/Users/owner/Downloads/SIPP_DATA"


###############
#2014

#Set the list of variables you want from this SIPP:
variable_list_core_1<-c("ssuid","PNUM","shhadid","swave","monthcode","wpfinwgt","TAGE", "ESEX", "EMS", "ERACE", "EEDUC",
                        "EEVERET","EDISABL","RGED", "TJB1_WKSUM1","TJB1_JOBHRS1","TJB1_JOBHRS2","TJB1_JOBHRS3","TJB2_JOBHRS1","TJB2_JOBHRS2","TJB2_JOBHRS3","TJB3_JOBHRS1","TJB3_JOBHRS2","TJB3_JOBHRS3","TJB4_JOBHRS1","TJB4_JOBHRS2","TJB4_JOBHRS3",
                        "TJB5_JOBHRS1","TJB5_JOBHRS2","TJB5_JOBHRS3","TJB6_JOBHRS1","TJB6_JOBHRS2","TJB6_JOBHRS3","TJB7_JOBHRS1","TJB7_JOBHRS2","TJB7_JOBHRS3",
                        "TJB1_MSUM","TJB2_MSUM","TJB3_MSUM","TJB4_MSUM","TJB5_MSUM","TJB6_MSUM","TJB7_MSUM","EJB1_STRTMON","TJB1_STRTYR","EJB1_BMONTH","EJB2_STRTMON","TJB2_STRTYR","EJB2_BMONTH","EJB3_STRTMON"
                        ,"TJB3_STRTYR","EJB3_BMONTH","EJB4_STRTMON","TJB4_STRTYR","EJB4_BMONTH","EJB5_STRTMON","TJB5_STRTYR","EJB5_BMONTH",
                        "EJB6_STRTMON","TJB6_STRTYR","EJB6_BMONTH","EJB7_STRTMON","TJB7_STRTYR","EJB7_BMONTH",
                        "RWKESR1", "RWKESR2", "RWKESR3", "RWKESR4", "RWKESR5", "EJB1_JBORSE","EJB2_JBORSE","EJB3_JBORSE","EJB4_JBORSE","EJB5_JBORSE","EJB6_JBORSE",
                        "EJB7_JBORSE", 
                        "EJB1_BSLRYB","EJB2_BSLRYB","EJB3_BSLRYB","EJB4_BSLRYB","EJB5_BSLRYB","EJB6_BSLRYB","EJB7_BSLRYB", 
                        "EJB1_EMONTH","EJB2_EMONTH",   "tjb1_ind","tjb2_ind","tjb3_ind","tjb4_ind","tjb5_ind","tjb6_ind","tjb7_ind", "tjb1_occ","tjb2_occ","tjb3_occ",
                        "tjb4_occ","tjb5_occ","tjb6_occ","tjb7_occ", "TJB1_TXAMT", "TJB2_TXAMT","EJB1_TXMTH", "EJB2_TXMTH",
                        "EJB1_CHEARN1","EJB1_CHEARN2","EJB2_CHEARN1","EJB2_CHEARN2","EJB1_CHHOUR1","EJB1_CHHOUR2","EJB2_CHHOUR1","EJB2_CHHOUR2","EJB1_PAYHR1","EJB1_PAYHR2","EJB1_PAYHR3","EJB2_PAYHR1","EJB2_PAYHR2","EJB2_PAYHR3",
                        "TJB1_HOURLY1","TJB1_HOURLY2","TJB1_HOURLY3","TJB2_HOURLY1","TJB2_HOURLY2","TJB2_HOURLY3","TJB3_HOURLY1","TJB3_HOURLY2","TJB3_HOURLY3","TJB4_HOURLY1",
                        "TJB4_HOURLY2","TJB4_HOURLY3","TJB5_HOURLY1","TJB5_HOURLY2","TJB5_HOURLY3","TJB6_HOURLY1","TJB6_HOURLY2","TJB6_HOURLY3","TJB7_HOURLY1","TJB7_HOURLY2","TJB7_HOURLY3",
                        "RHIMTH","TBSJ1VAL","TBSJ1DEBTVAL","TBSJ2VAL","TBSJ2DEBTVAL")

variable_list<-c("SSUID","PNUM","SHHADID","SWAVE","MONTHCODE","WPFINWGT","TAGE", "ESEX", "EMS", "ERACE", "EEDUC",
                 "EEVERET","EDISABL","RGED", "TJB1_WKSUM1","TJB1_JOBHRS1","TJB1_JOBHRS2","TJB1_JOBHRS3","TJB2_JOBHRS1","TJB2_JOBHRS2","TJB2_JOBHRS3","TJB3_JOBHRS1","TJB3_JOBHRS2","TJB3_JOBHRS3","TJB4_JOBHRS1","TJB4_JOBHRS2","TJB4_JOBHRS3","TJB5_JOBHRS1","TJB5_JOBHRS2","TJB5_JOBHRS3","TJB6_JOBHRS1","TJB6_JOBHRS2","TJB6_JOBHRS3","TJB7_JOBHRS1","TJB7_JOBHRS2","TJB7_JOBHRS3",
                 "TJB1_MSUM","TJB2_MSUM","TJB3_MSUM","TJB4_MSUM","TJB5_MSUM","TJB6_MSUM","TJB7_MSUM","EJB1_STRTMON","TJB1_STRTYR","EJB1_BMONTH","EJB2_STRTMON","TJB2_STRTYR","EJB2_BMONTH","EJB3_STRTMON","TJB3_STRTYR","EJB3_BMONTH","EJB4_STRTMON","TJB4_STRTYR","EJB4_BMONTH","EJB5_STRTMON","TJB5_STRTYR","EJB5_BMONTH",
                 "EJB6_STRTMON","TJB6_STRTYR","EJB6_BMONTH","EJB7_STRTMON","TJB7_STRTYR","EJB7_BMONTH","TJB1_TXAMT", "TJB2_TXAMT","EJB1_TXMTH", "EJB2_TXMTH",
                 "RWKESR1", "RWKESR2", "RWKESR3", "RWKESR4", "RWKESR5", "EJB1_JBORSE","EJB2_JBORSE","EJB3_JBORSE","EJB4_JBORSE","EJB5_JBORSE","EJB6_JBORSE","EJB7_JBORSE", 
                 "EJB1_BSLRYB","EJB2_BSLRYB","EJB3_BSLRYB","EJB4_BSLRYB","EJB5_BSLRYB","EJB6_BSLRYB","EJB7_BSLRYB",   "TJB1_IND","TJB2_IND","TJB3_IND","TJB4_IND","TJB5_IND","TJB6_IND","TJB7_IND", "TJB1_OCC","TJB2_OCC","TJB3_OCC","TJB4_OCC","TJB5_OCC","TJB6_OCC","TJB7_OCC", 
                 "EJB1_EMONTH","EJB2_EMONTH","EJB1_CHEARN1","EJB1_CHEARN2","EJB2_CHEARN1","EJB2_CHEARN2","EJB1_CHHOUR1","EJB1_CHHOUR2","EJB2_CHHOUR1","EJB2_CHHOUR2","EJB1_PAYHR1","EJB1_PAYHR2","EJB1_PAYHR3","EJB2_PAYHR1","EJB2_PAYHR2","EJB2_PAYHR3",
                 "TJB1_HOURLY1","TJB1_HOURLY2","TJB1_HOURLY3","TJB2_HOURLY1","TJB2_HOURLY2","TJB2_HOURLY3","TJB3_HOURLY1","TJB3_HOURLY2","TJB3_HOURLY3","TJB4_HOURLY1","TJB4_HOURLY2","TJB4_HOURLY3","TJB5_HOURLY1","TJB5_HOURLY2","TJB5_HOURLY3","TJB6_HOURLY1","TJB6_HOURLY2","TJB6_HOURLY3","TJB7_HOURLY1","TJB7_HOURLY2","TJB7_HOURLY3",
                 "RHIMTH","TBSJ1VAL","TBSJ1DEBTVAL","TBSJ2VAL","TBSJ2DEBTVAL")

# Load the data and select the chosen variables, note that Core 1 has a different set of names than the other three cores

Core_1<-fread("/Users/owner/Downloads/SIPP_DATA/SIPP2014/PU2014W1.csv")[,..variable_list_core_1]
Core_2<-fread("/Users/owner/Downloads/SIPP_DATA/SIPP2014/PU2014W2.csv")[,..variable_list]
Core_3<-fread("/Users/owner/Downloads/SIPP_DATA/SIPP2014/PU2014W3.csv")[,..variable_list]
Core_4<-fread("/Users/owner/Downloads/SIPP_DATA/SIPP2014/PU2014W4.csv")[,..variable_list]


# Add in the variable rhcalyr to each core.
Core_1$RHCALYR<-2013
Core_2$RHCALYR<-2014
Core_3$RHCALYR<-2015
Core_4$RHCALYR<-2016

#The first core in 2014 has naming conventions that aren't standard. Many of the names are not fully capitalized. In the next lines we change all the column headers to uppercase
Core_1<-data.table(Core_1)
setnames(Core_1,toupper(names(Core_1)))


# Bind together all the Cores
SIPP2014_full<-bind_rows(Core_1,Core_2,Core_3,Core_4)
SIPP2014_full<-data.table(SIPP2014_full)

#Make all the column names lowercase to match the earlier sipps
setnames(SIPP2014_full,tolower(names(SIPP2014_full)))

#Change the variables monthcode and eeduc to the naming from 2008 and prior rhcalmn and eeducate
SIPP2014_full<-SIPP2014_full%>%rename(rhcalmn=monthcode,eeducate=eeduc)

#create the csv file containing all the selected data
write.csv(SIPP2014_full,"/Users/owner/Downloads/SIPP_Data/SIPP2014_stack.csv")

#remove the data we no longer need
rm(Core_1,Core_2,Core_3,Core_4,SIPP2014_full,variable_list,variable_list_core_1)








SIPP2014_stack <- read.csv("/Users/owner/Downloads/SIPP_DATA/SIPP2014_stack_forrep.csv")


#Selecting data we want from the SIPP Stack +
#Variable Definitions: 
#ssuid = address identifier, pnum = person identifier for each address, swave: year (1,2,3,4) person is responding, tage = age, tjb1_msum.....tjb7_msum = monthly earnings from job 1 - job 7
SIPP_Income_Data <- data.table(ssuid = SIPP2014_stack$ssuid,  sex = SIPP2014_stack$esex, household_members = SIPP2014_stack$rhnumper,  pnum= SIPP2014_stack$pnum, swave = SIPP2014_stack$swave, tage = SIPP2014_stack$tage, job1 = SIPP2014_stack$tjb1_msum, job2 = SIPP2014_stack$tjb2_msum, job3 = SIPP2014_stack$tjb3_msum, job4 = SIPP2014_stack$tjb4_msum, job5 = SIPP2014_stack$tjb5_msum, job6 = SIPP2014_stack$tjb6_msum, job7 = SIPP2014_stack$tjb7_msum)

#Summing monthly earnings from all jobs to yearly level and get rid of rows where income reported is NA (but keep income = 0):
SIPP_Income_Data$alljobs <- rowSums(SIPP_Income_Data[,7:13],na.rm = TRUE)
SIPP_Income_Data$countna <- rowSums(is.na(SIPP_Income_Data[,7:13]))
SIPP_Income_Data <- SIPP_Income_Data %>% filter(countna < 7)


#Concatenate two columns to make person identifier:
SIPP_Income_Data$Cat <- gsub(" ", "_", paste(SIPP_Income_Data$ssuid, SIPP_Income_Data$pnum))
SIPP_Income_Data$catage <- gsub(" ", "", paste(SIPP_Income_Data$Cat, SIPP_Income_Data$tage))
#sum all months so that we are on a yearly level
SIPP_Income_Data[,alljobs_yearly:=sum(alljobs, na.rm = TRUE), by= .(Cat, tage)] #and by age
SIPP_Income_Data_un <- unique(SIPP_Income_Data,by= ("catage"))


#Go to this link https://www.census.gov/programs-surveys/sipp/data/datasets/2014-panel/wave-4.html and download the Longitudinal Weights File 
#Make sure it's in a table format before running this script (title the df "weights")
#Concatenating this as well to prepare merge
weights <- read.table("~/Downloads/LGTWGT2014PNL4.csv", header=TRUE, sep="|")
weights$cat <- gsub(" ", "_", paste(weights$ssuid, weights$pnum))


#Bringing weights into our data:
df_sipp = data.frame(cat = SIPP_Income_Data_un$Cat, alljobs_yearly = SIPP_Income_Data_un$alljobs_yearly, tage = SIPP_Income_Data_un$tage, swave = SIPP_Income_Data_un$swave)
df_weight = data.frame(cat = weights$cat,finpnl4 = weights$finpnl4)
Weighted_Data <- merge(x = df_sipp, y = df_weight, by.x = "cat", by.y = "cat", all.x = TRUE)

#Getting rid of NA Weighting
Weighted_Data <- Weighted_Data %>% filter(!is.na(finpnl4))


#Making our table for average (weighted) income over age:
Weighted_Data <- data.table(Weighted_Data)
Average_Income <- Weighted_Data[,.(mean_income=weighted.mean(alljobs_yearly, finpnl4)), by= tage]

#Make a simple plot of the data:
library(ggplot2)
ggplot(Average_Income, aes(Average_Income$tage, Average_Income$mean_income)) + geom_point() + ggtitle("Raw Average Income vs. Age") + xlab("Age") + ylab("Average Income")









#Now let's create a table that can tell us the average change in income experienced
#between two ages (EG: the average 27 year old will earn $X,XXX more dollars than they did last year)
#buckle up because i probably wrote the worst way to do this :')

#First let's get 2 columns: one that says ssuid, pnum, and age; and one that says
#ssuid, pnum, and age last year
Weighted_Data$catage <- gsub(" ", "", paste(Weighted_Data$cat, Weighted_Data$tage))
Weighted_Data$lastage <- Weighted_Data$tage - 1
Weighted_Data$last_age_cat <- gsub(" ", "", paste(Weighted_Data$cat, Weighted_Data$lastage))

#now let's use these concatenated columns to have each individual's income this year and
#last year be in the same row (CHECK,,, the labels are wrong rn)

df_last = data.frame(last = Weighted_Data$last_age_cat, currentincome = Weighted_Data$alljobs_yearly, perweight = Weighted_Data$finpnl4, age = Weighted_Data$tage)
df_now = data.frame(current = Weighted_Data$catage, lastincome = Weighted_Data$alljobs_yearly)
SIPP_Income_Diff <- merge(x = df_now, y= df_last, by.x = "current", by.y = "last", all.x = TRUE)
#remove na values so that we retain continuity:
SIPP_Income_Diff <- SIPP_Income_Diff %>% filter(!is.na(currentincome))


#how each individual's income changed this year
SIPP_Income_Diff$income_difference <- SIPP_Income_Diff$currentincome - SIPP_Income_Diff$lastincome
SIPP_Income_Diff <- data.table(SIPP_Income_Diff)


#creating a table with average income change by age
Average_Income_Diff <- SIPP_Income_Diff[,.(mean_income=weighted.mean(income_difference, perweight)), by= age]


#adding a column that sums each income change as we increase age 
#you need to set a start age -- I use age = 20 where average weighted income was calculated to be 8587 (thus im removing age <20)
Average_Income_Diff <- Average_Income_Diff[order(age),]
Average_Income_Diff <- Average_Income_Diff[-c(1, 2, 3, 4), ]
start_age_income = 8587
Average_Income_Diff[1,2] = start_age_income
Average_Income_Diff$cumsum <- cumsum(Average_Income_Diff$mean_income)


#plotting the results of this table:
ggplot(Average_Income_Diff, aes(Average_Income_Diff$age, Average_Income_Diff$cumsum)) + geom_point() + ggtitle("Aggregated Average Change in Income vs. Age") + xlab("Age") + ylab("Aggregated Average Changein Income")







#men vs women earnings
#how to split into 2 df, one where esex = 1 and esex = 2

df_sipp = data.frame(sex = SIPP_Income_Data_un$sex, cat = SIPP_Income_Data_un$Cat, alljobs_yearly = SIPP_Income_Data_un$alljobs_yearly, tage = SIPP_Income_Data_un$tage, swave = SIPP_Income_Data_un$swave)
df_weight = data.frame(cat = weights$cat,finpnl4 = weights$finpnl4)
Weighted_Data <- merge(x = df_sipp, y = df_weight, by.x = "cat", by.y = "cat", all.x = TRUE)

#Getting rid of NA Weighting
Weighted_Data <- Weighted_Data %>% filter(!is.na(finpnl4))

df1 <- Weighted_Data %>% filter(Weighted_Data$sex ==1)
df2 <- Weighted_Data %>% filter(Weighted_Data$sex ==2)
df1 <-data.table(df1)
df2 <- data.table(df2)
sexincome_m <- df1[,.(mean_income=weighted.mean(df1$alljobs_yearly, df1$finpnl4)),]
sexincome_f <- df2[,.(mean_income=weighted.mean(df2$alljobs_yearly, df2$finpnl4)),]
#found that women avg is 70 percent of mens






#poverty
povertythresholds <- read.csv("~/Downloads/SIPP_DATA/povertythresholdsuse.csv")

#get number from each household in here
df_sipp = data.frame(ssuid = SIPP_Income_Data_un$ssuid, number =SIPP_Income_Data_un$household_members, sex = SIPP_Income_Data_un$sex, cat = SIPP_Income_Data_un$Cat, alljobs_yearly = SIPP_Income_Data_un$alljobs_yearly, tage = SIPP_Income_Data_un$tage, swave = SIPP_Income_Data_un$swave)
df_weight = data.frame(cat = weights$cat,finpnl4 = weights$finpnl4)
Weighted_Data <- merge(x = df_sipp, y = df_weight, by.x = "cat", by.y = "cat", all.x = TRUE)
Weighted_Data <- Weighted_Data %>% filter(!is.na(finpnl4))

#separate into waves (wave1 = 2014, ... , wave4 = 2017)
df14 <- Weighted_Data %>% filter(Weighted_Data$swave ==1)
df15 <- Weighted_Data %>% filter(Weighted_Data$swave ==2)
df16 <- Weighted_Data %>% filter(Weighted_Data$swave ==3)
df17 <- Weighted_Data %>% filter(Weighted_Data$swave ==4)

#idk how to do functions so
df14$number[df14$number>8] <- 9
df15$number[df15$number>8] <- 9
df16$number[df16$number>8] <- 9
df17$number[df17$number>8] <- 9

#joining the poverty threshold data
df14m = data.frame(cat = df14$cat, ssuid = df14$ssuid, number = df14$number, alljobs_yearly = df14$alljobs_yearly,finpnl14= df14$finpnl4)
pt = data.frame(number1= povertythresholds$number, threshold = povertythresholds$X2014_threshold)
merge14 <- merge(x = df14m, y = pt, by.x = "number", by.y = "number1", all.x = TRUE)

df15m = data.frame(df15$cat, df15$ssuid, number =df15$number, df15$alljobs_yearly, df15$finpnl4)
pt = data.frame(number1 =povertythresholds$number, povertythresholds$X2015_threshold)
merge15 <- merge(x = df15m, y = pt, by.x = "number", by.y = "number1", all.x = TRUE)

df16m = data.frame(df16$cat, df16$ssuid, number = df16$number, df16$alljobs_yearly, df16$finpnl4)
pt = data.frame(number1 = povertythresholds$number, povertythresholds$X2016_threshold)
merge16 <- merge(x = df16m, y = pt, by.x = "number", by.y = "number1", all.x = TRUE)

df17m = data.frame(df17$cat, df17$ssuid, number = df17$number, df17$alljobs_yearly, df17$finpnl4)
pt = data.frame(number1 = povertythresholds$number, povertythresholds$X2017_threshold)
merge17 <- merge(x = df17m, y = pt, by.x = "number", by.y = "number1", all.x = TRUE)


#sum across ssuid (household)
merge14 <- data.table(merge14)
merge14 <- merge14[,famsum:=sum(alljobs_yearly, na.rm = TRUE), by= ssuid] #and by age
merge14 <- unique(merge14,by= ("ssuid"))
for (row in 1:length(merge14$number)) {
merge14$poverty[row] <- if (merge14$famsum[row] > merge14$threshold[row]) {0} else {1}
}

#add a column that says whether the family is in poverty


weighted.mean(df14$alljobs_yearly, df14$finpnl4)
weighted.mean(df15$alljobs_yearly, df15$finpnl4)
weighted.mean(df16$alljobs_yearly, df16$finpnl4)
weighted.mean(df17$alljobs_yearly, df17$finpnl4)
mean(df14$alljobs_yearly)
mean(df15$alljobs_yearly)
mean(df16$alljobs_yearly)
mean(df17$alljobs_yearly)

