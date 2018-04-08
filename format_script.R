library(xts)
library(tidyverse)
#import
#Note file wd and file path saved separately
setwd(wd)
payroll_p1<-read.csv(path,header=TRUE)
#formatting
payroll_p1$Date<-as.Date(payroll_p1$Date,format="%d/%m/%Y")
payroll_p1$Hours<-as.numeric(payroll_p1$Hours)
payroll_p1$Department<-as.character(payroll_p1$Department)
#Removing employee information and notes, leaving only Date, Dept, Hrs
payroll_p1<-payroll_p1[,c(1,5,8)]
#group by dept, sum up per day, spread dataset
payroll_sum<-payroll_p1 %>%
  group_by(Date,Department) %>%
  summarize(TotalHrs=sum(Hours)) %>%
  spread(Department,TotalHrs)
#reformatting some column names
names(payroll_sum)[2]<-"dotNet"
names(payroll_sum)[4]<-"FrontEnd"
#replace NAs with 0
payroll_sum[is.na(payroll_sum)]<-0
#reformat as xts
payroll_sum$Date<-as.Date(payroll_sum$Date,format="%d/%m/%Y")
payroll_ts<-xts(payroll_sum[,-1],order.by=as.Date(payroll_sum$Date,"%d/%m/%Y"))
#summarize weekly, monthly
payroll_weekly<-apply.weekly(payroll_ts,colSums)
payroll_monthly<-apply.monthly(payroll_ts,colSums)
#transform back to df
payroll_weekly<-as.data.frame(payroll_weekly)
payroll_monthly<-as.data.frame(payroll_monthly)
#create long sets,dummy ests and differences
weekly_long<-payroll_weekly %>%
  rownames_to_column(var="Date") %>%
  gather(Dept,Hrs,2:6) %>%
  mutate(d_Est= Hrs*(1+rnorm(length(Hrs),0,0.4)))%>%
  mutate(Var=Hrs-d_Est) %>%
  mutate(Date=as.Date(Date,format="%Y-%m-%d"))
monthly_long<-payroll_monthly %>%
  rownames_to_column(var="Date") %>%
  gather(Dept,Hrs,2:6) %>%
  mutate(d_Est= Hrs*(1+rnorm(length(Hrs),0,0.4)))%>%
  mutate(Var=Hrs-d_Est) %>%
  mutate(Date=as.Date(Date,format="%Y-%m-%d"))
#export files
saveRDS(weekly_long,file="./data/project_hrs_weekly_long.rds")
saveRDS(monthly_long,file="./data/project_hrs_monthly_long.rds")
