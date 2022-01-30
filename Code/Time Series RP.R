library(forecast)
library(TTR)
library(tseries)
library(dplyr)

#Import Data

df =  read.csv("C:\\Users\\ADMIN\\OneDrive\\Desktop\\New folder\\IM201994 - Sales Data.csv")


head(df)


impds = df %>% select(Period, ItemTn)           # Subset with select function and shortlist the important columns

impds = aggregate(.~Period,data=impds,FUN=sum) # Sum all the values in the same Period

head(impds)  # View Top 6 Rows
tail(impds)  # View Bottom 6 Rows



impds  ## Cutting short the table to important dataset

##_________Splitting_The_Data_into_5_parts_____________________________________

impds1 = impds[1:56, ]  ##-Before Covid 1st Lockdown from July 15 to Feb 20

head(impds1)  ##View Top 6 ROWS
tail(impds1)  ##View Bottom 6 ROWS

##Convert Into Time Series Data
sale_ts = ts(impds1$ItemTn,start = c(2015,7),end = c(2020,2),frequency = 12)
sale_ts

plot(sale_ts) ##Plot Time Series Data

decompose(sale_ts) ##Decompose to Observe Trend and Seasonality
plot(decompose(sale_ts)) ##Plot the Decomposition

adf.test(sale_ts) ## Augmented Dicky Fuller Test 


HW_temp_forecast = HoltWinters(sale_ts) ## Fitting the data into Holts Winter
HW_temp_forecast 
plot(HW_temp_forecast) ##Plot Holts Winter Fit


HW_pred_model = forecast:::forecast(HW_temp_forecast,h=4) ##Forecast of next 4 Months
HW_pred_model
plot(HW_pred_model) ##Plot Forecast


impds12 = as.data.frame(HW_pred_model) ##Convert the Forecast values to Dataframe


colnames(impds12) <- c('ItemTn','a','b','c','d') ##Change the names of the columns


impds12 = subset(impds12, select = -c(a,b,c,d) ) ##Delete unwanted Columns

impds12$Period = seq(57,60) ##Add Separate column for Period

impds12 ## Call the Dataframe

##_____________Minimising_the_affect_of_first_lockdown________________________________

clds1 = impds[57:60, ]   ##-Covid 1st Lockdown Affected Dataset from Mar 20 to Jun 20
head(clds1) ##View top 6 Rows of the 1st Lockdown Period

ids = merge(x = clds1, y = impds12, by = "Period", all.x = TRUE) ##Merge the table with forecast dataframe
ids ## Call the Merged Table

ids = subset(ids, select = -c(ItemTn.x) ) ## Delete the Unwanted Column

colnames(ids) <- c('Period','ItemTn') ##Replace the Original Value with Forecast value

impds_new = rbind(impds1,ids) ##Append the new Table 

head(impds_new) ##View Top 6 Rows of New Data
tail(impds_new) ##View Bottom 6 Rows of New Data

impds_new ##Call the New Data
plot(impds_new) ##Plot the New Data

##____________Append_the_next_Unaffected_timeline___________________________________

impds2 = impds[61:69, ]  ##-After 1st Lockdown and before 2nd Lockdown from Jul 20 to Mar 21
head(impds2)
tail(impds2)

impds_new = rbind(impds_new,impds2) ##Append
plot(impds_new) ##Plot Updated
impds_new ##Call Updated


## Converting in Timeseries Data
sale_ts2 = ts(impds_new$ItemTn,start = c(2015,7),end = c(2021,3),frequency = 12)
sale_ts2
plot(sale_ts2)


##Decompose to Observe Trend and Seasonality
decompose(sale_ts2)
plot(decompose(sale_ts2))

adf.test(sale_ts2)## Augmented Dicky Fuller Test 


HW_temp_forecast = HoltWinters(sale_ts2) ##Fit Timeseries Data into HoltsWinter
HW_temp_forecast
plot(HW_temp_forecast)



HW_pred_model = forecast:::forecast(HW_temp_forecast,h=3) ##Forecast for next 3 Months
HW_pred_model
plot(HW_pred_model)


impds12 = as.data.frame(HW_pred_model) ##Convert The Forecast Values to Dataframe


colnames(impds12) <- c('ItemTn','a','b','c','d') ##Changing the Column names


impds12 = subset(impds12, select = -c(a,b,c,d) ) ##Deleting the Unwanted Columns

impds12$Period = seq(70,72) ##Adding Period column

impds12

impds_new

##_______________________________________________

clds2 = impds[70:72, ]   ##-Covid 2nd Lockdown Affected Dataset from Apr 21 to Jun 21

head(clds2)


##Merge the 2 tables on basis of period
ids2 = merge(x = clds2, y = impds12, by = "Period", all.x = TRUE)
ids2

ids2 = subset(ids2, select = -c(ItemTn.x) ) ##Delete Unwanted Column

colnames(ids2) <- c('Period','ItemTn') ##Changing the Column Name

impds_new = rbind(impds_new,ids2) ##Append to updated table

head(impds_new) ##View top 6 rows of updated dataset
tail(impds_new) ##View bottom 6 rows of updated dataset

impds_new
plot(impds_new)

##_______________________________________________

impds3 = impds[73:78, ]  ##-After 2nd Lockdown Jul 21 to Dec 21
head(impds3)

impds_new = rbind(impds_new,impds3) ##Append the Remaining last after 2nd lockdown timeline
impds_new

##_______________________________________________
##Overall
##Forecast the Updated data
sale_ts = ts(impds_new$ItemTn,start = c(2015,7),end = c(2021,12),frequency = 12) 
sale_ts
plot(sale_ts)


decompose(sale_ts)
plot(decompose(sale_ts))
adf.test(sale_ts)


HW_temp_forecast = HoltWinters(sale_ts)
HW_temp_forecast
plot(HW_temp_forecast)


HW_pred_model = forecast:::forecast(HW_temp_forecast,h=12)
HW_pred_model
plot(HW_pred_model)
