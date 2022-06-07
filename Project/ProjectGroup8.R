library(data.table)
library(forecast)
library(ggplot2)
library(reshape2)
library(lubridate)
library(knitr)
library(urca)
library(tidyverse)

setwd("/Users/kutayilmen/Downloads")
todays_date=as.Date(today())
forecast_date=todays_date+1
production=fread("2022-06-07_production.csv")
weather=fread("2022-06-07_weather.csv")

# identify # of days to forecast
latest_available_prod_date=as.Date(max(production$date))
n_days=as.numeric(forecast_date-latest_available_prod_date)

# get the latest n_days and modify for forecasting
# creates date,hour,production for the upcoming days
forecasted_production=tail(production,n_days*24)
forecasted_production[,date:=date+n_days]
forecasted_production[,production:=NA]

# actual production data with forecasted dates
production_with_forecast=rbind(production,forecasted_production)

# create a template for forecast date
forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)

# transform long weather to wide
wide_weather=dcast(weather,date+hour~variable+lat+lon,value.var='value')

# merge with production with forecast dates
production_with_weather=merge(production_with_forecast,wide_weather,by=c('date','hour'))

# to account for hourly seasonality in regression setting, define hour as character
production_with_weather[,hour:=as.factor(hour)]

#-------------------------------------------------------------------------------

#reporting accuracy
statistics <- function(actual, forecasted){
  n=length(actual)
  error = actual - forecasted
  mean = mean(actual)
  sd = sd(actual)
  bias = sum(error) / sum(actual)
  mad = sum(abs(error)) / n
  wmape = mad / mean
  l = data.frame(n, mean, sd, bias, mad, wmape)
  colnames(l) <- c("N", "Mean", "Standard Deviation", "Bias", "MAD", "WMAPE")
  return(l)
}

#-------------------------------------------------------------------------------

#---------------------------------Visualization------------------------

#convert hourly data to daily data
daily_data=production_with_weather[,list(total_p=sum(production),max_p=max(production),
                      mean_p=mean(production)),list(date)]

#Daily Total Production for Available Data
ggplot(daily_data ,aes(x=date,y=total_p)) +
  geom_line(color = "darkred") +
  labs(title = "Daily Total Production for Available Data",
       x = "Date",
       y= "Production (MWh)" ) +
  scale_x_date(date_breaks = "3 month", date_labels = "%d %b %y") +
  theme_minimal()

#Daily Maximum Production for Available Data
ggplot(daily_data ,aes(x=date,y=max_p)) + 
  geom_line(color = "darkred") +
  labs(title = "Daily Maximum Production for Available Data",
       x = "Date",
       y= "Production (MWh)" ) +
  scale_x_date(date_breaks = "3 month", date_labels = "%d %b %y") +
  theme_minimal()

# daily plot
ggplot(data = daily_data[which(date=='2021-07-01'):which(date=='2021-07-28')], aes(x = date, y = total_p)) +
  geom_line(color = "darkred") +
  labs(title = "Daily Total Production Data between 01/07/21 and 28/07/21 ",
       x = "Date",
       y= "Production (MWh)" ) +
  scale_x_date(date_breaks = "7 days", date_labels = "%d %b %y") +
  theme_minimal()

copy <- production_with_weather
copy$hour <- as.numeric(copy$hour)
copy[,datetime:=ymd(date)+dhours(hour)]
copy=copy[order(datetime)]

# hourly plot
ggplot(data = copy[1:240], aes(x = datetime, y = production)) +
  geom_line(color = "darkred") +
  labs(title = "Daily Consumption Data between 01/01/17 and 21/01/17 ",
       x = "Date",
       y= "Consumption (MWh)")  

#-----------------------Model 0------------------------------

# baseline approach: "Most recent actual production as forecast"
baseline_forecast=production_with_forecast[date==latest_available_prod_date]$production
# update forecast table
forecast_table[,baseline:=baseline_forecast]

#-----------------------Model 1------------------------------ 

# test and train phases
production_with_weather[,hour:=as.factor(hour)]
train_data=production_with_weather[!is.na(production)]
test_data=production_with_weather[is.na(production)]

# expected test period
march_first <- which(grepl('2022-03-01',train_data$date) & grepl(0,train_data$hour) & !grepl(1,train_data$hour) & !grepl(2,train_data$hour))

may_twentyfourth <- which(grepl('2022-05-25',train_data$date) & grepl(0,train_data$hour) & !grepl(1,train_data$hour) & !grepl(2,train_data$hour))

evaluation_period <- march_first:(may_twentyfourth-1)

# prediction period

may_twentyfifth <- which(grepl('2022-05-25',train_data$date) & grepl(0,train_data$hour) & !grepl(1,train_data$hour) & !grepl(2,train_data$hour))

june_third <- which(grepl('2022-06-04',train_data$date) & grepl(0,train_data$hour) & !grepl(1,train_data$hour) & !grepl(2,train_data$hour))

prediction_period <- may_twentyfifth:(june_third-1)

# control phase to check the statistics of the model
control_data=production_with_weather[evaluation_period]
control_data[,datetime:=as.POSIXct(paste(control_data$date, control_data$hour), format="%Y-%m-%d %H")]
control_data=control_data[order(datetime)]

control_data_1week=train_data[(nrow(train_data)-167):nrow(train_data)]
control_data_1week[,datetime:=as.POSIXct(paste(control_data_1week$date, control_data_1week$hour), format="%Y-%m-%d %H")]
control_data_1week=control_data_1week[order(datetime)]

# Model 1 with all independent variables

# train with all variables
lm_model1=lm(production~.,train_data[,-c('date')])
summary(lm_model1)
#checkresiduals(lm_model1)
lm_forecast_test=predict(lm_model1,test_data)
test_data[,forecasted1:=as.numeric(lm_forecast_test)]
lm_forecast_control=predict(lm_model1,control_data)
control_data[,forecasted1:=as.numeric(lm_forecast_control)]

# for 7 days control
lm_forecast_control=predict(lm_model1,control_data_1week)
control_data_1week[,forecasted1:=as.numeric(lm_forecast_control)]

# postprocess the forecast based on actual production
control_data[production<=0,'forecasted1':=0]
control_data[forecasted1<=0,'forecasted1':=0]
control_data[production>=max(production),'forecasted1':=max(production)]
control_data[forecasted1>=max(production),'forecasted1':=max(production)]

control_data_1week[production<=0,'forecasted1':=0]
control_data_1week[forecasted1<=0,'forecasted1':=0]
control_data_1week[production>=max(production),'forecasted1':=max(production)]
control_data_1week[forecasted1>=max(production),'forecasted1':=max(production)]

# update forecast table
forecast_table[,forecast_all_var:=test_data[date==(forecast_date)]$forecasted1]

# check statistics of model 1
kable(statistics(control_data$production, control_data$forecasted1),
      caption = "Statistics of Model 1 for evaluation period", align = 'c')
kable(statistics(control_data_1week$production, control_data_1week$forecasted1),
      caption = "Statistics of Model 1 for last week", align = 'c')

#----------------Model 2-------------------

# check the correlations
vec <- as.matrix(cor(train_data[,c(4:39)], train_data$production))
mat <- matrix(vec,nrow=9)
mat <- as.data.table(mat)
colnames(mat) <- c("Cloud Cover", "Flux", "Humidity", "Temp")
mat
# take means of variables
cloud_mean <- rowMeans(subset(production_with_weather,select = c(4:12)))
flux_mean <- rowMeans(subset(production_with_weather,select = c(13:21)))  
humidity_mean <- rowMeans(subset(production_with_weather,select = c(22:30)))
temp_mean <- rowMeans(subset(production_with_weather,select = c(31:39)))

production_with_weather_means = subset(production_with_weather, select = -c(4 : 39))

production_with_weather_means[,cloud_mean:=NA]
production_with_weather_means[,flux_mean:=NA]
production_with_weather_means[,humidity_mean:=NA]
production_with_weather_means[,temp_mean:=NA]

for(i in 1:nrow(production_with_weather_means)){
  production_with_weather_means$cloud_mean[i] = cloud_mean[i]
  production_with_weather_means$flux_mean[i] = flux_mean[i]
  production_with_weather_means$humidity_mean[i] = humidity_mean[i]
  production_with_weather_means$temp_mean[i] = temp_mean[i]  
}

# test and train phases
production_with_weather_means[,hour:=as.factor(hour)]
train_data_means=production_with_weather_means[!is.na(production)]
test_data_means=production_with_weather_means[is.na(production)]

# control phase to check the statistics of the model
control_data_means=production_with_weather_means[evaluation_period]
control_data_means[,datetime:=as.POSIXct(paste(control_data_means$date, control_data_means$hour), format="%Y-%m-%d %H")]
control_data_means=control_data_means[order(datetime)]

control_data_1week_means=train_data_means[(nrow(train_data_means)-167):nrow(train_data_means)]
control_data_1week_means[,datetime:=as.POSIXct(paste(control_data_1week_means$date, control_data_1week_means$hour), format="%Y-%m-%d %H")]
control_data_1week_means=control_data_1week_means[order(datetime)]

# model 2 with means 
lm_model2 <- lm(production~., train_data_means[,-c('date')])
summary(lm_model2)
#checkresiduals(lm_model2)
lm_forecast_test=predict(lm_model2,test_data_means)
test_data_means[,forecasted2:=as.numeric(lm_forecast_test)]
lm_forecast_control=predict(lm_model2,control_data_means)
control_data_means[,forecasted2:=as.numeric(lm_forecast_control)]
# for 10 days control
lm_forecast_control=predict(lm_model2,control_data_1week_means)
control_data_1week_means[,forecasted2:=as.numeric(lm_forecast_control)]

# postprocess the forecast based on actual production
control_data_means[production<=0,'forecasted2':=0]
control_data_means[forecasted2<=0,'forecasted2':=0]
control_data_means[production>=max(production),'forecasted2':=max(production)]
control_data_means[forecasted2>=max(production),'forecasted2':=max(production)]

control_data_1week_means[production<=0,'forecasted2':=0]
control_data_1week_means[forecasted2<=0,'forecasted2':=0]
control_data_1week_means[production>=max(production),'forecasted2':=max(production)]
control_data_1week_means[forecasted2>=max(production),'forecasted2':=max(production)]

# update forecast table
forecast_table[,forecast_with_means:=test_data_means[date==forecast_date]$forecasted2]

# check statistics of model 2
kable(statistics(control_data_means$production, control_data_means$forecasted2),
      caption = "Statistics of Model 2 for evaluation period ", align = 'c')
kable(statistics(control_data_1week_means$production, control_data_1week_means$forecasted2),
      caption = "Statistics of Model 2 for one week ", align = 'c')

#-------------------------Model 3----------------------

daily_max_production=production_with_forecast[,list(max_prod=max(production)),by=list(date)]
daily_max_production[,rolling_max:=frollapply(max_prod,30,max,na.rm=T)]

# due to the loss of information, we used the daily max for each day for the first 30 days
daily_max_production$rolling_max[1:29] = frollapply(daily_max_production$max_prod[1:29],5,max,na.rm=T)
daily_max_production$rolling_max[1:4] <- as.double(max(daily_max_production$max_prod[1:4])) 

# merge with regression data
production_with_weather_means_capacity=merge(production_with_weather_means,daily_max_production,by=c('date'))
production_with_weather_means_capacity[,normalized_production:=production/rolling_max]

# test and train phases
production_with_weather_means_capacity[,hour:=as.factor(hour)]
train_data_means_capacity=production_with_weather_means_capacity[!is.na(production)]
test_data_means_capacity=production_with_weather_means_capacity[is.na(production)]

# control phase to check the statistics of the model
control_data_means_capacity=production_with_weather_means_capacity[evaluation_period]
control_data_means_capacity[,datetime:=as.POSIXct(paste(control_data_means_capacity$date, control_data_means_capacity$hour), format="%Y-%m-%d %H")]
control_data_means_capacity=control_data_means_capacity[order(datetime)]

# for a week control phase 
control_data_1week_means_capacity=train_data_means_capacity[(nrow(train_data_means_capacity)-167):nrow(train_data_means_capacity)]
control_data_1week_means_capacity[,datetime:=as.POSIXct(paste(control_data_1week_means_capacity$date, control_data_1week_means_capacity$hour), format="%Y-%m-%d %H")]
control_data_1week_means_capacity=control_data_1week_means_capacity[order(datetime)]


# model 3 with daily max in a moving manner to normalize with means of locations
lm_model3=lm(normalized_production~.,train_data_means_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model3)
#checkresiduals(lm_model3)
lm_forecast_test=predict(lm_model3,test_data_means_capacity)
test_data_means_capacity[,forecasted3:=as.numeric(lm_forecast_test)*rolling_max]
lm_forecast_control=predict(lm_model3,control_data_means_capacity)
control_data_means_capacity[,forecasted3:=as.numeric(lm_forecast_control)*rolling_max]
# for 7 days control
lm_forecast_control=predict(lm_model3,control_data_1week_means_capacity)
control_data_1week_means_capacity[,forecasted3:=as.numeric(lm_forecast_control)*rolling_max]

# postprocess the forecast based on actual production
control_data_means_capacity[production<=0,'forecasted3':=0]
control_data_means_capacity[forecasted3<=0,'forecasted3':=0]
control_data_means_capacity[production>=max(production),'forecasted3':=max(production)]
control_data_means_capacity[forecasted3>=max(production),'forecasted3':=max(production)]

control_data_1week_means_capacity[production<=0,'forecasted3':=0]
control_data_1week_means_capacity[forecasted3<=0,'forecasted3':=0]
control_data_1week_means_capacity[production>=max(production),'forecasted3':=max(production)]
control_data_1week_means_capacity[forecasted3>=max(production),'forecasted3':=max(production)]

# update forecast table
forecast_table[,forecast_with_means_normalized:=test_data_means_capacity[date==forecast_date]$forecasted3]

#check plot of actual vs fitted
cols <- c("forecasted" = "orange", "actual" = "blue")
ggplot() + 
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = forecasted3,color = "forecasted")) +
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "1 March 2022- 24 May 2022",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "1 month",expand = c(0,0)) +
  theme_minimal()

ggplot() + 
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = forecasted3,color = "forecasted")) +
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "Last Week of Available Data",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "24 hours",expand = c(0,0)) +
  theme_minimal()

# check statistics of model 3
kable(statistics(control_data_means_capacity$production, control_data_means_capacity$forecasted3),
      caption = "Statistics of Model 3 for evaluation period ", align = 'c')
kable(statistics(control_data_1week_means_capacity$production, control_data_1week_means_capacity$forecasted3),
      caption = "Statistics of Model 3 for one week ", align = 'c')
#--------------------Model 4-------------------------

# merge with regression data
production_with_weather_capacity=merge(production_with_weather,daily_max_production,by=c('date'))
production_with_weather_capacity[,normalized_production:=production/rolling_max]

# test and train phases
production_with_weather_capacity[,hour:=as.factor(hour)]
train_data_capacity=production_with_weather_capacity[!is.na(production)]
test_data_capacity=production_with_weather_capacity[is.na(production)]

# control phase to check the statistics of the model
control_data_capacity=production_with_weather_capacity[evaluation_period]
control_data_capacity[,datetime:=as.POSIXct(paste(control_data_capacity$date, control_data_capacity$hour), format="%Y-%m-%d %H")]
control_data_capacity=control_data_capacity[order(datetime)]

# for a week control phase 
control_data_1week_capacity=train_data_capacity[(nrow(train_data_capacity)-167):nrow(train_data_capacity)]
control_data_1week_capacity[,datetime:=as.POSIXct(paste(control_data_1week_capacity$date, control_data_1week_capacity$hour), format="%Y-%m-%d %H")]
control_data_1week_capacity=control_data_1week_capacity[order(datetime)]


# model 4 with daily max in a moving manner to normalize 
lm_model4=lm(normalized_production~.,train_data_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model4)
#checkresiduals(lm_model3)
lm_forecast_test=predict(lm_model4,test_data_capacity)
test_data_capacity[,forecasted4:=as.numeric(lm_forecast_test)*rolling_max]
lm_forecast_control=predict(lm_model4,control_data_capacity)
control_data_capacity[,forecasted4:=as.numeric(lm_forecast_control)*rolling_max]
# for 10 days control
lm_forecast_control=predict(lm_model4,control_data_1week_capacity)
control_data_1week_capacity[,forecasted4:=as.numeric(lm_forecast_control)*rolling_max]


# postprocess the forecast based on actual production
control_data_capacity[production<=0,'forecasted4':=0]
control_data_capacity[forecasted4<=0,'forecasted4':=0]
control_data_capacity[production>=max(production),'forecasted4':=max(production)]
control_data_capacity[forecasted4>=max(production),'forecasted4':=max(production)]

control_data_1week_capacity[production<=0,'forecasted4':=0]
control_data_1week_capacity[forecasted4<=0,'forecasted4':=0]
control_data_1week_capacity[production>=max(production),'forecasted4':=max(production)]
control_data_1week_capacity[forecasted4>=max(production),'forecasted4':=max(production)]


# update forecast table
forecast_table[,forecast_with_normalized:=test_data_capacity[date==forecast_date]$forecasted4]

# check statistics of model 4
kable(statistics(control_data_capacity$production, control_data_capacity$forecasted4),
      caption = "Statistics of Model 4 for evaluation period ", align = 'c')
kable(statistics(control_data_1week_capacity$production, control_data_1week_capacity$forecasted4),
      caption = "Statistics of Model 4 for one week ", align = 'c')

#----------Model 2 and model 3 gives the best wmape results----------------

#-------------------------Model 5-----------------------

lag72 <- vector()
train_data_means_capacity= cbind(train_data_means_capacity, lag72)
train_data_means_capacity$lag72[1:72] = NA
train_data_means_capacity$lag72[73:nrow(train_data_means_capacity)] = train_data_means_capacity$production[1:(nrow(train_data_means_capacity)-72)]
train_data_means_capacity = train_data_means_capacity[!is.na(lag72)]

test_data_means_capacity= cbind(test_data_means_capacity, lag72)
min = nrow(train_data_means_capacity) - 71
max = nrow(train_data_means_capacity)
test_data_means_capacity$lag72 = train_data_means_capacity$production[c(min:max)]

#control_data_means_capacity <- train_data_means_capacity[evaluation_period]
control_data_means_capacity= cbind(control_data_means_capacity, lag72)
control_data_means_capacity$lag72 = train_data_means_capacity$lag72[evaluation_period - 72]

control_data_1week_means_capacity= cbind(control_data_1week_means_capacity, lag72)
control_data_1week_means_capacity$lag72 = train_data_means_capacity$lag72[(nrow(train_data_means_capacity)-167):nrow(train_data_means_capacity)]

# train with all variables
lm_model5=lm(normalized_production~.,train_data_means_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model5)
#checkresiduals(lm_model5)
lm_forecast_test=predict(lm_model5,test_data_means_capacity)
test_data_means_capacity[,forecasted5:=as.numeric(lm_forecast_test)*rolling_max]
lm_forecast_control=predict(lm_model5,control_data_means_capacity)
control_data_means_capacity[,forecasted5:=as.numeric(lm_forecast_control)*rolling_max]
#for 1 week
lm_forecast_control=predict(lm_model5,control_data_1week_means_capacity)
control_data_1week_means_capacity[,forecasted5:=as.numeric(lm_forecast_control)*rolling_max]

# postprocess the forecast based on actual production
control_data_means_capacity[production<=0,'forecasted5':=0]
control_data_means_capacity[forecasted5<=0,'forecasted5':=0]
control_data_means_capacity[production>=max(production),'forecasted5':=max(production)]
control_data_means_capacity[forecasted5>=max(production),'forecasted5':=max(production)]

control_data_1week_means_capacity[production<=0,'forecasted5':=0]
control_data_1week_means_capacity[forecasted5<=0,'forecasted5':=0]
control_data_1week_means_capacity[production>=max(production),'forecasted5':=max(production)]
control_data_1week_means_capacity[forecasted5>=max(production),'forecasted5':=max(production)]

# update forecast table
forecast_table[,forecast_with_means_normalized_lag72:=test_data_means_capacity[date==forecast_date]$forecasted5]

#check plot of actual vs fitted
ggplot() + 
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = forecasted5,color = "forecasted")) +
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "1 March 2022- 24 May 2022",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "14 days",expand = c(0,0)) +
  theme_minimal()

ggplot() + 
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = forecasted5,color = "forecasted")) +
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "Last Week of Available Data",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "24 hours",expand = c(0,0)) +
  theme_minimal()

# check statistics of model 5
kable(statistics(control_data_means_capacity$production, control_data_means_capacity$forecasted5),
      caption = "Statistics of Model 5 for evaluation period", align = 'c')
# check statistics of model 5
kable(statistics(control_data_1week_means_capacity$production, control_data_1week_means_capacity$forecasted5),
      caption = "Statistics of Model 5 for one week", align = 'c')


#------------------------Model 6----------------------------------

train_data_means_capacity[,differ1:=train_data_means_capacity$production-shift(train_data_means_capacity$production,1)]
test_data_means_capacity[,differ1:=train_data_means_capacity$production[c(min:max)]-shift(train_data_means_capacity$production[c(min:max)],1)]
train_data_means_capacity$differ1[1] = 0
test_data_means_capacity$differ1[1] = 0

evaluation_period

differ1 <- vector()
control_data_means_capacity <- cbind(control_data_means_capacity, differ1)
control_data_means_capacity$differ1 <- train_data_means_capacity$differ1[evaluation_period-72]

control_data_1week_means_capacity <- cbind(control_data_1week_means_capacity, differ1)
control_data_1week_means_capacity$differ1 <- train_data_means_capacity$differ1[(nrow(train_data_means_capacity)-167):nrow(train_data_means_capacity)]


lm_model6=lm(normalized_production~.,train_data_means_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model6)
#checkresiduals(lm_model6)
lm_forecast_test=predict(lm_model6,test_data_means_capacity)
test_data_means_capacity[,forecasted6:=as.numeric(lm_forecast_test)*rolling_max]
lm_forecast_control=predict(lm_model6,control_data_means_capacity)
control_data_means_capacity[,forecasted6:=as.numeric(lm_forecast_control)*rolling_max]
# for 1 week
lm_forecast_control=predict(lm_model6,control_data_1week_means_capacity)
control_data_1week_means_capacity[,forecasted6:=as.numeric(lm_forecast_control)*rolling_max]

# postprocess the forecast based on actual production
control_data_means_capacity[production<=0,'forecasted6':=0]
control_data_means_capacity[forecasted6<=0,'forecasted6':=0]
control_data_means_capacity[production>=max(production),'forecasted6':=max(production)]
control_data_means_capacity[forecasted6>=max(production),'forecasted6':=max(production)]

control_data_1week_means_capacity[production<=0,'forecasted6':=0]
control_data_1week_means_capacity[forecasted6<=0,'forecasted6':=0]
control_data_1week_means_capacity[production>=max(production),'forecasted6':=max(production)]
control_data_1week_means_capacity[forecasted6>=max(production),'forecasted6':=max(production)]


#check plot of actual vs fitted
ggplot() + 
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = forecasted6,color = "forecasted")) +
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "1 March 2022- 24 May 2022",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "14 days",expand = c(0,0)) +
  theme_minimal()

ggplot() + 
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = forecasted6,color = "forecasted")) +
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "Last Week of Available Data",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "24 hours",expand = c(0,0)) +
  theme_minimal()

# check statistics of model 6
kable(statistics(control_data_means_capacity$production, control_data_means_capacity$forecasted6),
      caption = "Statistics of Model 6 for evaluation period ", align = 'c')

# check statistics of model 6
kable(statistics(control_data_1week_means_capacity$production, control_data_1week_means_capacity$forecasted6),
      caption = "Statistics of Model 6 for one week ", align = 'c')

#-----------------------------------Model 7--------------------------------

train_data_means_capacity[,differ24:=train_data_means_capacity$production-shift(train_data_means_capacity$production,24)]
test_data_means_capacity[,differ24:=train_data_means_capacity$production[c(min:max)]-shift(train_data_means_capacity$production[c(min:max)],24)]
train_data_means_capacity$differ24[1:24] = 0
test_data_means_capacity$differ24[1:24] = 0

differ24 <- vector()
control_data_means_capacity <- cbind(control_data_means_capacity, differ24)
control_data_means_capacity$differ24 <- train_data_means_capacity$differ24[evaluation_period-72]

control_data_1week_means_capacity <- cbind(control_data_1week_means_capacity, differ24)
control_data_1week_means_capacity$differ24 <- train_data_means_capacity$differ24[(nrow(train_data_means_capacity)-167):nrow(train_data_means_capacity)]


lm_model7=lm(normalized_production~.,train_data_means_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model7)
#checkresiduals(lm_model7)
lm_forecast_test=predict(lm_model7,test_data_means_capacity)
test_data_means_capacity[,forecasted7:=as.numeric(lm_forecast_test)*rolling_max]
lm_forecast_control=predict(lm_model7,control_data_means_capacity)
control_data_means_capacity[,forecasted7:=as.numeric(lm_forecast_control)*rolling_max]
# for 1 week
lm_forecast_control=predict(lm_model7,control_data_1week_means_capacity)
control_data_1week_means_capacity[,forecasted7:=as.numeric(lm_forecast_control)*rolling_max]

# postprocess the forecast based on actual production
control_data_means_capacity[production<=0,'forecasted7':=0]
control_data_means_capacity[forecasted7<=0,'forecasted7':=0]
control_data_means_capacity[production>=max(production),'forecasted7':=max(production)]
control_data_means_capacity[forecasted7>=max(production),'forecasted7':=max(production)]

control_data_1week_means_capacity[production<=0,'forecasted7':=0]
control_data_1week_means_capacity[forecasted7<=0,'forecasted7':=0]
control_data_1week_means_capacity[production>=max(production),'forecasted7':=max(production)]
control_data_1week_means_capacity[forecasted7>=max(production),'forecasted7':=max(production)]

# update forecast table
forecast_table[,forecast_with_means_normalized_lag72_dif1_dif24:=test_data_means_capacity[date==forecast_date]$forecasted7]

#check plot of actual vs fitted
ggplot() + 
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = forecasted7,color = "forecasted")) +
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "1 March 2022- 24 May 2022",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "14 days",expand = c(0,0)) +
  theme_minimal()

ggplot() + 
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = forecasted7,color = "forecasted")) +
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "Last Week of Available Data",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "24 hours",expand = c(0,0)) +
  theme_minimal()

# check statistics of model 7
kable(statistics(control_data_means_capacity$production, control_data_means_capacity$forecasted7),
      caption = "Statistics of Model 7 for evaluation period ", align = 'c')
kable(statistics(control_data_1week_means_capacity$production, control_data_1week_means_capacity$forecasted7),
      caption = "Statistics of Model 7 for one week ", align = 'c')

#----------------------------------Model 8---------------------------------------

# outliers exceed 95% significance level removed

train_data_means_capacity[,residuals:=NA] 
train_data_means_capacity$residuals <- lm_model7$residuals[1:(nrow(train_data_means_capacity))]

# Detecting lower and upper 5% residuals and marking them as outlier_small and outlier_great
train_data_means_capacity[!is.na(residuals), quant5:=quantile(residuals,0.05)]
train_data_means_capacity[!is.na(residuals), quant95:=quantile(residuals,0.95)]
train_data_means_capacity[,outlier_small:=as.numeric(residuals<quant5)]
train_data_means_capacity[,outlier_great:=as.numeric(residuals>quant95)]

residual <- vector()
control_data_means_capacity <- cbind(control_data_means_capacity, residuals)
control_data_means_capacity$residuals <- train_data_means_capacity$residuals[evaluation_period-72]

control_data_means_capacity[!is.na(residuals), quant5:=quantile(residuals,0.05)]
control_data_means_capacity[!is.na(residuals), quant95:=quantile(residuals,0.95)]
control_data_means_capacity[,outlier_small:=as.numeric(residuals<quant5)]
control_data_means_capacity[,outlier_great:=as.numeric(residuals>quant95)]

test_data_means_capacity[,outlier_small:=0]
test_data_means_capacity[,outlier_great:=0]

control_data_1week_means_capacity <- cbind(control_data_1week_means_capacity, residuals)
control_data_1week_means_capacity$residuals <- train_data_means_capacity$residuals[(nrow(train_data_means_capacity)-167):nrow(train_data_means_capacity)]

control_data_1week_means_capacity[!is.na(residuals), quant5:=quantile(residuals,0.05)]
control_data_1week_means_capacity[!is.na(residuals), quant95:=quantile(residuals,0.95)]
control_data_1week_means_capacity[,outlier_small:=as.numeric(residuals<quant5)]
control_data_1week_means_capacity[,outlier_great:=as.numeric(residuals>quant95)]

lm_model8=lm(normalized_production~.,train_data_means_capacity[,-c('date','rolling_max','production','max_prod','quant5', 'quant95', 'residuals'),with=F])
summary(lm_model8)
#checkresiduals(lm_model8)
lm_forecast_test=predict(lm_model8,test_data_means_capacity)
test_data_means_capacity[,forecasted8:=as.numeric(lm_forecast_test)*rolling_max]
lm_forecast_control=predict(lm_model8,control_data_means_capacity)
control_data_means_capacity[,forecasted8:=as.numeric(lm_forecast_control)*rolling_max]
# for 1 week
lm_forecast_control=predict(lm_model8,control_data_1week_means_capacity)
control_data_1week_means_capacity[,forecasted8:=as.numeric(lm_forecast_control)*rolling_max]

# postprocess the forecast based on actual production
control_data_means_capacity[production<=0,'forecasted8':=0]
control_data_means_capacity[forecasted8<=0,'forecasted8':=0]
control_data_means_capacity[production>=max(production),'forecasted8':=max(production)]
control_data_means_capacity[forecasted8>=max(production),'forecasted8':=max(production)]

control_data_1week_means_capacity[production<=0,'forecasted8':=0]
control_data_1week_means_capacity[forecasted8<=0,'forecasted8':=0]
control_data_1week_means_capacity[production>=max(production),'forecasted8':=max(production)]
control_data_1week_means_capacity[forecasted8>=max(production),'forecasted8':=max(production)]

# update forecast table
forecast_table[,forecast_with_means_normalized_lag72_dif1_dif24_outlier:=test_data_means_capacity[date==forecast_date]$forecasted8]

#check plot of actual vs fitted
ggplot() + 
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = forecasted8,color = "forecasted")) +
  geom_line(data = control_data_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "1 March 2022- 24 May 2022",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "14 days",expand = c(0,0)) +
  theme_minimal()

ggplot() + 
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = forecasted8,color = "forecasted")) +
  geom_line(data = control_data_1week_means_capacity, aes(x = datetime, y = production,color = "actual")) +
  labs(title = "Predicted vs. Actual",
       subtitle = "Last Week of Available Data",
       x = "Time",
       y = "Production",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks = "24 hours",expand = c(0,0)) +
  theme_minimal()
# check statistics of model 8
kable(statistics(control_data_means_capacity$production, control_data_means_capacity$forecasted8),
      caption = "Statistics of Model 8 for evaluation period", align = 'c')

kable(statistics(control_data_1week_means_capacity$production, control_data_1week_means_capacity$forecasted8),
      caption = "Statistics of Model 8 for one week ", align = 'c')

# postprocess the forecast based on the baseline
forecast_table[baseline<=0,'forecast_with_means_normalized_lag72_dif1_dif24_outlier':=0]
forecast_table[forecast_with_means_normalized_lag72_dif1_dif24_outlier<=0,'forecast_with_means_normalized_lag72_dif1_dif24_outlier':=0]
forecast_table[baseline>=max(baseline),'forecast_with_means_normalized_lag72_dif1_dif24_outlier':=max(baseline)]
forecast_table[forecast_with_means_normalized_lag72_dif1_dif24_outlier>=max(baseline),'forecast_with_means_normalized_lag72_dif1_dif24_outlier':=max(baseline)]

cat(paste(forecast_table$forecast_with_means_normalized_lag72_dif1_dif24_outlier,collapse=','))

#--------------------------------------------------------------------------------------------------

# First Model Statistics for evaluation period
kable(statistics(control_data_means_capacity$production, control_data_means_capacity$forecasted3),
      caption = "Statistics of Model 1 for evaluation period ", align = 'c')

# Final Model Statistics for evaluation period
kable(statistics(control_data_means_capacity$production, control_data_means_capacity$forecasted8),
      caption = "Statistics of Model 8 for evaluation period", align = 'c')

# First Model Statistics for one week
kable(statistics(control_data_1week_means_capacity$production, control_data_1week_means_capacity$forecasted3),
      caption = "Statistics of Model 1 for one week", align = 'c')

# Final Model Statistics for one week
kable(statistics(control_data_1week_means_capacity$production, control_data_1week_means_capacity$forecasted8),
      caption = "Statistics of Model 8 for one week", align = 'c')

#-----------------------------------------------------------------------------------



