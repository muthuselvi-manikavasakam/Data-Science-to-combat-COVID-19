# MS5103 Final Year Project, Group 13, COVID-19 Analysis

####### Covid-19 Machine Learning #######

# clean environment, set working directory and load libraries
rm(list = ls())
setwd("C:\\Datasets\\Covid19\\")
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(MLmetrics)
library(mgcv)
library(prophet)

#define https paths for datasources "new daily data is updated automatically", source: JHU CSSE - Johns Hopkins University
confirmed_path = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'

# *---1st Dataset---*
# ETL "confirmed" cases data:
confirmed <- read.csv(confirmed_path, header = TRUE, sep=",",
                      stringsAsFactors = FALSE, check.names=FALSE)
confirmed <- melt(confirmed, id = c(1:4))
confirmed <- subset(confirmed, select = -c(1,2,3,4))
colnames(confirmed) <- c("Date","Total_Cases")
confirmed <- confirmed %>%
  select(Date, Total_Cases) %>%
  group_by(Date) %>%
  summarise(Total_Cases = sum(Total_Cases))

confirmed$Date <- as.Date(as.character(confirmed$Date),format = "%m/%d/%y")
confirmed <- as.data.frame(confirmed) #paste grouped tbl into df

confirmed <- confirmed[order(confirmed[,1]),]
row.names(confirmed) <- NULL #reset index after sorted

confirmed <- subset(confirmed, Date <= "2020-06-15")
#include measure to count days since January 22nd - nrows(df)=145
confirmed$day <- 0:145

# prapate train, test and predict datasets
train <- subset(confirmed, Date < "2020-06-01") 
test <- subset(confirmed, Date >= "2020-06-01")
predict <- subset(test, select = -c(1,2))

## Linear regression model
m1 <- lm(Total_Cases ~ day, data=train)
m1forecast <- predict(m1, newdata = predict, 
                      interval = "confidence")
m1forecast <- as.data.frame(m1forecast)
colnames(m1forecast) <- c("lm_Forecast","lm_lwr","lm_upr")
m1forecast$day <- 131:145


## Facebook Prophet Additive Regression
train_profhet <- train #prepare training df to prophet parameters
train_profhet <- subset(train_profhet, select = -c(3))
colnames(train_profhet) <- c("ds","y") #prophet accepts only 2 variables
m <- prophet(train_profhet) #fit model
future <- make_future_dataframe(m, periods = 15) #define 15 days forecast timeframe
forecast <- predict(m, future) # prediction


# To see the effect of changing the proposition of points 
# that influence the smooth (f in command or Span for smooth in menu)
help(lowess)
plot(confirmed$Date,confirmed$Total_Cases)
f1=lowess(confirmed$Date,confirmed$Total_Cases,f=0.2)
f2=lowess(confirmed$Date,confirmed$Total_Cases,f=0.3)
f3=lowess(confirmed$Date,confirmed$Total_Cases,f=0.4)
plot(confirmed$Date,confirmed$Total_Cases,
     xlab="Date",ylab="Total_Cases", 
     main="Covid-19 Development",
     sub="black f=0.2, red f=0.3, 
     blue f=0.4, green ls")
lines(f1$x,f1$y,type="l")
lines(f2$x,f2$y,type="l",col="red")
lines(f3$x,f3$y,type="l",col="blue") 
abline(lm(confirmed$Date,confirmed$Total_Cases),
       col="green")


## Polynomial quadratic (squared) linear model:
m2 <- lm(Total_Cases ~ day +I(day^2), data=train)
m2forecast <- predict(m2, newdata = predict, 
                      interval = "confidence")
m2forecast <- as.data.frame(m2forecast)
colnames(m2forecast) <- c("pol_Forecast","pol_lwr","pol_upr")
m2forecast$day <- 131:145


## Generalized Additive Model:
# bs="cr" selects a cubic spline basis, 
# k selects the dimension of the basis (degree of smoothing) 
# 3 degrees were identified as good fit using lowess plot = Blue line
m4 <- gam(Total_Cases ~ s(day,bs="cr",k=3), data = train)
m4forecast <- predict(m4, newdata = predict, 
                      interval = "confidence")
m4forecast <- as.data.frame(m4forecast)
colnames(m4forecast) <- c("gam_Forecast")
m4forecast$day <- 131:145


#Visual Outputs
## Produces a scatterplot of moving days vs total values from quadratic model:
plot(Total_Cases ~ day, data=train)
## Adds the quadratic line to the plot:
lines(train$day, predict(m2))

#prophet visuals
plot(m, forecast)
prophet_plot_components(m, forecast)

# use the same ggplot code to plot different visual outputs. Updated accordingly.
ggplot(train, aes(x = day, y = Total_Cases)) + 
  geom_point() +
  lines(train$day, predict(m4)) +
  stat_smooth(method = "gam", col = "red") +
  labs(title="Covid-19 | Generalized Additive Model Total Cases vs Days") +
  theme_stata()


## Prepare prediction outputs into a single dataframe for comparison against real values
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
mforecast <- forecast
mforecast <- subset(mforecast, ds >= "2020-06-01")
mforecast <- subset(mforecast, select = -c(1:11))
mforecast <- subset(mforecast, select = -c(3,4))
mforecast <- mforecast[,c("yhat","yhat_lower","yhat_upper")]
colnames(mforecast) <- c("prophet_Forecast","prophet_lwr","prophet_upr")
mforecast$day <- 131:145

# merge prepared dataframes
comparison_df <- inner_join(inner_join(m1forecast, m2forecast),mforecast)
comparison_df <- inner_join(comparison_df, test)
comparison_df <- inner_join(comparison_df, m4forecast)

# Calculate MAPEs for each model
lm_mape <- MAPE(comparison_df$lm_Forecast,comparison_df$Total_Cases)
lm_mape

prophet_mape <- MAPE(comparison_df$prophet_Forecast,comparison_df$Total_Cases)
prophet_mape

pol_mape <- MAPE(comparison_df$pol_Forecast,comparison_df$Total_Cases)
pol_mape

# Gam mape was into a separed df, due to model output without lower and upper values
comp_m4_df <- inner_join(test, m4forecast)
m4_mape <- MAPE(comp_m4_df$gam_Forecast,comp_m4_df$Total_Cases)
m4_mape


comparison_df1 <- subset(comparison_df, select = c(1,4,5,8,11,12,13))

comparison_df1 <- comparison_df1[,c(5,2,1,3,4,7,6)]
colnames(comparison_df1) <- c("Date","day","lm_fcst","pol_fcst","prophet_fcst","gam_fcst","Actual_value")
