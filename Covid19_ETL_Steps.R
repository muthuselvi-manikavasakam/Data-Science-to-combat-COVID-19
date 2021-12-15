# MS5103 Final Year Project, Group 13, COVID-19 Analysis

# clean environment, set working directory and load libraries
rm(list = ls())
setwd("C:\\Datasets\\Covid19\\")
library(reshape2)
library(dplyr)
library(naniar)
library(tidyverse)

#define https paths for datasources "new daily data is updated automatically", source: JHU CSSE - Johns Hopkins University
confirmed_path = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
deaths_path = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
recovered_path = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv' 

# *---1st Dataset---*
# ETL "confirmed" cases data:
confirmed <- read.csv(confirmed_path, header = TRUE, sep=",",
                      stringsAsFactors = FALSE, check.names=FALSE)
confirmed <- melt(confirmed, id = c(1:4))
confirmed <- subset(confirmed, select = -c(1,3,4))
colnames(confirmed) <- c("Country","Date","Total_Cases")
confirmed <- confirmed %>%
  select(Country, Date, Total_Cases) %>%
  group_by(Country, Date) %>%
  summarise(Total_Cases = sum(Total_Cases))

#Create "NewCases" KPI with daily new confirmed cases by country, nan = 0
confirmed$New_Cases <- ave(confirmed$Total_Cases, 
                           factor(confirmed$Country),
                           FUN=function(x) c(0,diff(x)))

#create "GrowthRate" KPI, with diff change Day - 1 Day, replace inf and nan by 0
confirmed$Growth_Rate <- round(ave(confirmed$Total_Cases, 
                                  factor(confirmed$Country), 
                                  FUN=function(x) x/lag(x) -1), 
                                  digits = 4)
is.na(confirmed$Growth_Rate) <- sapply(confirmed$Growth_Rate, is.infinite)
confirmed$Growth_Rate[is.na(confirmed$Growth_Rate)] <- 0               

#Due to global reporting adjustments, there are NewCases with negative values
nrow(confirmed[confirmed$New_Cases<0,])

#since it's only few records (0,001) and negative results will impact other variables
#replace negative values in "NewCases" and "GrowthRate" by Zero
confirmed$New_Cases <- ifelse(confirmed$New_Cases < 0,0,confirmed$New_Cases)
confirmed$Growth_Rate <- ifelse(confirmed$Growth_Rate < 0,0,confirmed$Growth_Rate)

# fix Date variable extraction formatting 
confirmed$Date <- as.Date(as.character(confirmed$Date),format = "%m/%d/%y")

# create "Double_Rate" KPI to measure double rate, by countries, 7 day period, > 100 cases.
confirmed <- confirmed %>%
     group_by(Country) %>%
     mutate(Cases_doub_7days = 7*log(2)/log(Total_Cases/lag(Total_Cases,7))) %>%
     replace_with_na(replace = list(Cases_doub_7days = c("NaN", 0, "Inf"))) %>%
     mutate(tc_exceeds_50 = as.numeric(Total_Cases >= 50),
     tc_date = case_when(tc_exceeds_50 == 0 ~ as.Date(NA),
                         tc_exceeds_50 == cumsum(tc_exceeds_50) ~ Date,
                         tc_exceeds_50 < cumsum(tc_exceeds_50) ~ as.Date(NA))) %>%
     fill(tc_date) %>% 
     mutate(days_since_50_cases = as.numeric(difftime(Date, tc_date, units = "days")))
confirmed <- as.data.frame(confirmed) #paste grouped tbl into df


# *---2nd Dataset---*
# ETL "Recovered" cases data from Https web address, JHU CSSE - Johns Hopkins University
recovered <- read.csv(recovered_path, header = TRUE, sep=",",
                      stringsAsFactors = FALSE, check.names=FALSE)
recovered <- melt(recovered, id = c(1:4))
recovered <- subset(recovered, select = -c(1,3,4))
colnames(recovered) <- c("Country","Date","Total_Recovered")
recovered <- recovered %>%
  select(Country, Date, Total_Recovered) %>%
  group_by(Country, Date) %>%
  summarise(Total_Recovered = sum(Total_Recovered))
recovered$Date <- as.Date(as.character(recovered$Date),format = "%m/%d/%y")
recovered <- as.data.frame(recovered)


# *---3rd Dataset---*
# ETL "deaths" cases data from Https web address, JHU CSSE - Johns Hopkins University
deaths <- read.csv(deaths_path, header = TRUE, sep=",",
                   stringsAsFactors = FALSE, check.names=FALSE)
deaths <- melt(deaths, id = c(1:4))
deaths <- subset(deaths, select = -c(1,3,4))
colnames(deaths) <- c("Country","Date","Total_Deaths")
deaths <- deaths %>%
  select(Country, Date, Total_Deaths) %>%
  group_by(Country, Date) %>%
  summarise(Total_Deaths = sum(Total_Deaths))
deaths$Date <- as.Date(as.character(deaths$Date),format = "%m/%d/%y")

#new daily deaths measure
deaths$New_deaths <- ave(deaths$Total_Deaths, 
                           factor(deaths$Country),
                           FUN=function(x) c(0,diff(x)))
deaths <- as.data.frame(deaths)


# *---4th Dataset---*
# ETL from Google Big Query Mobility results
mobility <- read.csv("big_query_mobility.csv", header = TRUE, sep=",",
                     stringsAsFactors = FALSE, check.names=FALSE)
mobility <- subset(mobility, select = -c(1,3))
colnames(mobility) <- c("Country","Date","Retail_Recreation","Transit_Stations","Workplaces")
mobility$Date <- as.Date(mobility$Date)
mobility$Retail_Recreation <- mobility$Retail_Recreation/100 #get % numerical
mobility$Transit_Stations <- mobility$Transit_Stations/100 
mobility$Workplaces <- mobility$Workplaces/100 
mobility <- mobility[order(mobility[,1], mobility[,2]),]
row.names(mobility) <- NULL #reset index after sorted
#mobility data is not available for 66 countries from the covid19 development data
#China, Russia among other small ones.


# *---5th Dataset---*
# ETL from Google Big Query Gdelt Covid onlinenews
onlinenews <- read.csv("big_query_onlinenews.csv", header = TRUE, sep=",",
                       stringsAsFactors = FALSE, check.names=FALSE)
onlinenews$Date <- as.Date(onlinenews$Date)
onlinenews <- onlinenews[order(onlinenews[,1], onlinenews[,2]),]
row.names(onlinenews) <- NULL #reset index after sorted
#count of news data, not available for 18 countries from Covid19 development data.


# *---6th Dataset---*
# ETL from Worlwide 2020 Population by country dataset, provided by https://ourworldindata.org/
population <- read.csv("world_countrywise_population.csv", header = TRUE, sep=",",
                       stringsAsFactors = FALSE, check.names=FALSE)
population <- subset(population, select = -c(1,3,4))
colnames(population) <- c("Country","Population")


# *---Final Dataset---*
# join Confirmed,Recovered,Deaths for a complete Covid 19 timeseries df
covid19 <- inner_join(inner_join(confirmed, recovered),deaths)
covid19$Country[covid19$Country == "US"] <- "United States" #naming diff
covid19$Country[covid19$Country == "Korea, South"] <- "South Korea" #naming diff

# merge Covid development with available population data
covid19 <- merge(covid19,population,all.x=TRUE)
covid19$Cases_Per_100k <- (covid19$Total_Cases/covid19$Population)*100000

# merge Covid development + population with available covid onlinenews data
covid19 <- merge(covid19,onlinenews,all.x=TRUE)

# merge Covid development + population + Onlinenews with available mobility data
covid19 <- merge(covid19,mobility,all.x=TRUE)

#filter out, cut off date decided for analysis < 16/06/2020
covid19 <- subset(covid19, Date < "2020-06-16")

# Export Final Dataset as csv file for further analysis in:
# IBM SPSS: better visual outputs from Pearson correlation analysis
# Tableau: better and easier timeseries graphical representations
#write.csv(covid19, "c:/Datasets/Covid19/deaths.csv", row.names=FALSE)
#write.table(covid19, "c:/Datasets/Covid19/Covid19_Dataset.txt", sep="\t", row.names=FALSE)

