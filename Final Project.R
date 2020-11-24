#final project
#objective: observing relationship between fires in Napa 

#install packages
install.packages("lubridate")
install.packages("tidyverse")
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)

#read in datasets needed to complete this project (California fire data)
calFire <- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/mapdataall.csv")
View(calFire)
#reformatting the dates so they could be read in for graphs and other dataframes later on
calFire$Date <- as.Date(calFire$incident_date_created, "%m/%d/%y %H:%M")

#col in calFire dataframe that just gives the year the fire occurred
calFire$year <- year(calFire$dates)

#read in datasets needed to complete this project (Napa, CA air quality data)
calAir2013 <- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/ad_viz_plotval_data Napa 2013.csv")
calAir2014 <- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/ad_viz_plotval_data Napa 2014.csv")
calAir2015 <- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/ad_viz_plotval_data Napa 2015.csv")
calAir2016 <- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/ad_viz_plotval_data Napa 2016.csv")
calAir2017<- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/ad_viz_plotval_data Napa 2017.csv")
calAir2018 <- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/ad_viz_plotval_data Napa 2018.csv")
calAir2019 <- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/ad_viz_plotval_data Napa 2019.csv")
calAir2020 <- read.csv("/Volumes/Tenzin USB/ENVST206 Data/calfire/ad_viz_plotval_data Napa 2020.csv")

#FIRST SCATTER PLOT CODE

#dataframe for Napa
unique(calFire$incident_county)
#napalocations <- c("Napa", "Napa, Solano", "Napa, Sonoma", "Napa, Yolo", "Napa, Sonoma, Lake, Yolo, Solano")

#all fires that happened in Napa
napaFire <- calFire[calFire$incident_county ==  "Napa"|calFire$incident_county == "Napa, Solano"|
                      calFire$incident_county == "Napa, Sonoma"|
                      calFire$incident_county == "Napa, Yolo"|
                      calFire$incident_county == "Napa, Sonoma, Lake, Yolo, Solano",] 
View(napaFire)

#how many fires occurred in Napa each year
firecount <- aggregate(napaFire$year, by=list(napaFire$year), FUN= "length")
#renaming the columns to note what each column means
colnames(firecount) <- c("Year", "nfires")

#create a scatter plot for the firecount
plot(firecount$Year, firecount$nfires,
     xlab= "Year",
     ylab= "Number of Fires",
     main= "Number of Fires per Year from 2013-2020",
     pch= 19)


#CODE FOR SECOND GRAPH
#in this part of the code, I am choosing which fire to focus in on (I chose 2017)
#also converting dates to day of year for the third graphs

calAir2013$dates <- as.Date(calAir2013$Date, "%m/%d/%Y")
days2013 <- lubridate::yday(calAir2013$dates)
View(days2013)

calAir2014$dates <- as.Date(calAir2014$Date,  "%m/%d/%Y")
days2014 <- lubridate::yday(calAir2014$dates)

calAir2015$dates <- as.Date(calAir2015$Date,  "%m/%d/%Y")
days2015 <- lubridate::yday(calAir2015$dates)

calAir2016$dates <- as.Date(calAir2016$Date,  "%m/%d/%Y")
days2016 <- lubridate::yday(calAir2016$dates)

calAir2017$dates <- as.Date(calAir2017$Date, "%Y-%m-%d")
days2017 <- lubridate::yday(calAir2017$dates)

calAir2018$dates <- as.Date(calAir2018$Date,  "%m/%d/%Y")
days2018 <- lubridate::yday(calAir2018$dates)

calAir2019$dates <- as.Date(calAir2019$Date,  "%m/%d/%Y")
days2019 <- lubridate::yday(calAir2019$dates)

calAir2020$dates <- as.Date(calAir2020$Date,  "%m/%d/%Y")
days2020 <- lubridate::yday(calAir2020$dates)

#fires in Napa in 2017 to choose which fire 
napaFire2017 <- napaFire[napaFire$year == 2017]
View(napaFire2017)

#creating a dataframe subset from the 2017 air quality data with just the dates one week before and two weeks after the fire on 2017-10-08
mainNapaFire2017 <- calAir2017[calAir2017$Date == "2017-10-01"| 
                                 calAir2017$Date == "2017-10-02"|
                                 calAir2017$Date == "2017-10-03"|
                                 calAir2017$Date == "2017-10-04"|
                                 calAir2017$Date == "2017-10-05"|
                                 calAir2017$Date == "2017-10-06"|
                                 calAir2017$Date == "2017-10-07"|
                                 calAir2017$Date == "2017-10-08"|
                                 calAir2017$Date == "2017-10-09"|
                                 calAir2017$Date == "2017-10-10"|
                                 calAir2017$Date == "2017-10-11"|
                                 calAir2017$Date == "2017-10-12"|
                                 calAir2017$Date == "2017-10-13"|
                                 calAir2017$Date == "2017-10-14"|
                                 calAir2017$Date == "2017-10-15"|
                                 calAir2017$Date == "2017-10-16"|
                                 calAir2017$Date == "2017-10-17"|
                                 calAir2017$Date == "2017-10-18"|
                                 calAir2017$Date == "2017-10-19"|
                                 calAir2017$Date == "2017-10-20"|
                                 calAir2017$Date == "2017-10-21",]
View(mainNapaFire2017)                                 


#actual graph code for the air quality in fire 10/08/2017.
ggplot(data = mainNapaFire2017, aes(x= Date, y= Daily.Mean.PM2.5.Concentration))+
  geom_point() +
  geom_path() +
  labs(x= "Date",
       y= "Daily Mean PM2.5 Concentration (ug/m3)",
       title= "Daily Mean PM2.5 Concentration in Napa, CA from 10/1/2017-10/21/2017")


#CODE FOR THIRD GRAPH

#here I am creating a new dataframe to combine all the PM2.5 concentrations from 2013-2020
airall <- data.frame(Days = c(days2013, days2014, days2015, days2016, days2017, days2018, days2019, days2020),
                     PM25 = c(calAir2013$Daily.Mean.PM2.5.Concentration, calAir2014$Daily.Mean.PM2.5.Concentration,
                              calAir2015$Daily.Mean.PM2.5.Concentration, calAir2016$Daily.Mean.PM2.5.Concentration,
                              calAir2017$Daily.Mean.PM2.5.Concentration, calAir2018$Daily.Mean.PM2.5.Concentration,
                              calAir2019$Daily.Mean.PM2.5.Concentration, calAir2020$Daily.Mean.PM2.5.Concentration),
                     Years = c(year(calAir2013$dates),year(calAir2014$dates),year(calAir2015$dates),year(calAir2016$dates),
                               year(calAir2017$dates), year(calAir2018$dates), year(calAir2019$dates), year(calAir2020$dates)))

#using the new dataframe, it is easier to make a plot of air quality each year for further analysis for any trends
ggplot(data = airall, aes(x= Days, y= PM25, color = as.factor(Years)))+
  geom_line(alpha= 0.5)+
  labs(main= "Daily Mean PM2.5 Concentration in Napa, CA from 2013-2020",
       x= "Day of Year",
       y= "Daily Mean PM2.5 Concentration (ug/m3)",
       color = "Year")+
  ggtitle("Daily Mean PM2.5 Concentration in Napa, CA from 2013-2020")


#STATISTICAL TEST - linear regression
#creating a new dataframe of all Daily Mean PM2.5 concentration combined to compare with nfires
calAircombined <- data.frame(meanPM2.5 = c(mean(calAir2013$Daily.Mean.PM2.5.Concentration),
                               mean(calAir2014$Daily.Mean.PM2.5.Concentration),
                               mean(calAir2015$Daily.Mean.PM2.5.Concentration),
                               mean(calAir2016$Daily.Mean.PM2.5.Concentration),
                               mean(calAir2017$Daily.Mean.PM2.5.Concentration),
                               mean(calAir2018$Daily.Mean.PM2.5.Concentration),
                               mean(calAir2019$Daily.Mean.PM2.5.Concentration),
                               mean(calAir2020$Daily.Mean.PM2.5.Concentration)),
                             Year = c(seq(2013, 2020)))

calFireAir <- full_join(firecount, calAircombined, by = "Year")

ggplot(data = calFireAir, aes(x= nfires, y= meanPM2.5))+
  geom_point()+
  ggtitle("Number of Fires vs. Daily Mean PM2.5 Concentration Each Year From 2013-2020")

cal.linReg <- lm(calFireAir$meanPM2.5 ~ calFireAir$nfires)
cal.res <- rstandard(cal.LinReg)
qqnorm(cal.res)
qqline(cal.res)

plot(calFireAir$nfires, cal.res, 
     xlab= "Number of Fires",
     ylab= "standardized residuals",
     pch= 19)
abline(h=0)

summary(cal.linReg)
