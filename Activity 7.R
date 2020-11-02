#activity 7

calFire <- read.csv("/Users/TenzinSherpa/Documents/ENVST206 Data/calfire/ad_viz_plotval_data.csv")
View(calFire)
caldata <- read.csv("/Users/TenzinSherpa/Documents/ENVST206 Data/calfire/mapdataall.csv")
View(caldata)

#question 5
#calculating the mean of acres burned: 4445.786
mean(caldata$incident_acres_burned)
#standard deviation: 32059.71
sd(caldata$incident_acres_burned)

#plot of data
plot(caldata$year, caldata$incident_acres_burned,
     pch= 19,
     ylab= "Acres burned",
     xlab= "year",
     col= "black"
     )

