#activity 2
#the examples are on the top and the activity problems that require coding are at the bottom (for now)


heights <- c(3,2,3)

datW <- read.csv("/Users/TenzinSherpa/Documents/ENVST206 Data/a02/noaa2011124.csv")

help(matrix)

#create a matrix with 2 columns that fill in by row
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#create a matrix with two columns that fill in by column
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#locate a specific area of the matrix formatted [row,column]
Mat.bycol[1,2]

#a space in the square bracket after or before a comma means all items in that slot.
#look at all values in row 1
Mat.bycol[1,]
#look at all values in column 2
Mat.bycol[,2]

#access to the excel files
datW <- read.csv("/Users/TenzinSherpa/Documents/ENVST206 Data/a02/noaa2011124.csv")

#get more information about the dataframe
str(datW)

#converting name sites into factors
datW$NAME <-as.factor(datW$NAME)

#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
#we should get NA because data is missing from the data set
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#next look at the standard deviation
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

help(dnorm)

#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#qnorm gives me the value at which all values and below equal the probability in my argument
#Here I'm calculating the value of the 95th quantile or a probability of 0.95
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


#question 2
#numeric data vector
number <- c(3.4, 5.2, 9.7, 45.8, 67.9)
number
#character data vector
character <- c("dog", "cat", "fish","turtle","rhino")
character
#integer data vector
vecExample<- c(5, 67, 234, 1, 57)
vecExample
#factor data vector using the character data vector
character <- c("dog", "cat", "fish","turtle","rhino")
vecFactor <- as.factor(character)
vecFactor

#question 3
help("hist")

#question 4
hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

#question 5
#find the current high temperature threshold. 
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#then you want to find how often that temperature and values greater than it will occur under the new mean
1 - pnorm(18.51026,
          mean(datW$TAVE[datW$siteN == 1]+4,na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#question 6
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily percipitation (mm)", 
     ylab="Relative freqauency",
     col="grey75",
     border="white")

#question 7
totalPRCP <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN= "sum", na.rm= TRUE)
colnames(totalPRCP) <-c("NAME", "YEAR", "PRCP")
totalPRCP

#question 8
#Aberdeen, WA US
hist(totalPRCP$PRCP[totalPRCP$NAME == "ABERDEEN, WA US"],
     freq=FALSE,
     main = "ABERDEEN, WA US",
     xlab = "Annual Percipitation (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")
#MANDAN EXPERIMENT STATION, ND US
hist(totalPRCP$PRCP[totalPRCP$NAME == "MANDAN EXPERIMENT STATION, ND US"],
     freq=FALSE, 
     main = "MANDAN EXPERIMENT STATION, ND US",
     xlab = "Annual Percipitation (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

#question 9
#how likely a year of precipitation of 700mm or less 
pnorm(700,
          mean(totalPRCP$PRCP[totalPRCP$NAME == "MANDAN EXPERIMENT STATION, ND US"],na.rm=TRUE),
          sd(totalPRCP$PRCP[totalPRCP$NAME == "MANDAN EXPERIMENT STATION, ND US"],na.rm=TRUE))

pnorm(700,
       mean(totalPRCP$PRCP[totalPRCP$NAME == "ABERDEEN, WA US"],na.rm=TRUE),
       sd(totalPRCP$PRCP[totalPRCP$NAME == "ABERDEEN, WA US"],na.rm=TRUE))


