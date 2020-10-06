#activity 5

datW <- read.csv("/Users/TenzinSherpa/Documents/ENVST206 Data/a05/noaa2011124.csv")
View(datW)
#specify that the name column should be a factor
datW$NAME<- as.factor(datW$NAME)
#set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS
#make a dataframe with just precipitation, year, and site name
#remove NA using na.omit
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))
#total annual precipitation (mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)
#use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)
#rename columns
colnames(precip) <- c("NAME","year","totalP")
#add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x


#make a new dataframe
pr <- precip[precip$ncount >=364, ]

#look at only livermore california and morrisville new york preciptiation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

#question 2
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(0,1600, by=400), las=2 )
#add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend

legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn

#question 3
#make a dataframe with just precipitation, year, and site name
#remove NA using na.omit
datE <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))


#use aggregate to get total annual precipitation
Tmaxave <- aggregate(datE$TMAX, by=list(datE$NAME,datE$year), FUN="mean", na.rm=TRUE)
#rename columns
colnames(Tmaxave) <- c("NAME","year","TMAX")
#add the x column from aggregate looking at the length of observations in each year
Tmaxave$ncount <- aggregate(datE$TMAX, by=list(datE$NAME,datE$year), FUN="length")$x

#make a new dataframe
tm <- Tmaxave[Tmaxave$ncount >=364, ]

#look at only mandan north dakota and morrisville new york max temperature
nd <- tm[tm$NAME == nameS[3], ]
newy <- tm[tm$NAME == nameS[5], ]

plot(nd$year, nd$TMAX,
     type = "b",
     pch = 19,
     ylab = "Average Annual Maximum Temperature (C)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(5, 15))
#add y axis
axis(2, seq(5,15, by=5), las=2 )
#add new york
points(newy$year, newy$TMAX,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend

legend("bottomleft", #position
       c("North Dakota", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn

install.packages("ggplot2")
library(ggplot2)

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation") #make axis labels

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+ #make axis labels
  theme_classic() #change plot theme

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("#7FB3D5","#34495E", "#E7B800", "#FC4E07","#26A69A"))

#question 5
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("#E69F00","#56B4E9", "#D55E00", "#F0E442","#0072B2"))

ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic() #git rid of ugly gridlines

sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]

#specify date format
#%Y means a four number year 
#- indicates that the date uses dashes to separate
#%m means month
#%d means day
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")


#question 7
 ggplot(data = sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

 ggplot(data=sub, aes(x=DATE, y=PRCP))+
   geom_col(fill="royalblue3")+
   theme_classic()+
   labs(x="year", y="Daily precipitation (mm)")

#question 8
 sub <- datW[datW$NAME == nameS[2] & datW$ year == 1974,]
 
 #specify date format
 #%Y means a four number year 
 #- indicates that the date uses dashes to separate
 #%m means month
 #%d means day
 sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")
 
 ggplot(data = sub, aes(x=DATE, y=TMAX))+
   geom_point()+
   geom_path()+
   theme_classic()+
   labs(x="year", y="Maximimum temperature (C)")
 
 ggplot(data=sub, aes(x=DATE, y=PRCP))+
   geom_col(fill="royalblue3")+
   theme_classic()+
   labs(x="year", y="Daily precipitation (mm)")
 
#question 9
 mnaz <- datW[datW$NAME == nameS[3] & datW$year >= 2000,]
 mnaz$DATE <- as.Date(mnaz$DATE,"%Y-%m-%d")
 ggplot(data = mnaz, aes(x=DATE, y=TMIN))+
   geom_point()+
   geom_path()+
   theme_classic()+
   labs(x="year", y="Minimum temperature (C)")
