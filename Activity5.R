#activity 5

datW <- read.csv("/Users/TenzinSherpa/Documents/ENVST206 Data/a05/noaa2011124.csv")
datW$NAME <- as.factor(datW$NAME)

#create vector
nameS <-levels(datW$NAME)
nameS[2]

#make dataframe
datP <- na.omit(data.frame(PRCP = datW$PRCP,
                   NAME = datW$NAME,
                   year = datW$year))

#total annual precipitation for site
pr <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN= "sum")
View(pr)
colnames(pr) <- c("NAME", "year", "total p")
pr$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN= "length")$x

pr <- pr[pr$ncount >= 364,]

#install.packages("ggplot2")
#base r plot
plot(pr$year, pr$totalP)

ggplot(data= pr,
       aes(x= year,
           y= totalP,
           color = NAME))+
  geom_point()+
  geom_path()+
  labs(x= "year", y= "Annual precipitation (mm)")+
  theme_classic()+
  

