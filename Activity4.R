#activity 4
datB <- read.csv("/Users/TenzinSherpa/Documents/ENVST206 Data/a04/beaver_dam.csv")
head(datB)
View(datB)

#plot example in class
plot(datB$dams.n, datB$area.ha, pch = 19)

#plot the relationship between the presence of beaver dams in the total surface area of water.
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

#set up regressio
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
View(dam.mod)

#get standardized residuals
dam.res <- rstandard(dam.mod)

qqnorm(dam.res)
qqline(dam.res)

#will test if your data is significant enough to be non-normal
shapiro.test(dam.res)

#make residual plot
plot(datB$dams.n, datB$dam.res, pch= 19)
#more descriptive residual plot
plot(datB$dams.n, dam.res, 
     xlab = "beaver damns", 
     ylab = "standardized residual")
abline(h=0)
#we discover that there are no major concerns about the regression assumptions around the residuals.

#this will print out the regression table
summary(dam.mod)

#make plot of beaver dams and surface water
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")
#add regression line
#make line width thicker
abline(dam.mod, lwd=2)

pheno <- read.csv("/Users/TenzinSherpa/Documents/ENVST206 Data/a04/red_maple_pheno.csv")
View(pheno)
#set up panel of plots with one row and two columns
par(mfrow=c(1,2))

#question 3
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")

plot(pheno$Lat,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Latitude")

plot(pheno$elev,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Elevation (m)")

#since urban and rural are names, we must make them factors first before attempting to run our data
pheno$siteDesc <- as.factor(pheno$siteDesc)
plot(pheno$siteDesc,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "urban/rural")

plot(pheno$Prcp,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

dev.off()

plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

#question 5
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)

#question 6
mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)
mlr.res <- rstandard(mlr)
mlFitted <- fitted(mlr)
 
qqnorm(mlr.res)
qqline(mlr.res)

#code for the residual plot
plot(mlFitted, mlr.res,
     xlab = "fitted values", 
     ylab = "standardized residual")
abline(h=0)

summary(mlr)


