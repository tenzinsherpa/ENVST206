#activity3
ch4 <- read.csv("/Users/TenzinSherpa/Documents/a03/lemming_herbivory.csv")
View (ch4)

#it is easier to work with the data in the csv as factor data
ch4$herbivory <-as.factor(ch4$herbivory)

#make box plots in R
#plot(dependent variable (~ = depends on) independent variable)
plot(ch4$CH4_Flux ~ ch4$herbivory)

#another way to plot the data
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", 
     ylab="CH4 fluxes (mgC m –2 day–1) ")

#shapiro-wilk test
#null hypothesis: data is normally distributed
#alternative hypothesis: data is not normally distributed
#since the p-values for both Ex and Ctl is above 0.05, WE DO NOT REJECT THE NULL HYPOTHESIS
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#null hypothesis: no difference in variance among groups
#alternative hypothesis: there is a difference in variance among groups
#it is appropriate to use a t-test now.
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

#t-test
t.test(ch4$CH4_Flux ~ ch4$herbivory)

#question 2
help(t.test)

#read in insect data
datI <- read.csv("/Users/TenzinSherpa/Documents/a03/insect_richness.csv")

datI$urbanName <- as.factor(datI$urbanName)

#question 4
#run a shapiro-wilk test for all urbannames 
shapiro.test(datI$Richness[datI$urbanName == "Developed"])
shapiro.test(datI$Richness[datI$urbanName == "Dense"])
shapiro.test(datI$Richness[datI$urbanName == "Suburban"])
shapiro.test(datI$Richness[datI$urbanName == "Natural"])

#bartlett test
bartlett.test(datI$Richness ~ datI$urbanName)


#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run the ANOVA
in.aov <- aov(in.mod)
#print out ANOVA table
summary(in.aov)

#run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
#view results
tukeyT

#make a plot
#make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)

tapply(datI$Richness, datI$urbanName, "mean")

#set up contingency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")
#to see the values of the mosaic and know the expected values
species

#make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

#Conduct a chi-squared test
chisq.test(species)