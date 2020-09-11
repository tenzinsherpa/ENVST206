#activity3
ch4 <- read.csv("/Users/TenzinSherpa/Documents/a03/lemming_herbivory.csv")
View (ch4)

#it is easier to work with the data in the csv as factor data
ch4$herbivory <-as.factor(ch4$herbivory)

#make box plots in R
#plot(dependent variable (~ = depends on) independent variable)
plot(ch4$CH4_Flux ~ ch4$herbivory)

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
