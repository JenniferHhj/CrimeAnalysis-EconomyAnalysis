library("readxl")
library("tidyverse")
library("broom")
library("lmtest")
library("stargazer")

library("knitr")
library("plm")
library("plyr")
library("ggplot2")
library("tidyr")
library("Hmisc")
install.packages("Hmisc")
install.packages("corrplot")
library(estimatr)


# File name with path
library(readr)
crime_data <- read_csv("C:/Users/henry bi/Downloads/crimedata_try2.csv")
df <- crime_data

reg1 <- lm(ViolentCrimesPerPop~PctLess9thGrade+PctBSorMore+medIncome	+pctWWage	+pctUrban	+racepctblack	
           +racePctWhite	+racePctAsian	+racePctHisp	+PopDens	+PctUnemployed
           +agePct12t29	+PctPopUnderPov	+PctNotSpeakEnglWell	+MedRent	
           +NumInShelters	+NumStreet +PctNotHSGrad, data = df)

summary(reg1)

df$ViolentCrimesPerPopPercent <- (crime_data$ViolentCrimesPerPop / 100000)*100


reg2 <- lm(ViolentCrimesPerPopPercent~PctLess9thGrade+PctBSorMore	+medIncome	+pctWWage	+pctUrban	+racepctblack	
           +racePctWhite	+racePctAsian	+racePctHisp	+PopDens	+PctUnemployed
           +agePct12t29	+PctPopUnderPov	+PctNotSpeakEnglWell	+MedRent	
           +NumInShelters	+NumStreet , data = df)

summary(reg2)

stargazer(reg2,
          type= "html",
          out= "termcolumns.doc", 
          title= "Term Project Columns", 
          align = TRUE)


## the matrix of scatter plot 
df %>%
  gather(-ViolentCrimesPerPop, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = ViolentCrimesPerPopPercent)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# Henry Bi Transformation
df$medIncome <- log(df$medIncome)

# robust linear regression 
fit_robust <- lm_robust(ViolentCrimesPerPop ~ PctBSorMore + PctNotHSGrad + medIncome+racepctblack +PctUnemployed
                        +agePct12t29	+PctNotSpeakEnglWell +NumStreet, data = df)

ggplot(df, aes(x= log(medIncome), y= ViolentCrimesPerPopPercent)) + geom_point()
ggplot(df, aes(x= medIncome, y= ViolentCrimesPerPopPercent)) + geom_point()
ggplot(df, aes(x= log(medIncome) , y= ViolentCrimesPerPop)) + geom_point()
ggplot(df, aes(x= medIncome , y= ViolentCrimesPerPop)) + geom_point()




#  selected data and easy MLR
reg3 <- lm(ViolentCrimesPerPop ~ PctBSorMore + PctNotHSGrad + medIncome+racepctblack +PctUnemployed
           +agePct12t29	+PctNotSpeakEnglWell +NumStreet, data = df)
summary(reg3)

summary(fit_robust)


# residual, heteroskedastic test Henry Bi 
reg4 <- lm(reg3$residuals^2 ~ PctBSorMore + PctNotHSGrad + medIncome+racepctblack +PctUnemployed
           +agePct12t29	+PctNotSpeakEnglWell +NumStreet, data = df)
summary(reg4)


# fixes using wls Sven & henry Bi
lnres3 <- log(reg3$residuals^2)
weights_reg3 <-lm(lnres3 ~ PctBSorMore + PctNotHSGrad + medIncome+racepctblack +PctUnemployed
                  +agePct12t29	+PctNotSpeakEnglWell +NumStreet, data = df)
hhat <- exp(weights_reg3$fitted.values)

wls.reg3 <- lm(ViolentCrimesPerPop ~ PctBSorMore + PctNotHSGrad + medIncome+racepctblack +PctUnemployed
               +agePct12t29	+PctNotSpeakEnglWell +NumStreet, weights = 1/hhat, data =df)
summary(wls.reg3)
summary(fit_robust)
summary(reg3)


# reset test Jennifer
resettest(ViolentCrimesPerPop~ PctBSorMore + PctNotHSGrad + medIncome+racepctblack +PctUnemployed
          +agePct12t29	+PctNotSpeakEnglWell +NumStreet, power=2:3,type=c('fitted'),data = df)


# auto correlation test Henry BI OVB
dwtest(fit_robust)
dwtest(reg3)
# fixed effects 



