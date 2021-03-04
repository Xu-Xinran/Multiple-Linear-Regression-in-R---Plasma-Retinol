#R code
#Import data
plasmaRetinol <- read.delim("~/Documents/6214/6214 final project/plasmaRetinol.txt", header=FALSE)
View(plasmaRetinol)

colnames(plasmaRetinol) <- c("age","sex","smoking","quetelet","vitamin","calories","fat","fiber","alcohol","choles terol","betadiet","retdiet","betaplasma","retplasma")

summary(plasmaRetinol)

#Visualize Data
#Boxplot of Plasma Retinol and Plasma Beta-carotene by sex par(mfrow=c(1,2))
boxplot(retplasma~sex,data=plasmaRetinol, main="Boxplot of Plasma Retinol",
        xlab="sex", ylab="retplasma") 
boxplot(betaplasma~sex,data=plasmaRetinol, main="Boxplot of Beta-carotene",
        xlab="sex", ylab="betaplasma")
#Boxplot of Plasma Retinol and Plasma Beta-carotene by vitamin 
boxplot(retplasma~vitamin,data=plasmaRetinol, main="Boxplot of Plasma Retinol",
        xlab="vitamin", ylab="retplasma") 
boxplot(betaplasma~vitamin,data=plasmaRetinol, main="Boxplot of Beta- carotene",
        xlab="vitamin", ylab="betaplasma")
#Boxplot of Plasma Retinol and Plasma Beta-carotene by smoking 
boxplot(retplasma~smoking,data=plasmaRetinol, main="Boxplot of Plasma Retinol",
        xlab="smoking", ylab="retplasma") 
boxplot(betaplasma~smoking,data=plasmaRetinol, main="Boxplot of Beta- carotene",
        xlab="smoking", ylab="betaplasma")

#Simple Histogram
hist(plasmaRetinol$age,main="Histogram of Age",xlab="Age")
par(mfrow=c(2,2))
hist(plasmaRetinol$quetelet,main="Histogram of Quetelet",xlab="Quetelet") hist(plasmaRetinol$calories,main="Histogram of Calories",xlab="Calories") hist(plasmaRetinol$fat,main="Histogram of Fat",xlab="Fat") hist(plasmaRetinol$fiber,main="Histogram of Fiber",xlab="Fiber")
par(mfrow=c(2,2))
hist(plasmaRetinol$alcohol,main="Histogram of Alcohol",xlab="Alcohol") hist(plasmaRetinol$cholesterol,main="Histogram of Cholestrol",xlab="Cholestrol") hist(plasmaRetinol$betadiet,main="Histogram of Betadiet",xlab="Betadiet") hist(plasmaRetinol$retdiet,main="Histogram of Ritdiet",xlab="Ritdiet")

#scatter plot of Plasma Beta-carotene 
plot(plasmaRetinol$age,plasmaRetinol$betaplasma,main="Scatterplot of Plasma Beta-carotene and Age",xlab="Age", ylab="Plasma Beta-carotene") 
pairs(~betaplasma+quetelet+calories+fat+fiber,data=plasmaRetinol,
      main="Scatterplot Matrix of Plasma Beta-carotene 1") 
pairs(~betaplasma+alcohol+cholesterol+betadiet+retdiet,data=plasmaRetinol,
      main="Scatterplot Matrix of Plasma Beta-carotene 2")

#scatter plot of Plasma Retinol 
plot(plasmaRetinol$age,plasmaRetinol$retplasma,main="Scatterplot of Plasma Retinol and Age",xlab="Age", ylab="Plasma Retinol") 
pairs(~retplasma+quetelet+calories+fat+fiber,data=plasmaRetinol,
      main="Scatterplot Matrix of Plasma Retinol 1") 
pairs(~retplasma+alcohol+cholesterol+betadiet+retdiet,data=plasmaRetinol,
      main="Scatterplot Matrix of Plasma Retinol 2")

#age
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) 
hist(plasmaRetinol$age,main="Histogram of Age",xlab="Age") 
plot(plasmaRetinol$age,plasmaRetinol$betaplasma,main="Scatterplot of Plasma Beta-carotene and Age",xlab="Age", ylab="Plasma Beta-carotene") 
plot(plasmaRetinol$age,plasmaRetinol$retplasma,main="Scatterplot of Plasma Retinol and Age",xlab="Age", ylab="Plasma Retinol")

#cleaning data
plasmaRetinol_sub<- subset(plasmaRetinol,calories<4000&fat<150&fiber<30&alcohol<20&cholesterol< 600 &betadiet<7000 &retdiet<2200 &betaplasma<500&retplasma<1200) 
View(plasmaRetinol_sub)
plasmaRetinol_sub<- subset(plasmaRetinol,calories<4000&fat<150&fiber<30&alcohol<20&cholesterol<600 &betadiet<7000 &retdiet<2200)

View(plasmaRetinol_sub)

#scatterplot after cleaning data
#scatter plot of Plasma Beta-carotene after deleting 
pairs(~betaplasma+quetelet+calories+fat+fiber,data=plasmaRetinol_sub,
      main="Scatterplot Matrix of Plasma Beta-carotene 1 after Deleting Outliers") 
pairs(~betaplasma+alcohol+cholesterol+betadiet+retdiet,data=plasmaRetinol_sub, 
      main="Scatterplot Matrix of Plasma Beta-carotene 2 after Deleting Outliers")

#scatter plot of Plasma Retinol after deleting 
pairs(~retplasma+quetelet+calories+fat+fiber,data=plasmaRetinol_sub,
      main="Scatterplot Matrix of Plasma Retinol 1 after Deleting Outliers") 
pairs(~retplasma+alcohol+cholesterol+betadiet+retdiet,data=plasmaRetinol_sub,
      main="Scatterplot Matrix of Plasma Retinol 2 after Deleting Outliers")

#factor categorical variables 
plasmaRetinol_sub$sex=as.factor(plasmaRetinol_sub$sex) 
plasmaRetinol_sub$smoking=as.factor(plasmaRetinol_sub$smoking) 
plasmaRetinol_sub$vitamin=as.factor(plasmaRetinol_sub$vitamin)

#################### Plasma Beta-carotene ################## 
#regression analysis
lm1<-lm(betaplasma~ age+sex+smoking+quetelet+vitamin+calories+fat+fiber+alcohol+cholesterol+betadi et+retdiet, data=plasmaRetinol_sub)
summary(lm1)
#Diagnostic Plots for Plasma Beta-carotene par(mfrow=c(2,2))
plot(lm1)

#log betaplasma
logbetaplasma=log(plasmaRetinol_sub$betaplasma)
lm2<-lm(logbetaplasma~ age+sex+smoking+quetelet+vitamin+calories+fat+fiber+alcohol+cholesterol+betadi et+retdiet, data=plasmaRetinol_sub)
summary(lm2)

#Diagnostic Plots for Log Plasma Beta-carotene
par(mfrow=c(2,2))
plot(lm2)

#model fit 
library(MASS) 
stepAIC(lm2) 
AIC.choice <- lm2 
summary(AIC.choice) 
AIC(lm2)

#Forward,Backword,and Stepwise selection 
library(MASS)
step.forward <- stepAIC(lm2, direction="forward") 
step.forward$anova
step.backward <- stepAIC(lm2, direction="backward") 
step.backward$anova
step.both <- stepAIC(lm2, direction="both") 
step.both$anova

#Best Subset Selection
library(leaps)
attach(plasmaRetinol_sub)
leaps<- regsubsets(logbetaplasma~age+sex+smoking+quetelet+vitamin+calories+fat+fiber +alcohol+cholesterol+betadiet+retdiet,data=plasmaRetinol_sub,nbest=1) 
summary(leaps)

#final model
lm5<-lm(logbetaplasma~ age + smoking + quetelet + vitamin + alcohol + betadiet, data=plasmaRetinol_sub)
summary(lm5)
stepAIC(lm5)
AIC.choice <- lm5
summary(AIC.choice)
AIC(lm5)

#################### Plasma Retinol ##################
#regression analysis
lm3<-lm(retplasma~ age+sex+smoking+quetelet+vitamin+calories+fat+fiber+alcohol+cholesterol+betadi et+retdiet, data=plasmaRetinol_sub)
summary(lm3)
#Diagnostic Plots for Plasma Retinol 
par(mfrow=c(2,2))
plot(lm3)
#model fit 
library(MASS) 
stepAIC(lm3) 
AIC.choice <- lm3 
summary(AIC.choice) 
AIC(lm3)

#Forward,Backword,and Stepwise selection 
library(MASS)
step.forward <- stepAIC(lm3, direction="forward") 
step.forward$anova
step.backward <- stepAIC(lm3, direction="backward") 
step.backward$anova
step.both <- stepAIC(lm3, direction="both") 
step.both$anova

#Best Subset Selection
library(leaps)
attach(plasmaRetinol_sub)
leaps<- regsubsets(retaplasma~age+sex+smoking+quetelet+vitamin+calories+fat+fiber+alc ohol+cholesterol+betadiet+retdiet,data=plasmaRetinol_sub,nbest=1) summary(leaps)

#final model
lm6<-lm(retplasma~ age + sex + alcohol, data=plasmaRetinol_sub) 
summary(lm6)
stepAIC(lm6)
AIC.choice <- lm6
summary(AIC.choice)
AIC(lm6)

#################### Plasma Retinol by Plasma Beta-carotene ##################
#regression analysis
lm4<-lm(retplasma~ age+sex+smoking+quetelet+vitamin+calories+fat+fiber+alcohol+cholesterol+betadi et+retdiet+betaplasma, data=plasmaRetinol_sub)
summary(lm4)

#Diagnostic Plots for Plasma Retinol by Plasma Beta-carotene 
par(mfrow=c(2,2))
plot(lm4)

#model fit 
library(MASS) 
stepAIC(lm4) 
AIC.choice <- lm4 
summary(AIC.choice) 
AIC(lm4)

#Forward,Backword,and Stepwise selection 
library(MASS)
step.forward <- stepAIC(lm4, direction="forward") 
step.forward$anova
step.backward <- stepAIC(lm4, direction="backward")
step.backward$anova
step.both <- stepAIC(lm4, direction="both") 
step.both$anova

#Best Subset Selection
library(leaps)
attach(plasmaRetinol_sub)
leaps<- regsubsets(retaplasma~age+sex+smoking+quetelet+vitamin+calories+fat+fiber+alc ohol+cholesterol+betadiet+retdiet+betaplasma,data=plasmaRetinol_sub,nbest=1) 
summary(leaps)
