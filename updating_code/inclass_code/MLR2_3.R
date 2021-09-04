#
#      	    
#			Multiple Linear Regression
#	 Metrics & Variable Selection
#  Diagnostics & Transformations
#******************************************************

#load data
setwd(sourcedir)
source("AccidentInput.R")

acts <- file.inputl(traindir)

sapply(acts, dim)

totacts <- combine.data(acts)

# Build a data frame xdmg with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)

ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()


xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

# Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]


#Load data and combine data from 2001-2019 into totacts

# Source AccidentInput
source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data
acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2018
# with columns that are consistent for all of these years

# Get a common set the variables

comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

# the combined data frame
totacts <- combine.data(acts, comvar)


##Build a data frame with only extreme accidents for ACCDMG

dmgbox <-boxplot(totacts$ACCDMG)


xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]


##Data cleaning 

## Remove 9/11
xdmg <- xdmg[-186,]

##Remove duplicates
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", 
                                     "DAY", "TIMEHR", "TIMEMIN")])),]

## Reset rownames (observation #s) for sequential numbering- 
## otherwise they will remain the #s from totacts
rownames(xdmgnd) <- NULL



#Build linear regression models in R: lm


xdmgnd.lm1<-lm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1,data=xdmgnd)

xdmgnd.lm2<-lm(ACCDMG ~ TEMP + TRNSPD + CARS, data=xdmgnd)


##Display regression results for each model

summary(xdmgnd.lm1)



summary(xdmgnd.lm2)


##You should be able to find: estimated coefficients, residuals, t-test results, F test results, R^2, adjusted R^2,

names(xdmgnd.lm1)


##What are the coefficients of each of the linear models?

coef(xdmgnd.lm1)


##what is the sum of the residuals squared?

sum(xdmgnd.lm1$res^2)



##Metrics and Variable Selection      

##Criterion based assessments

##Adjusted R^2:

summary(xdmgnd.lm1)$adj.r.squared


##AIC:

AIC(xdmgnd.lm1)


##BIC:

AIC(xdmgnd.lm1,k=log(nrow(xdmgnd)))


##Variable Selection

#Stepwise Regression  

##If you have many predictors, it will take some time to get results. To save time, you can set 'trace=F' to get reults without showing each step:

xdmgnd.lm1.step<-step(xdmgnd.lm1, trace=F)



summary(xdmgnd.lm1.step)



#Partial F Test

##Recall that we can only compare two nested models by partial F test:

anova(xdmgnd.lm1,xdmgnd.lm2)
anova(xdmgnd.lm1,xdmgnd.lm1.step)



# Test Sets 

##Source TestSet.R

source("TestSet.R")


##set test sets size:

test.size<-1/3


##generate training sets and test sets from original data:

xdmgnd.data<-test.set(xdmgnd,test.size)


##Check distribution of ACCDMG of test set, training set:
library(ggplot2)
library(ggpubr)

#2 different plotting methods

##method 1
par(mfrow=c(2,2))
hist(xdmgnd.data$train$ACCDMG)
hist(xdmgnd.data$test$ACCDMG)
hist(xdmgnd$ACCDMG)
par(mfrow=c(1,1))

##method 2 with ggplot
a <- ggplot(as.data.frame(xdmgnd.data$train$ACCDMG), aes(xdmgnd.data$train$ACCDMG)) + geom_histogram()
b <- ggplot(as.data.frame(xdmgnd.data$test$ACCDMG), aes(xdmgnd.data$test$ACCDMG)) + geom_histogram()
c <- ggplot(as.data.frame(xdmgnd$ACCDMG), aes(xdmgnd$ACCDMG)) + geom_histogram()
ggarrange(a,b,c, ncol=2, nrow = 2)

##Build models with training set:
xdmgnd.lm1.train<-lm(ACCDMG~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd.data$train)

xdmgnd.lm2.train<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd.data$train)


##Recall that we need to measure predicted MSE. 
##First, how to predict with lm models:

xdmgnd.lm1.pred<-predict(xdmgnd.lm1.train,newdata=xdmgnd.data$test) 



xdmgnd.lm2.pred<-predict(xdmgnd.lm2.train,newdata=xdmgnd.data$test)


##Next, compute PMSE:

pmse.xdmgnd.lm1<-mse(xdmgnd.lm1.pred,xdmgnd.data$test$ACCDMG)

pmse.xdmgnd.lm1


pmse.xdmgnd.lm2<-mse(xdmgnd.lm2.pred,xdmgnd.data$test$ACCDMG)

pmse.xdmgnd.lm2


##Which model is better based on PMSE?



#Version 2 Test Sets- Run multiple iterations with different testing and training sets

##create vectors to store PMSE

pmse1.result<-NULL;
pmse2.result<-NULL;


for (i in c(1:20)){
  #set test sets size: 
  test.size<-1/3
  # generate training sets and test sets from original data:
  xdmgnd.data<-test.set(xdmgnd,test.size)
  
  # Build model with train set:
  lm1.train<-lm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1,data=xdmgnd.data$train)
  lm2.train<-lm(ACCDMG ~ TEMP + TRNSPD + CARS,data=xdmgnd.data$train)
  
  # First, how to predict with lm models:
  lm1.pred<-predict(lm1.train,newdata=xdmgnd.data$test) 
  lm2.pred<-predict(lm2.train,newdata=xdmgnd.data$test) 
  
  # Next, compute PMSE:
  pmse.lm1<-mse(lm1.pred,xdmgnd.data$test$ACCDMG)
  pmse.lm2<-mse(lm2.pred,xdmgnd.data$test$ACCDMG)
  
  # Add the PMSE for this run into your vector to stor PMSE
  pmse1.result<-c(pmse1.result,pmse.lm1)
  pmse2.result<-c(pmse2.result,pmse.lm2)
}


##Compare models based over 20 runs of PMSE

#Plot results method 1
plot(pmse1.result,type='b',col='blue',xlab="Index", ylab="PMSE")
lines(pmse2.result,type='b',col='red')
title(main="Model Comparison Based on PMSE")

#Plot results method 2 with ggplot
Index <- 1:length(pmse1.result);
df <- data.frame(Index,pmse1.result,pmse2.result)
ggplot(data=df, aes(x=Index)) +
  geom_line(aes(y = pmse1.result), color = 'blue', size = 1) +
  geom_line(aes(y = pmse2.result), color = 'red', linetype = 'twodash', size = 1) +
  ggtitle("Model Comparison Based on PMSE") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

##Which model is better from visual inspection of graph?



#We can also use statistical tests to compare our models.  


#Paired t test:

t.test(pmse1.result,pmse2.result,paired=T)


##Wilcoxon Test:


wilcox.test(pmse1.result,pmse2.result,paired=T)


##Which model performs better based on the paired t test and paired Wilcoxon test?



# Cross-Validation


##Need the boot library

library(boot)


##You need to use glm (a function to estimate generalized linear model) instead of lm. 
##Don't be confused by generalized linear models. Because lm is a special case of glm, glm 
##function can be used to estimate lm models as long as you set parameters correctly.


xdmgnd.lm1.cv<-glm(ACCDMG~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd)

xdmgnd.lm2.cv<-glm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)


##Cross-validation error

xdmgnd.lm1.err<-cv.glm(xdmgnd,xdmgnd.lm1.cv,K=10)



xdmgnd.lm1.err$delta



xdmgnd.lm2.err<-cv.glm(xdmgnd,xdmgnd.lm2.cv,K=10)



xdmgnd.lm2.err$delta


##There are two components for estimated errors: the first is the raw cross-validation estimate of prediction error; the second is the adjusted cross-validation estimate.

##Compare xdmgnd.lm2 and xdmgnd.lm3 based on adjusted cross-validation estimate.  Which model performs better?


# Diagnostics Plot      

##Generate diagnostics plot one by one

library(MASS)
library(lindia)

plot(xdmgnd.lm1,labels.id = NULL)

# Optional: All the diagnostic plots together with lindia package

gg_diagnose(xdmgnd.lm1)

#Plot graphs individually

plot(xdmgnd.lm1,which=1) #Residual vs. Fitted



plot(xdmgnd.lm1,which=2) #QQ



plot(xdmgnd.lm1,which=3) #Scale-Location



plot(xdmgnd.lm1,labels.id = NULL, which=4) #Cook's distance



plot(xdmgnd.lm1,which=5) #Redisuals vs. Leverage



plot(xdmgnd.lm1,which=6) #Cook's dist vs. Leverage


#Take a closer look at observations with a high Cook's D


#Now use ggplot

#Residual vs. Fitted

library(ggfortify)

autoplot(xdmgnd.lm1, which=1)

#QQ

autoplot(xdmgnd.lm1, which=2)

#Scale-Location

autoplot(xdmgnd.lm1, which=3)

#Cook's distance

autoplot(xdmgnd.lm1, which=4)

#Residual vs Leverage Plot

autoplot(xdmgnd.lm1, which=5)


# Cook's dist vs Leverage

autoplot(xdmgnd.lm1, which=6)


#Plot all four plots together

par(mfrow=c(2,2))
plot(xdmgnd.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 

#With ggplot
autoplot(xdmgnd.lm1)
autoplot(xdmgnd.lm1, which=1:6)

#Save the above plot as png:

#Save the plot to a file
png('diagnostics.png')

par(mfrow=c(2,2))
plot(xdmgnd.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 
dev.off()


#What happened in each of the accidents noted on the Cook's Distance plot?




##What do you observe in each diagnostic plot?  Discuss your observations and any issues.

##a. residuals vs. fitted

##b. qq-plot

##c. Scale-Location?

##d. Cook's distance / Residuals vs. Leverage?


#Transformations

##Let's take a look at the response variable ACCDMG.  

plot(density(xdmgnd$ACCDMG))

ggplot(as.data.frame(xdmgnd$ACCDMG), aes(xdmgnd$ACCDMG)) + geom_density()

##Do we violate the distributional assumption of our response variable?


#Box-Cox Transformation          

boxcox(xdmgnd.lm1) #box-cox plot

gg_boxcox(xdmgnd.lm1)


boxcox(xdmgnd.lm1, plotit=T, lambda=seq(-2,2,by=0.5))


##get x and y values without plotting

boxcox(xdmgnd.lm1,plotit=F)


##find y value for maximum lambda

max(boxcox(xdmgnd.lm1, plotit = F)$y)


## find best lamda value

boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 


##The best lambda and store in L

L<-boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 


##The model with the best lamda transformation

xdmgnd.lm1.boxcox<-lm(ACCDMG^L~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd)


##Display regression results for boxcox model

summary(xdmgnd.lm1.boxcox)


##Let's replot our density function for our response variable ACCDMG

plot(density((xdmgnd$ACCDMG^L)))

ggplot(as.data.frame(xdmgnd$ACCDMG^L), aes(xdmgnd$ACCDMG^L)) + geom_density()

##Plot diagnostic for your new model xdmgnd.lm1.boxcox


##What do you observe in the diagnostic plots for your new model xdmgnd.lm1.boxcox?  Did the transformation help?


## Now, let's try a logarithm transform of the response variable ACCDMG.

plot((density(log(xdmgnd$ACCDMG))))

ggplot(as.data.frame(log(xdmgnd$ACCDMG)), aes(log(xdmgnd$ACCDMG))) + geom_density()

## Build a new model with log(ACCDMG) as the response



## Plot diagnostics for your new model xdmgnd.lm1.log



## What do you observe in the diagnostic plots for your new model xdmgnd.lm1.log?  Which trasformation do you choose?

