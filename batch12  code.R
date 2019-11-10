# Understanding the Business Problem - Predicting Mpg gto the given variables( as being mulitple independent variable are there hence  we are using multiple linear regression)
getwd()
setwd("D:/Batch12")
getwd()

##################################################


# Reading data
mtcars=read.csv('mtcars.csv')
dim(mtcars)

##################################################
# structure of data file
##################################################

str(mtcars)
mtcars$vs=as.factor(mtcars$vs)
mtcars$am=as.factor(mtcars$am)
str(mtcars)
head(mtcars)

##################################################
#Undersytanding the data with descrptive statistics
##################################################

summary(mtcars)
sd(mtcars$mpg)
sd(mtcars$hp)

##################################################
# to check the Missing values
##################################################

is.null(mtcars$mpg)

sapply(mtcars, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})

##################################################
# to check the outlier we wil use box plot
##################################################
str(mtcars)
boxplot(mtcars$mpg)
boxplot(mtcars$disp)
boxplot(mtcars$hp)
boxplot(mtcars$drat)
boxplot(mtcars$wt)
boxplot(mtcars$qsec)

mtcars$hp[mtcars$hp>quantile(mtcars$hp, 0.95)] <- quantile(mtcars$hp, 0.95)
mtcars$qsec[mtcars$qsec>quantile(mtcars$qsec, 0.95)] <- quantile(mtcars$qsec, 0.95)
mtcars$wt[mtcars$wt>quantile(mtcars$wt, 0.95)] <- quantile(mtcars$wt, 0.95)

boxplot(mtcars$hp)
boxplot(mtcars$qsec)

pairs(mtcars)

# Subset data to features we wish to keep/use.
features <- c("mpg", "disp", "hp", "drat", "wt","qsec")
data1=mtcars[features]
str(data1)
install.packages("corrplot")
library(corrplot)
pre1.cor	=	cor(data1)
pre1.cor
corrplot(pre1.cor,	method="circle")
##################################################
library(caret)
featurePlot(x=mtcars$mpg, y=mtcars$am, plot="box")
featurePlot(x=mtcars$mpg, y=mtcars$vs, plot="box")
##################################################
##################333
#Hypithesis testing
##################333

str(mtcars)
x1=aov(mtcars$mpg ~ mtcars$vs)
summary(x1)
x2=aov(mtcars$mpg ~ mtcars$am)
summary(x2)
##################
mymodel=lm(mpg~.,data=mtcars)
summary(mymodel)
# stepwise modle
best_model <- step(mymodel, direction = "both")
summary(best_model)

pred=predict(best_model,data=mtcars)
xx=cbind(mtcars$mpg,pred)
write.csv(xx,"predicted.csv")
######################################33
# Predicting the furture
#####################################3
test=read.csv('test.csv')
test
test$am=as.factor(test$am)
dim(test)
newpred=predict(best_model, newdata = test)

######################################
# Submitting the results
######################################33
yy=cbind(test,newpred)
write.csv(yy,"predicted.csv")








