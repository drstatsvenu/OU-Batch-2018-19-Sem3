
getwd()
setwd("E:/")

#reading the data

german=read.csv("german credit card.csv")

#viewing the head,tail of the data

head(german)
tail(german) 

#viewing the data

View(german)

#to get descriptive statistics

summary(german)

#checking for missing values

is.na(german$x1)
is.na(german$lc)

#checking the percentage of missing values in each column (variable)

sum(is.na(german))
sapply(german, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})

#checking for outliers

boxplot(german$x14)
boxplot(german$x15)
boxplot(german$x16)
boxplot(german$x17)
boxplot(german$x18)
boxplot(german$x19)
boxplot(german$x20)

#removing the outliers

german$x14[german$x14>quantile(german$x14,0.95)] <-quantile(german$x14,0.95)
german$x15[german$x15>quantile(german$x15,0.90)] <-quantile(german$x15,0.90)
german$x16[german$x16>quantile(german$x16,0.95)] <-quantile(german$x16,0.95)
german$x17[german$x17>quantile(german$x17,0.95)] <-quantile(german$x17,0.95)
german$x18[german$x18>quantile(german$x18,0.95)] <-quantile(german$x18,0.95)
german$x19[german$x19>quantile(german$x19,0.95)] <-quantile(german$x19,0.95)
german$x20[german$x20>quantile(german$x20,0.80)] <-quantile(german$x20,0.80)


#converting categorical variable to factor

german$x1=as.factor(german$x1)
german$x2=as.factor(german$x2)
german$x3=as.factor(german$x3)
german$x4=as.factor(german$x4)
german$x5=as.factor(german$x5)
german$x6=as.factor(german$x6)
german$x7=as.factor(german$x7)
german$x8=as.factor(german$x8)
german$x9=as.factor(german$x9)
german$x10=as.factor(german$x10)
german$x11=as.factor(german$x11)
german$x12=as.factor(german$x12)
german$x13=as.factor(german$x13)
german$dep=as.factor(german$dep)
str(german)

mymodel <- glm(dep ~ ., data = train, family = 'binomial')
summary(mymodel)
#changing to numeric all the imputed variables

pre1=german[c(14:20)]
library(corrplot)
str(pre1)
pre1.cor  = cor(pre1)
corrplot(pre1.cor, method="circle")

#continuous vs categories

library(caret)
x <- german[,14]
str(german)
y <- german[,"dep"]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- german[,15]
str(german)
y <- german[,"dep"]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- german[,16]
str(german)
y <- german[,"dep"]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- german[,17]
str(german)
y <- german[,"dep"]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- german[,18]
str(german)
y <- german[,"dep"]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- german[,19]
str(german)
y <- german[,"dep"]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- german[,20]
str(german)
y <- german[,"dep"]
featurePlot(x=x, y=y, plot="box")


str(german)

#testing the significance diffrence using anova

a=aov(german$x14 ~ german$dep)
summary(a)
a=aov(german$x15 ~ german$dep)
summary(a)
a=aov(german$x16 ~ german$dep)
summary(a)
a=aov(german$x17 ~ german$dep)
summary(a)
a=aov(german$x18 ~ german$dep)
summary(a)
a=aov(german$x19 ~ german$dep)
summary(a)
a=aov(german$x20 ~ german$dep)
summary(a)

# Preparing data for training and testing - train (80%) & test (20%)

set.seed(1234)
ind <- sample(2, nrow(german), replace = T, prob = c(0.7, 0.3))
train <- german[ind==1,]
test <- german[ind==2,]
View(train)
dim(train)

table(train$dep)

dim(test)
table(test$dep)

#evaluate algorithms:baseline

#Fitting Logistic  Regression
mymodel <- glm(dep ~ ., data = train, family = 'binomial')
summary(mymodel)
# Prediction

p1 <- predict(mymodel, train, type = 'response')
pred1 <- ifelse(p1>0.5, 1, 0)
pred1<-as.factor(pred1)
table(train$dep)
str(pred1)
library(caret)
confusionMatrix( pred1,train$dep)

###Testing

p2 <- predict(mymodel, test, type = 'response')
predtest <- ifelse(p2>0.5, 1, 0)
predtest<-as.factor(predtest)
library(caret)
confusionMatrix(predtest,test$dep)

# Identifying key variables

# Final model

library(randomForest)
rf_model <- randomForest(dep ~ ., data =train)
print(rf_model)


varImpPlot(rf_model,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

#Final model with Shortlisted variabvles

ite1 <- glm(dep~x1+x2+x3+x4+x5+x7+x13+x14+x15+x18, data =  train , family = 'binomial')
summary(ite1)
p2 <- predict(ite1, train, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
str(pred2)
pred2<-as.factor(pred2)
library(caret)
confusionMatrix( pred2,train$dep)

# Testing on test data with final Model

pred=predict(ite1,newdata=test,type = 'response')
p3 <- ifelse(pred>0.5, 1, 0)
p3<-as.factor(p3)
library(caret)
confusionMatrix(p3,test$dep)
#####################################e1####3



