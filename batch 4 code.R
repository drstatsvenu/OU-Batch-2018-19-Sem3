getwd()
setwd("D:/project 2k18/final")
#readind the data
data=read.csv("Final data.csv")
#Viewing the data
View(data)
#checking the dimension of data
dim(data)
#checking for the variables names in the data
names(data)
#viewing the head , tail of the data
head(data)
tail(data)
#to know the structure of the data
str(data)
#changing the datatypes into factors
data$rank=as.factor(data$rank)
data$sex=as.factor(data$sex)

str(data)
#to get descrptive statistics
summary(data)

#checking for missing values
table(is.na(data))

colSums(is.na(data))

# Checking for outliers in both Dependent and Independent variable

summary(data)
boxplot(data$salary)
boxplot(data$yrs.since.phd)
boxplot(data$yrs.service)
data$salary[data$salary>quantile(data$salary, 0.95)] <- quantile(data$salary, 0.95)
boxplot(data$salary)
data$yrs.since.phd[data$yrs.since.phd>quantile(data$yrs.since.phd, 0.95)] <- quantile(data$yrs.since.phd, 0.95)
boxplot(data$yrs.since.phd)
data$yrs.service[data$yrs.service>quantile(data$yrs.service, 0.95)] <- quantile(data$yrs.service, 0.95)
boxplot(data$yrs.service)
####################
pre1=data[c(3,4,6)]
str(pre1)
library(corrplot)
pre1.cor	=	cor(pre1)
print(pre1.cor)
corrplot(pre1.cor,	method="circle")
#####################################33
# Continuous vs categories
library(caret)
x <- data[,6]
y <- data[,1]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- data[,6]
y <- data[,2]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- data[,6]
y <- data[,5]
featurePlot(x=x, y=y, plot="box")
  
#anova
x1=aov(data$salary ~ data$rank)
summary(x1)
x2=aov(data$salary  ~ data$sex)
summary(x2)
x3=aov(data$salary ~ data$discipline)
summary(x3)
######################################3
#statistical testss/ Filter Method
######################################3
# Preparing data for Traing and tessting
###########################3
train_rows<- sample(1:nrow(data), size=0.7*nrow(data))
train_rows
training <- data[train_rows, ]
test <- data[-train_rows, ]
dim(data)
dim(training)
dim(test)
names(training)
names(test)
###########################3
# Evaluate Algorithms: Baseline
###########################3
# Run algorithms using 10-fold cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
# lm
set.seed(100)
fit.lm <- train(salary~., data=training, method="lm", metric=metric, preProc=c("range"), trControl=control)
print(fit.lm)
# GLM
set.seed(100)
fit.glm <- train(salary~., data=training, method="glm", metric=metric, preProc=c("range"), trControl=control)
print(fit.glm)
# CART
set.seed(100)
grid <- expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart <- train(salary~., data=training, method="rpart", metric=metric, tuneGrid=grid, preProc=c("range"), trControl=control)
print(fit.cart)
# kNN
set.seed(100)
fit.knn <- train(salary~., data=training, method="knn", metric=metric, preProc=c("range"), trControl=control)
print(fit.knn)
# SVM
set.seed(100)
fit.svm <- train(salary~., data=training, method="svmRadial", metric=metric, preProc=c("range"), trControl=control)
print(fit.svm)
# RF
set.seed(100)
fit.rf <- train(salary~., data=training, method="rf", metric=metric, preProc=c("range"), trControl=control)
print(fit.rf)
results <- resamples(list(LM=fit.lm, GLM=fit.glm,CART=fit.cart, KNN=fit.knn, SVM=fit.svm,RF=fit.rf))
summary(results)
dotplot(results)
#####################################
library(randomForest)
set.seed(100)
fit.rffin <- randomForest(salary~., data=training, mtry=2,tree=500,
                          importance=TRUE, na.action=na.omit)
print(fit.rffin)
## Show "importance" of variables: higher value mean more important:
round(importance(fit.rffin), 2)

######################
# GLMFianl model with key variables
######################
set.seed(100)
fit.glm <- train(salary~ rank + discipline + yrs.since.phd  , data=training, method="glm", metric=metric, preProc=c("range"), trControl=control)
print(fit.glm)
pred=predict(fit.glm,data=training)
xx=cbind(training$salary,pred)
write.csv(xx, 'trainpred.csv')
######################

names(test)
test1=test[,c(1:4)]
str(test1)
predtest=predict(fit.glm,newdata=test)
zz=cbind(predtest,test$salary)
write.csv(zz, 'testpred.csv')
######################
