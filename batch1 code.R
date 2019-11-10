getwd()
setwd("D:/final project")
#readind the data
data=read.csv("input.csv")
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
data$Year=as.factor(data$Year)
data$Genre=as.factor(data$Genre)
data$Sequel=as.factor(data$Sequel)
str(data)
#to get descrptive statistics
summary(data)

#checking for missing values
table(is.na(data))

colSums(is.na(data))

#checking the percentage of missing values in each column(variable)
sapply(data, function(df)
{ 
  (sum(is.na(df)==TRUE)/ length(df))*100; 
}  ) 

#checking for the packages
require(Amelia) 
library(Amelia)

#installing packages
#install.packages("Amelia")
library(Amelia)                
#checking for the missing values by visualization
missmap(data, main="Missing Map")
#installing package
install.packages("Hmisc")
library(Hmisc)
data$AF<- impute(data$AF, mean) 
data$Screens<- impute(data$Screens, mean) 
data$Budget<- impute(data$Budget, mean) 
missmap(data, main="Missing Map")
dim(data)
str(data)
#changing to numeric all the imputed variables
data$Budget=as.numeric(data$Budget)
data$AF=as.numeric(data$AF)
data$Screens=as.numeric(data$Screens)
str(data)
#######################
pre1=data[c(2,4,5,6,8,9:12)]
str(pre1)
library(corrplot)
pre1.cor	=	cor(pre1)
corrplot(pre1.cor,	method="circle")
#####################################33
# Continuous vs categories
library(caret)
x <- data[,4]
y <- data[,1]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- data[,4]
y <- data[,3]
featurePlot(x=x, y=y, plot="box")

library(caret)
x <- data[,4]
y <- data[,7]
featurePlot(x=x, y=y, plot="box")
  
#####################################33
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
set.seed(7)
fit.lm <- train(Gross~., data=training, method="lm", metric=metric, preProc=c("center", "scale"), trControl=control)
print(fit.lm)
# GLM
set.seed(7)
fit.glm <- train(Gross~., data=training, method="glm", metric=metric, preProc=c("center", "scale"), trControl=control)
print(fit.glm)

# CART
set.seed(7)
grid <- expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart <- train(Gross~., data=training, method="rpart", metric=metric, tuneGrid=grid, preProc=c("center", "scale"), trControl=control)
print(fit.cart)
# kNN
set.seed(7)
fit.knn <- train(Gross~., data=training, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
print(fit.knn)
# SVM
set.seed(7)

fit.svm <- train(Gross~., data=training, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control)
print(fit.svm)
# RF
set.seed(7)
fit.rf <- train(Gross~., data=training, method="rf", metric=metric, preProc=c("center", "scale"), trControl=control)
print(fit.rf)
# Compare algorithms
results <- resamples(list(LM=fit.lm, GLM=fit.glm,CART=fit.cart, KNN=fit.knn, SVM=fit.svm,RF=fit.rf))
summary(results)
dotplot(results)
###########################3
#Random Forest
###########################3
library("randomForest")
## data(airquality)
set.seed(7)
fit.rffin <- randomForest(Gross ~ ., data=training, mtry=13,tree=500,
                         importance=TRUE, na.action=na.omit)

print(fit.rffin)

## Show "importance" of variables: higher value mean more important:
round(importance(fit.rffin), 2)

pred=predict(fit.rffin,data=training)
xx=cbind(training$Gross,pred)
write.csv(xx, 'trainpred.csv')
###############
#Test data
###############
predtest=predict(fit.rffin,newdata=test)
yy=cbind(test$Gross,predtest)
write.csv(yy, 'testpred.csv')
##############################
# Fitting linear regression with selected variables
##############################
mymodel=lm(Gross ~ Screens+Budget+Ratings+Sequel+Genre,data=training)
summary(mymodel)
names(test)
test1=test[,c(2,3,5,6,7)]
str(test1)
predtest=predict(mymodel,newdata=test1)
zz=cbind(predtest,test$Gross)
write.csv(zz,'predtest.csv')





