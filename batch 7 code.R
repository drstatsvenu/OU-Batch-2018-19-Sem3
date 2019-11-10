# Setting working directory
getwd()
setwd("D:/batch7 user knowledge")
getwd()

# Laoding required libraries to perform modeling
install.packages("mice")
library(mice)
install.packages("randomForest")
library(randomForest)
install.packages("ggplot2")
library(ggplot2)
install.packages("glmnet")
library(glmnet)

#######################################################################
# reading data from Folder and checking for the data types
dataset <- read.csv(file.choose(), header = T)
str(dataset)


dataset$UNS=as.factor(dataset$UNS)
str(dataset)
dim(dataset)
tail(dataset)
head(dataset)
summary(dataset)
#######################################################################
#Checking for Missing value
print(all(!is.na(dataset)))


#Missing value  Proportion for all the variables
sapply(dataset, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;})

#######################################################################
boxplot(dataset$STG)
boxplot(dataset$LPR)
boxplot(dataset$PEG)

dataset$STG[dataset$STG>quantile(dataset$STG, 0.95)] <- quantile(dataset$STG, 0.95)
boxplot(dataset$STG)

pairs(dataset)
dim(dataset)

pre1=dataset[c(1:5)]
install.packages("corrplot")
library(corrplot)
pre1.cor	=	cor(pre1)
pre1.cor
corrplot(pre1.cor,	method="circle")

# Continuous vs categories
install.packages("caret")
library(caret)
install.packages("ggplot2")
library(ggplot2)
x <- dataset[,1:5]
y <- dataset[,6]
featurePlot(x=x, y=y, plot="box")

str(dataset)

#####
#anova
x1=aov(dataset$STG ~ dataset$UNS)
summary(x1)
x2=aov(dataset$SCG ~ dataset$UNS)
summary(x2)
x3=aov(dataset$STR~ dataset$UNS)
summary(x3)
x4=aov(dataset$LPR~ dataset$UNS)
summary(x4)
x5=aov(dataset$PEG~ dataset$UNS)
summary(x5)
#######################################################################
# Splting data 
#######################################################################

train_rows<- sample(1:nrow(dataset), size=0.7*nrow(dataset))
train_rows
training <- dataset[train_rows, ]
test <- dataset[-train_rows, ]
dim(dataset)
dim(training)
dim(test)
head(test)

#######################################################################
# Model Pipleline
#######################################################################

# Run algorithms using 5-fold cross validation
control <- trainControl(method="repeatedcv", number=5, repeats=3)
# GLM
set.seed(7)
fit.glm <- train(UNS~., data=training, method="glm", metric="Accuracy", trControl=control)
print(fit.glm)

# CART
set.seed(7)

grid <- expand.grid(.cp=c(0.01,0.05,0.1))
fit.cart <- train(UNS~., data=training, method="rpart", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.cart)

# SVM
set.seed(7)
grid <- expand.grid(.sigma=c(0.01,0.05,0.1), .C=c(1))
fit.svm <- train(UNS~., data=training, method="svmRadial", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.svm)


# kNN
set.seed(7)
grid <- expand.grid(.k=c(1,3,5,7))
fit.knn <- train(UNS~., data=training, method="knn", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.knn)

#######RF
fit.rf <- train(UNS~., data=training, method="rf", metric="Accuracy", trControl=control)
print(fit.rf)

# Compare algorithms
results <- resamples(list(SVM=fit.svm, CART=fit.cart, kNN=fit.knn, glm=fit.glm, RF=fit.rf))
summary(results)
dotplot(results)
###################################

#######################################3
#Tunning Random Forest
#################################################################
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

#################
install.packages("caret")
library(caret)
library(randomForest)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:6), .ntree=c(100, 200, 300))
set.seed(100)
custom <- train(UNS~., data=training,method=customRF, tuneGrid=tunegrid, trControl=control)
print(custom)

#########################################################################3
rf_model3 <- randomForest(UNS ~ ., data =training, ntree=200, mtry=2)

varImpPlot(rf_model3,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf_model3)

##############################################
#$Final model With Logistic Regression
##############################################

dim(training)
dim(test)

trainControl <- trainControl(method="repeatedcv", number=5, repeats=3)


final.glm <-train (UNS ~ PEG+LPR+STG, data=training, method="glm",
                   trControl=trainControl)



summary(final.glm)
print (final.glm)


predslog <- predict(final.glm, data=training, type = "raw")
tabtrain <- table(Predicted = predslog, Actual = training$UNS )
caret::confusionMatrix(predslog,training$UNS)
##############################################

#On testing
##############################################
dim(test)
names(test)


p2 <- predict(final.glm,newdata=test,type="raw")

tabtest <- table(Predicted = p2, Actual = test$UNS)

caret::confusionMatrix(p2,test$UNS)
##############################################



