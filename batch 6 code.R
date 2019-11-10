# Checking and setting the path
getwd()
setwd("C:/Users/mghnp/Desktop/RProject")
getwd()

# Loading required packages
install.packages("mice")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("glmnet")
install.packages("Hmisc")
install.packages("e1071")
install.packages("caret")
library(mice)
library(randomForest)
library(ggplot2)
library(glmnet)
library(Hmisc)
library(e1071)
library(caret)

# Reading the data file into R
# File name: Mushroom
M1=read.csv("Mushroom.csv")

# Looking at basic framework of the dataset - observations, attributes, datatypes
dim(M1)
class(M1)
names(M1)
str(M1)
cols=c("y","x1","x2", "x3","x4","x5", "x6","x7","x8","x9", "x10", "x11", "x12", "x13", "x14", "x15","x16", "x17", "x18", "x19", "x20", "x21", "x22")
M1[cols]=lapply(M1[cols], factor)
str(M1)
summary(M1)
View(M1)

# Removing x16 variable as it has only one level and irrelevant for data analysis
M2=M1[,c(1:16,18:23)]
names(M2)
summary(M2)

#Checking for missing value
print(all(!is.na(M2)))

#Missing value Proportion for all the variables
sapply(M2, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})

# Missing values impute for small proportion of missing values
library(Hmisc)
M2$x11= impute(M2$x11, mode)


sapply(M2, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})

# Numerical relationships between variables 
prop.table(table(M2$x1,M2$y),2)
prop.table(table(M2$x1,M2$y),1)
prop.table(table(M2$x2,M2$y),2)
prop.table(table(M2$x2,M2$y),1)
prop.table(table(M2$x3,M2$y),2)
prop.table(table(M2$x3,M2$y),1)
prop.table(table(M2$x4,M2$y),2)
prop.table(table(M2$x4,M2$y),1)
prop.table(table(M2$x5,M2$y),2)
prop.table(table(M2$x5,M2$y),1)
prop.table(table(M2$x6,M2$y),2)
prop.table(table(M2$x6,M2$y),1)
prop.table(table(M2$x7,M2$y),2)
prop.table(table(M2$x7,M2$y),1)
prop.table(table(M2$x8,M2$y),2)
prop.table(table(M2$x8,M2$y),1)
prop.table(table(M2$x9,M2$y),2)
prop.table(table(M2$x9,M2$y),1)
prop.table(table(M2$x10,M2$y),2)
prop.table(table(M2$x10,M2$y),1)
prop.table(table(M2$x11,M2$y),2)
prop.table(table(M2$x11,M2$y),1)
prop.table(table(M2$x12,M2$y),2)
prop.table(table(M2$x12,M2$y),1)
prop.table(table(M2$x13,M2$y),2)
prop.table(table(M2$x13,M2$y),1)
prop.table(table(M2$x14,M2$y),2)
prop.table(table(M2$x14,M2$y),1)
prop.table(table(M2$x15,M2$y),2)
prop.table(table(M2$x15,M2$y),1)
prop.table(table(M2$x17,M2$y),2)
prop.table(table(M2$x17,M2$y),1)
prop.table(table(M2$x18,M2$y),2)
prop.table(table(M2$x18,M2$y),1)
prop.table(table(M2$x19,M2$y),2)
prop.table(table(M2$x19,M2$y),1)
prop.table(table(M2$x20,M2$y),2)
prop.table(table(M2$x20,M2$y),1)
prop.table(table(M2$x21,M2$y),2)
prop.table(table(M2$x21,M2$y),1)
prop.table(table(M2$x22,M2$y),2)
prop.table(table(M2$x22,M2$y),1)

# Visualization
# Visual relationships between variables
pairs(M2)

#Dividing dataset into smaller dataset for clear picturing
M_a=M2[,c(1,2:6)]
M_b=M2[,c(1,7:11)]
M_c=M2[,c(1,12:16)]
M_d=M2[,c(1,17:22)]
pairs(M_a)
pairs(M_b)
pairs(M_c)
pairs(M_d)

# Finding if variables are significant by performing chi-squared test
chisq.test(M2$y,M2$x1)
chisq.test(M2$y,M2$x2)
chisq.test(M2$y,M2$x3)
chisq.test(M2$y,M2$x4)
chisq.test(M2$y,M2$x5)
chisq.test(M2$y,M2$x6)
chisq.test(M2$y,M2$x7)
chisq.test(M2$y,M2$x8)
chisq.test(M2$y,M2$x9)
chisq.test(M2$y,M2$x10)
chisq.test(M2$y,M2$x11)
chisq.test(M2$y,M2$x12)
chisq.test(M2$y,M2$x13)
chisq.test(M2$y,M2$x14)
chisq.test(M2$y,M2$x15)
chisq.test(M2$y,M2$x17)
chisq.test(M2$y,M2$x18)
chisq.test(M2$y,M2$x19)
chisq.test(M2$y,M2$x20)
chisq.test(M2$y,M2$x21)
chisq.test(M2$y,M2$x22)

# Splitting dataset as Train and Test data
Train_Rows=sample(1:nrow(M2), size=0.8*nrow(M2))
Train_Rows
Train=M2[Train_Rows, ]
Test=M2[-Train_Rows, ]
dim(M2)
dim(Train)
dim(Test)

# Model Pipeline
# Run algorithms using 5-fold cross validation
library(caret)
control = trainControl(method="repeatedcv", number=5, repeats=3)

# GLM
set.seed(9)
fit.glm = train(y~., data=Train, method="glm", metric="Accuracy", trControl=control)
print(fit.glm)

# CART
set.seed(9)
grid = expand.grid(.cp=c(0.01,0.05,0.1))
fit.cart = train(y~., data=Train, method="rpart", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.cart)

# SVM
set.seed(9)
grid = expand.grid(.sigma=c(0.01,0.05,0.1), .C=c(1))
fit.svm = train(y~., data=Train, method="svmRadial", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.svm)


# kNN
set.seed(9)
grid = expand.grid(.k=c(1,3,5,7))
fit.knn = train(y~., data=Train, method="knn", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.knn)

#Random Forest
fit.rf = train(y~., data=Train, method="rf", metric="Accuracy", trControl=control)
print(fit.rf)

# Comparing algorithms
M3=resamples(list(SVM=fit.svm, CART=fit.cart, kNN=fit.knn, glm=fit.glm, RF=fit.rf))
summary(M3)
dotplot(M3)

#Tuning Random Forest
customRF = list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters = data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid = function(x, y, len = NULL, search = "grid") {}
customRF$fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict = function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort = function(x) x[order(x[,1]),]
customRF$levels = function(x) x$classes

# Choosing desired parameter values
library(caret)
library(randomForest)
control = trainControl(method="repeatedcv", number=5, repeats=3)
tunegrid = expand.grid(.mtry=c(1:6), .ntree=c(100, 200, 300))
set.seed(9)
M4= train(y~., data=Train,method=customRF, tuneGrid=tunegrid, trControl=control)
print(M4)

# Feature Selection
rf_model3 = randomForest(y ~ ., data =Train, ntree=100, mtry=2)
varImpPlot(rf_model3, sort = T, n.var = 10, main = "Top 10 - Variable Importance")
importance(rf_model3)

#Running final model using Logistic Regression
dim(Train)
trainControl = trainControl(method="repeatedcv", number=5, repeats=3)
final.glm =train (y~ x5 + x11 + x22 + x8, data=Train, method="glm", trControl=trainControl)
summary(final.glm)
print (final.glm)

# Finding accuracy of Logistic regression on Train data
predslog = predict(final.glm, data=Train, type="raw")
tabtrain = table(Predicted = predslog, Actual = Train$y)
caret::confusionMatrix(predslog,Train$y)

# Running and finding accuracy of the final model on Test data
dim(Test)
names(Test)
M5 = predict(final.glm,newdata=Test,type="raw")
tabTest = table(Predicted = M5, Actual = Test$y)
caret::confusionMatrix(M5,Test$y)

