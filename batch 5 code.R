R code
getwd()
#setting working directory
setwd("D:/r project on si events/project")
getwd()
#reading the data
sports<-read.csv('primary data.csv',stringsAsFactors = T, na.strings = c(""," ","NA","?",NA))
#Viewing the data
View(sports)
#viewing the head, tail of the data
head(sports,10)
tail(sports,10)
#checking the dimension of data
dim(sports)
#checking for the variables names in the data
names(sports)
#To know the structure of the data
str(sports)
#changing the data types into factors
sports$Qualify_PhysicalTest<-as.factor(sports$Qualify_PhysicalTest)
str(sports)
#To get descriptive statistics
summary(sports)
#Checking for missing values
sum(is.na(sports))
colSums(is.na(data))
#Checking the percentage of missing values in each column (variable)
sapply(sports, function(df) {(sum(is.na(df)==TRUE)/ length(df))*100; })
#Understanding data using Descrptive
pairs(sports)
#Checking for outliers
boxplot(sports$BMI )
boxplot(sports$calories)
boxplot(sports$Protein)
boxplot(sports$practice)
boxplot(sports$workout)
# Removing outliers
sports$calories[sports$calories>quantile(sports$calories, 0.95)] <- quantile(sports$calories, 0.95)
sports$Protein[sports$Protein>quantile(sports$Protein, 0.80)] <- quantile(sports$Protein, 0.80)
sports$practice[sports$practice>quantile(sports$practice, 0.90)] <- quantile(sports$practice, 0.90)
sports$workout[sports$workout>quantile(sports$workout, 0.80)] <- quantile(sports$workout, 0.80)
sports$workout[sports$workout<quantile(sports$workout, 0.05)] <- quantile(sports$workout, 0.05)


boxplot(sports$calories)
boxplot(sports$Protein)
boxplot(sports$practice)
boxplot(sports$workout)
# Continuous vs Continuous 
#Correlation
pre1=sports[c(2:6)]
#installing packages
install.packages("corrplot")
library(corrplot)
pre1.cor	=	cor(pre1)
corrplot(pre1.cor,	method="circle")
# Continuous vs categories
#Feature plot
library(caret)
x <- pre1[,2]
str(sports)
y <- sports[,7]
featurePlot(x=x, y=y, plot="box")
library(caret)
x <- pre1[,3]
str(sports)
y <- sports[,7]
featurePlot(x=x, y=y, plot="box")
library(caret)
x <- pre1[,4]
str(sports)
y <- sports[,7]
featurePlot(x=x, y=y, plot="box")
library(caret)
x <- pre1[,5]
str(sports)
y <- sports[,7]
featurePlot(x=x, y=y, plot="box")
library(caret)
x <- pre1[,6]
str(sports)
y <- sports[,7]
featurePlot(x=x, y=y, plot="box")


#Statistical testss/ Filter Method
#ANOVA
x1=aov(sports$BMI ~ sports$Qualify_PhysicalTest)
summary(x1)
x2=aov(sports$calories ~ sports$Qualify_PhysicalTest)
summary(x2)
x3=aov(sports$Protine ~ sports$Qualify_PhysicalTest)
summary(x3)
x4=aov(sports$practice~ sports$Qualify_PhysicalTest)
summary(x4)
x5=aov(sports$workout~ sports$Qualify_PhysicalTest)
summary(x5)
##choosing continuous variables
str(sports)
cont<-subset(sports,select = -c(Qualify_PhysicalTest))
str(cont)
#standardizing continuous variables
scale_training <- as.data.frame(scale(cont[,],  
                                      center = TRUE, scale = TRUE))
str(scale_training)
scale_training<-cbind(scale_training, sports$Qualify_PhysicalTest)
str(scale_training)
names(scale_training)
write.csv(scale_training,'primary data.csv')
scaledata=read.csv('primary data.csv')
str(scaledata)
scaledata$Qualify_PhysicalTest=as.factor(scaledata$Qualify_PhysicalTest)
str(scaledata)
# Preparing data for Training and testing
train_rows<-sample(1:nrow(scaledata), size=0.7*nrow(scaledata))
train_rows
training <- scaledata[train_rows, ]
test <- scaledata[-train_rows, ]
dim(scaledata)
dim(training)
dim(test)
names(training)
names(test)
# Evaluate Algorithms: Baseline
# Run algorithms using 10-fold cross validation
control<-trainControl(method="repeatedcv", number=10,repeats = 3)
seed <- 7
metric <- "Accuracy"
print(trainControl)
# General linear model
set.seed(seed)
fit.glm <- train(Qualify_PhysicalTest~., data=training, method="glm", metric=metric, trControl=control)
print(fit.glm)

# CART
set.seed(seed)
fit.cart <- train(Qualify_PhysicalTest~.,data=training, method="rpart", metric=metric, trControl=control)
print(fit.cart)
# kNN
set.seed(seed)
fit.knn <- train(Qualify_PhysicalTest~.,data=training, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
print(fit.knn)
# SVM
set.seed(seed)
fit.svm <- train(Qualify_PhysicalTest~.,data=training, metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
print(fit.svm)

# Random Forest
set.seed(seed)
fit.rf <- train(Qualify_PhysicalTest~.,data=training, method="rf", metric=metric, trControl=control)
print(fit.rf)
# Compare algorithms
results <- resamples(list(logistic=fit.glm,svm=fit.svm, knn=fit.knn, DT=fit.cart,rf=fit.rf ))
# Table comparison
summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)
# Checking Random Forest
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
#Random Forest for checking  variable importance
library(caret)
library(randomForest)
control <- trainControl(method="repeatedcv", number = 10 , repeats=3)
tunegrid <- expand.grid(.mtry=c(1:5),ntree=c(100,200,500))
set.seed(7)
custom <- train(Qualify_PhysicalTest~.,data = training , method=customRF, tuneGrid=tunegrid, trControl=control)
print(custom)
#variable importance
library('randomForest')
control <- trainControl(method="repeatedcv", number=5)
rf_model <- randomForest(Qualify_PhysicalTest~.,data = training,ntree=500,mtry=4,trControl=control)
varImpPlot(rf_model,sort = T,  n.var = 5,main = "Variable Importance")
importance(rf_model)
# Fitting logistic regression with selected variables
set.seed(100)
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
final.glm <-train (Qualify_PhysicalTest ~  Protein +practice+BMI , data=training, method="glm",  trControl=trainControl)
summary(final.glm)
print (final.glm)
#confusion matrix for train data
predslog <- predict(final.glm, data=training, type = "raw")
tabtrain <- table(Predicted = predslog, Actual = training$Qualify_PhysicalTest)
confusionMatrix(tabtrain)
#confusion matrix for test data
predstest <- predict(final.glm, newdata=test, type = "raw")
tabtest <- table(Predicted = predstest, Actual = test$Qualify_PhysicalTest)
confusionMatrix(tabtest)
