# Setting working directory
getwd()
setwd("C:/Users/SS ENTER/Desktop/jyothi")
getwd()

# Laoding required libraries to perform modeling
library(mice)
library(randomForest)
library(ggplot2)
library(glmnet)

#######################################################################
# reading data from Folder and checking for the data types
dataset <- read.csv(file.choose(), header = T)
str(dataset)

table(dataset$ldisease)


dataset$ldisease <- as.factor(dataset$ldisease)
dataset$gender <- as.factor(dataset$gender)
str(dataset)

dim(dataset)
head(dataset)
tail(dataset)
#######################################################################
#Checking for Missing value
print(all(!is.na(dataset)))

#Missing value  Proportion for all the variables
sapply(dataset, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})

# Missing values impute for small proportion of missing values
dataset$alb_glob_ratio[is.na(dataset$alb_glob_ratio)]<-mean(dataset$alb_glob_ratio,na.rm=T)
sum(is.na(dataset))
dataset$alb_glob_ratio=as.numeric(dataset$alb_glob_ratio)
str(dataset)

#Missing value  Proportion for all the variables
sapply(dataset, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})



#######################################################################
boxplot(dataset$total_proteins)
boxplot(dataset$alk_phos)
boxplot(dataset$age)
boxplot(dataset$total_brubin)
boxplot(dataset$direct_brubin)
boxplot(dataset$alam_amin)
boxplot(dataset$asp_amin)
boxplot(dataset$albumin)
boxplot(dataset$alb_glob_ratio)


dataset$total_proteins[dataset$total_proteins>quantile(dataset$total_proteins, 0.95)] <- quantile(dataset$total_proteins, 0.95)
dataset$total_proteins[dataset$total_proteins<quantile(dataset$total_proteins, 0.05)] <- quantile(dataset$total_proteins, 0.05)
dataset$alk_phos[dataset$alk_phos>quantile(dataset$alk_phos, 0.85)] <- quantile(dataset$alk_phos, 0.85)
dataset$total_brubin[dataset$total_brubin>quantile(dataset$total_brubin, 0.85)] <- quantile(dataset$total_brubin, 0.85)
dataset$direct_brubin[dataset$direct_brubin>quantile(dataset$direct_brubin, 0.85)] <- quantile(dataset$direct_brubin, 0.85)
dataset$alam_amin[dataset$alam_amin>quantile(dataset$alam_amin, 0.85)] <- quantile(dataset$alam_amin, 0.85)
dataset$asp_amin[dataset$asp_amin>quantile(dataset$asp_amin, 0.85)] <- quantile(dataset$asp_amin, 0.85)
dataset$albumin[dataset$albumin>quantile(dataset$albumin, 0.95)] <- quantile(dataset$albumin, 0.95)
dataset$albumin[dataset$albumin<quantile(dataset$albumin, 0.05)] <- quantile(dataset$albumin, 0.05)
dataset$alb_glob_ratio[dataset$alb_glob_ratio>quantile(dataset$alb_glob_ratio, 0.90)] <- quantile(dataset$alb_glob_ratio, 0.90)
dataset$alb_glob_ratio[dataset$alb_glob_ratio<quantile(dataset$alb_glob_ratio, 0.05)] <- quantile(dataset$alb_glob_ratio, 0.05)

boxplot(dataset$total_proteins)
boxplot(dataset$alk_phos)
boxplot(dataset$age)
boxplot(dataset$total_brubin)
boxplot(dataset$direct_brubin)
boxplot(dataset$alam_amin)
boxplot(dataset$asp_amin)
boxplot(dataset$albumin)
boxplot(dataset$alb_glob_ratio)



pairs(dataset)
pre1=dataset[c(3:11)]
install.packages("corrplot")
library(corrplot)
pre1.cor	=	cor(pre1)
corrplot(pre1.cor,	method="number")

# Continuous vs categories
library(caret)
x <- dataset[,3:11]
y <- dataset[,1]
featurePlot(x=x, y=y, plot="box")

str(dataset)
cont<-subset(dataset,select = -c(gender, ldisease))
str(cont)

#######################################################################
# Normalizing input data
#######################################################################
scale_training <- as.data.frame(scale(cont[,],  
                                center = TRUE, scale = TRUE))
scale_training<-cbind(scale_training,dataset$ldisease,dataset$gender)
str(scale_training)

# we can also set/change the variable names using colnames

colnames(scale_training) <- c("age","total_brubin","direct_brubin","alk_phos","alam_amin","asp_amin","total_proteins","albumin","alb_glob_ratio","ldisease","gender")
names(scale_training)
table(scale_training$ldisease)

write.csv(scale_training,"scaledata.csv")

#######################################################################
# Splting data 
#######################################################################

train_rows<- sample(1:nrow(scale_training), size=0.9*nrow(scale_training))
train_rows
training <- scale_training[train_rows, ]
test <- scale_training[-train_rows, ]
dim(scale_training)
dim(training)
dim(test)
head(test)

#######################################################################
# Model Pipleline
#######################################################################
library(caret)
# Run algorithms using 10-fold cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# GLM
set.seed(7)
fit.glm <- train(ldisease~., data=dataset, method="glm", metric="Accuracy", trControl=control)
print(fit.glm)

# CART
set.seed(7)
grid <- expand.grid(.cp=c(0.01,0.05,0.1))
fit.cart <- train(ldisease~., data=training, method="rpart", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.cart)

# SVM
set.seed(7)
grid <- expand.grid(.sigma=c(0.01,0.05,0.1), .C=c(1))
fit.svm <- train(ldisease~., data=training, method="svmRadial", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.svm)


# kNN
set.seed(7)
grid <- expand.grid(.k=c(1,3,5,7))
fit.knn <- train(ldisease~., data=training, method="knn", metric="Accuracy", tuneGrid=grid, trControl=control)
print(fit.knn)

#######RF
fit.rf <- train(ldisease~., data=training, method="rf", metric="Accuracy", trControl=control)
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
library(caret)
library(randomForest)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:6), .ntree=c(100, 200, 300))
set.seed(100)
custom <- train(ldisease~., data=training,method=customRF, tuneGrid=tunegrid, trControl=control)

print(custom)

#########################################################################3
rf_model3 <- randomForest(ldisease ~ ., data =training, ntree=100, mtry=5)

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

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)


final.glm <-train (ldisease ~ asp_amin +  alk_phos + total_brubin + alam_amin + age + direct_brubin, data=training, method="glm",
                   trControl=trainControl)



summary(final.glm)
print (final.glm)


predslog <- predict(final.glm, data=training, type = "raw")
tabtrain <- table(Predicted = predslog, Actual = training$ldisease )
caret::confusionMatrix(predslog,training$ldisease)
##############################################

#On testing
##############################################
dim(test)
names(test)


p2 <- predict(final.glm,newdata=test,type="raw")

tabtest <- table(Predicted = p2, Actual = test$ldisease)

caret::confusionMatrix(p2,test$ldisease)
##############################################



