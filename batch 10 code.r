# Setting working directory
getwd()
setwd("C:/Users/Sm/Desktop/Ou elective/Projects/Batch10 diabats/Bala Kartik")

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

# we can also set/change the variable names using colnames

colnames(dataset) <- c("preg_times", "glucose_test", "blood_press", "tsk_thickness",
                       "serum", "bm_index", "pedigree_fun", "age", "dep")

str(dataset)

dataset$dep <- as.factor(dataset$dep)
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

# As Missing value is coded 0 we have mention 0 as missing value 
cols_change <- colnames(dataset)[!colnames(dataset) %in% c("preg_times","serum", "dep")]
bool_data <- dataset[cols_change] == 0
dataset[cols_change][bool_data] <- NA

print(all(!is.na(dataset)))

#Missing value  Proportion for all the variables
sapply(dataset, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})
# Missing values impute for small proportion of missing values
library(Hmisc)
dataset$glucose_test[is.na(dataset$glucose_test)]<-mean(dataset$glucose_test,na.rm=T)
dataset$blood_press[is.na(dataset$blood_press)]<-mean(dataset$blood_press,na.rm=T)
dataset$tsk_thickness[is.na(dataset$tsk_thickness)]<-mean(dataset$tsk_thickness,na.rm=T)
dataset$bm_index[is.na(dataset$bm_index)]<-mean(dataset$bm_index,na.rm=T)

sum(is.na(dataset))
#######################################################################
dataset$glucose_test=as.numeric(dataset$glucose_test)
dataset$blood_press=as.numeric(dataset$blood_press)
dataset$tsk_thickness=as.numeric(dataset$tsk_thickness)
dataset$bm_index=as.numeric(dataset$bm_index)
str(dataset)

write.csv(dataset,"afterimp.csv")

#######################################################################
boxplot(dataset$preg_times)
boxplot(dataset$serum)


dataset$preg_times[dataset$preg_times>quantile(dataset$preg_times, 0.95)] <- quantile(dataset$preg_times, 0.95)
dataset$preg_times[dataset$preg_times<quantile(dataset$preg_times, 0.05)] <- quantile(dataset$preg_times, 0.05)

dataset$serum[dataset$serum>quantile(dataset$serum, 0.95)] <- quantile(dataset$serum, 0.95)
dataset$serum[dataset$serum<quantile(dataset$serum, 0.05)] <- quantile(dataset$serum, 0.05)


boxplot(preg_times ~ dep, data=dataset, 
        main="Income vs Occupation Type", 
        ylab="Income")

pairs(dataset)

pre1=dataset[c(1:8)]
library(corrplot)
pre1.cor	=	cor(pre1)
corrplot(pre1.cor,	method="circle")

# Continuous vs categories
library(caret)
x <- dataset[,1:8]
y <- dataset[,9]
featurePlot(x=x, y=y, plot="box")

#######################################################################
# Normalizing input data
#######################################################################
scale_training <- as.data.frame(scale(dataset[, -9],  
                                center = TRUE, scale = TRUE))
scale_training$dep <- dataset[, "dep"]
names(scale_training)
table(scale_training$dep)

write.csv(scale_training,"scaledata.csv")

library(caret)
x <- scale_training[,1:8]
y <- scale_training[,9]
featurePlot(x=x, y=y, plot="box")



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
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)

# CART

#DT
set.seed(100)

fit.cart <- train(dep~., data=training, method="rpart",
                  trControl=trainControl)
summary(fit.cart)

#Logistic Regression
set.seed(100)
fit.glm <-train (dep~., data=training, method="glm",
                 trControl=trainControl)

summary(fit.glm)
print(fit.glm)


#SVm
set.seed(100)
fit.svm <- train(dep~., data=training, method="svmRadial",
                 trControl=trainControl)
print(fit.svm)

# KNN
set.seed(7)

fit.knn <- train(dep~., data=training, method="knn", trControl=trainControl)
print(fit.knn)


# Random Forest
set.seed(7)
fit.rf <- train(dep~., data=training, method="rf", trControl=trainControl)



#collect resamples
results <- resamples(list(CART=fit.cart, logistic=fit.glm, SVM=fit.svm, KNN=fit.knn, RF=fit.rf))

summary(results)

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


##############################################

# Tunnign random forest 

bestmtry <- tuneRF(training[, c(-9)],training$dep, ntreeTry=300, 
                   stepFactor=2,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
print(bestmtry)


# Final model

rf_model3 <- randomForest(dep ~ ., data =training, ntree=300, mtry=2)



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



final.glm <-train (dep ~ glucose_test + bm_index + age + pedigree_fun, data=training, method="glm",
                   trControl=trainControl)



summary(final.glm)
print (final.glm)


predslog <- predict(final.glm, data=training, type = "raw")

tabtrain <- table(Predicted = predslog, Actual = training$dep)
caret::confusionMatrix(predslog,training$dep,positive="1")
##############################################

#On testing
##############################################
dim(test)
names(test)


p2 <- predict(final.glm,newdata=test,type="raw")

tabtest <- table(Predicted = p2, Actual = test$dep)
caret::confusionMatrix(p2,test$dep,positive="1")
##############################################



