getwd()
setwd("C:/DMMLT")
getwd()
###################################################
#Reading data
###################################################################
data<-read.csv('email.csv',stringsAsFactors = T, na.strings = c(""," ","NA","?",NA))
View(data)
head(data,10)
tail(data)
#Structure of Dataset :
names(data)
pairs(data)
str(data)
summary(data)
dim(data)
###################################################################
# Correctign data types
###################################################################
data$Email_Status<-as.factor(data$Email_Status)
data$Time_Email_sent_Category<-as.factor(data$Time_Email_sent_Category)
data$Email_Type<-as.factor(data$Email_Type)
data$Email_Source_Type<-as.factor(data$Email_Source_Type)
data$Email_Campaign_Type<-as.factor(data$Email_Campaign_Type)
str(data)
###################################################################
#5.	Find missing values in data set if any.
sum(is.na(data))

#Missing value  Proportion for all the variables
sapply(data, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})
###################################################################
#IMPUTATION
######################################3
install.packages("Hmisc")
library(Hmisc)
data$Total_Past_Communications[is.na(data$Total_Past_Communications)]<-median(data$Total_Past_Communications,na.rm=T)
data$Total_Links[is.na(data$Total_Links)]<-median(data$Total_Links,na.rm=T)
data$Total_Images[is.na(data$Total_Images)]<-median(data$Total_Images,na.rm=T)
#Imputing with most frequent occuring level
data$Customer_Location[is.na(data$Customer_Location)]<-'G'
sum(is.na(data))

#######

sapply(data, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})
######################################3
# Removing outliers
######################################
boxplot(data$Subject_Hotness_Score)
boxplot(data$Total_Past_Communications)
boxplot(data$Total_Links)
boxplot(data$Total_Images)

data$Subject_Hotness_Score[data$Subject_Hotness_Score>quantile(data$Subject_Hotness_Score, 0.95)] <- quantile(data$Subject_Hotness_Score, 0.95)
data$Total_Links[data$Total_Links>quantile(data$Total_Links, 0.95)] <- quantile(data$Total_Links, 0.95)
data$Total_Images[data$Total_Images>quantile(data$Total_Images, 0.90)] <- quantile(data$Total_Images, 0.90)

boxplot(data$Subject_Hotness_Score)
boxplot(data$Total_Links)
boxplot(data$Total_Images)

names(data)

######################
# Hypothesis testing
######################
chisq.test(data$Email_Status, data$Email_Type, correct=FALSE)
chisq.test(data$Email_Status, data$Customer_Location , correct=FALSE)
chisq.test(data$Email_Status, data$Email_Campaign_Type , correct=FALSE)

#anova
x1=aov(data$Total_Links ~ data$Email_Status)
summary(x1)
x2=aov(data$Total_Images  ~ data$Email_Status)
summary(x2)
summary(data)
######################################3
train_rows<- sample(1:nrow(data), size=0.7*nrow(data))
train_rows
training <- data[train_rows, ]
test <- data[-train_rows, ]
dim(data)
dim(training)
dim(test)
names(training)
names(test)
str(training)
######################################3
# set-up test options
install.packages("dplyr")
library(dplyr)
install.packages("caret")
library(caret)
control <- trainControl(method="repeatedcv", number=5)
seed <- 7
metric <- "Accuracy"

# Multinomnal  Regression

set.seed(seed)
fit.glm1 <- train(Email_Status~., data=training, method="glm", metric=metric, trControl=control)
print(fit.glm1)

# CART
set.seed(seed)
fit.cart <- train(Email_Status~., data=training, method="rpart", metric=metric, trControl=control)
print(fit.cart)
# kNN
set.seed(seed)
fit.knn <- train(Email_Status~., data=training, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
print(fit.knn)
# SVM
set.seed(seed)
fit.svm <- train(Email_Status~., data=training, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
print(fit.svm)
# Random Forest
set.seed(seed)
fit.rf <- train(Email_Status~., data=training, method="rf", metric=metric, trControl=control)
print(fit.rf)

# Compare algorithms
results <- resamples(list(logistic=fit.glm1,svm=fit.svm, knn=fit.knn, DT=fit.cart,rf=fit.rf ))
# Table comparison
summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

###########################
#################################################################
# Checking Random Forest
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
#########################################################################3
library(caret)
install.packages("randomForest")
library(randomForest)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
#tunegrid <- expand.grid(.mtry=c(1:5), .ntree=c(100, 150, 200, 250))
tunegrid <- expand.grid(.mtry=c(1:5), .ntree=c(100,200,500))

set.seed(seed)
custom <- train(Email_Status~Email_Type+               
                  Subject_Hotness_Score+     Email_Source_Type+        
                  Customer_Location+         Email_Campaign_Type+      
                  Total_Past_Communications+ Time_Email_sent_Category+ 
                  Word_Count+                Total_Links+              
                  Total_Images,data = training,method=customRF, tuneGrid=tunegrid, trControl=control)


print(custom)



####################################################

library('randomForest')

rf_model <- randomForest(Email_Status~Email_Type+               
                           Subject_Hotness_Score+     Email_Source_Type+        
                           Customer_Location+         Email_Campaign_Type+      
                           Total_Past_Communications+ Time_Email_sent_Category+ 
                           Word_Count+                Total_Links+              
                           Total_Images,data = training,ntree=200,mtry=2,trControl=control)


#################################################################
# VAriable Importance
#################################################################

#We can have a look at the variable importance of our random forest model below :

install.packages("ggthemes")
library('ggthemes')
library('dplyr')

importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Vari
       ables') +
  coord_flip() + 
  theme_few()
############################################

#Final model
############################################
install.packages("nnet")
library(nnet)
mymodel <- glm(Email_Status~Subject_Hotness_Score+Customer_Location+
  Total_Past_Communications+Word_Count+Total_Links+Total_Images+Email_Campaign_Type, data=training,family='binomial')

# Prediction
p1 <- predict(mymodel, newdata=training, type = 'response')
pred1 <- ifelse(p1>0.5, 1, 0)
library(caret)
confusionMatrix(as.factor(pred1),training$Email_Status)





#FoR Test data
p2=predict(mymodel, newdata=test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
library(caret)
confusionMatrix(as.factor(pred2),test$Email_Status)


