getwd()
setwd("D:/project")
getwd()
################3
# reading data
data <- read.csv("mphil.csv")
dim(data)
str(data)
View(data)

sum(is.na(all))

#Missing value  Proportion for all the variables

sapply(data, function(df) {
  (sum(is.na(df)==TRUE)/ length(df))*100;
})


# checking is there any correlation  to carry out factor analysis
########################################################################
library(corrplot)
pre1.cor	=	cor(data)
write.csv (pre1.cor,"correlations.csv")
corrplot(pre1.cor,	method="circle")

########################################################################
# Runiing factor analsyis and profiling them
########################################################################
head(data)
# Run PCA to determine number of factors
data.pca <- princomp(data)
# Summary of proprtion of variances : We notice as the component increases the proportion of variance decreases .
sum_pca<-summary(data.pca)
xx=sum_pca
print(sum_pca)

plot(data.pca)
# Based on the summary it appears 4 components exist .
# Run Factor Analysis

library(psych)
fact<-factanal(data,factors=5,rotation="varimax")
print(fact$loadings, digits=2, cutoff=.3, sort=FALSE)

# To sort the loadings
print(fact$loadings, digits=2, cutoff=.3, sort=TRUE)

write.csv(fact$loadings,"factor5new.csv")
######################################3
fact1<-factanal(data,factors=5,rotation="varimax", scores = "regression")
fact1$scores 
write.csv(fact1$scores ,"factorscores.csv")

# To run cluster Analysis  
# K means Clustering :
#K-means clustering is the most commonly used unsupervised machine learning algorithm for partitioning 
#a given data set into a set of k groups (i.e. k clusters), where k represents the number of groups pre-specified by the analyst.

#The basic idea behind k-means clustering consists of defining clusters so that the 
#total intra-cluster variation (known as total within-cluster variation) is minimized. 

# Set Seed = 20 -> This means that R will try 20 different random starting assignments 
# and then select the one with the lowest within cluster variation.

#The classification of observations into groups requires some methods for computing the distance 
#or the (dis)similarity between each pair of observations. 
#The result of this computation is known as a dissimilarity or distance matrix. 

#The choice of distance measures is a critical step in clustering. 

set.seed(20)
Cluster <- kmeans(fact1$scores, 3, nstart = 20)
Cluster
#Cluster Size 
Cluster$size
#Cluster membership 
Cluster$cluster
xx=cbind(fact1$scores,Cluster$cluster)

write.csv(xx,"factorcluster.csv")


library(dplyr)
library(tidyr)


df<-read.csv("factorcluster.csv")
names(df)

##################################33
########################################################################
# Discriminant Analysis 
# Combining the cluster membership to the original data file .
Dis_data <- cbind(data,Cluster$cluster)

# It gives the number of respondents and no of variables in a data set
dim(Dis_data)
head(Dis_data)
str(Dis_data)

write.csv(Dis_data,"class.csv")



#Read the data

calgo=read.csv("class.csv")
str(calgo)
calgo$cluster=as.factor(calgo$cluster)


library(MASS)
LDA <- lda(cluster ~ . , data = calgo)
LDA

# Output : Prior Probabilities -> this tells us what the probabilities of the group in the dataset 
# : Group means , Coeff of linear discrimination -> gives the coeff used in discriminant equation .

#To predict the class membership
names(Dis_data)
LDA_predict <- predict(LDA, data = calgo[,c(1:27)])
LDA_predict$class

names(Dis_data)


#Determine how well the model fits.
table(LDA_predict$class, calgo$Cluster)
#OUTPUT : Good model , as the misclassification is very less .

# To cross validate the model 
## perform LDA setting CV as TRUE -> This does leave one out classification , it leave the data point and try 
# to fit on the rest of the data and then try to predict the leftout data point .
LDA_predict_CV <- predict(LDA, data = calgo[,c(1:27)], CV=TRUE)
#Determine how well the model fits.
table(LDA_predict_CV$class, calgo$Cluster)

##########################################################3
#Predicting on test daat
##########################################################3
test=read.csv("test.csv")

LDA_predict <- predict(LDA, newdata = test)$class



dim(test)

LDA_predict <- predict(LDA, newdata = test)$class

##########################################################3























