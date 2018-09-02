setwd("F:/NCIRL_CLASS/Sem2/ADM/CA 2017-20180623")
getwd()
hrdata <- read.csv("ADM CA 1 Data.csv",stringsAsFactors = F)
#dat <- read.rds("ACS.rds")
str(hrdata)

set.seed(17154928)
my_dataset <- hrdata[order(runif(600)),]
my_dataset <- my_dataset[-10]

col1 <- round(runif(1)*32)+2
col2 <- round(runif(1)*31)+2
col3 <- round(runif(1)*30)+2

cols <- names(my_dataset)

print(paste("I lost: ", cols[col1], ",", cols[col2], ",", cols[col3]))

library(caret)
nearZeroVar(my_dataset,saveMetrics = TRUE)

names(my_dataset[col1])
my_dataset <- my_dataset[-col1]

names(my_dataset[col2])
my_dataset <- my_dataset[-col2]

names(my_dataset[col3])
my_dataset <- my_dataset[-col3]

str(my_dataset)
my_dataset$Attrition <- as.factor(my_dataset$Attrition)
my_dataset$BusinessTravel <- as.factor(my_dataset$BusinessTravel)
my_dataset$Department <- as.factor(my_dataset$Department)
#my_dataset$ï..Age <- as.numeric(my_dataset$ï..Age)
my_dataset$Education <- factor(my_dataset$Education, levels = c(1,2,3,4,5), labels = c("BC","Col","Bach","Mas","Dr"))
my_dataset$EducationField <- as.factor(my_dataset$EducationField)
my_dataset$Gender <- factor(my_dataset$Gender)
my_dataset$JobInvolvement <- factor(my_dataset$JobInvolvement,levels = c(1,2,3,4),labels = c("Low","Medium","High","Very High"))
my_dataset$JobRole <- as.factor(my_dataset$JobLevel)
my_dataset$JobSatisfaction <- factor(my_dataset$JobSatisfaction,levels = c(1,2,3,4),labels = c("Low","Medium","High","Very High"))
my_dataset$MaritalStatus <- as.factor(my_dataset$MaritalStatus)
my_dataset$Over18 <- as.factor(my_dataset$Over18)
my_dataset$OverTime <- as.factor(my_dataset$OverTime)
my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating, levels =c(1,2,3,4), labels = c("Low","Good","Excellent","Outstanding"))
my_dataset$JobInvolvement <- factor(my_dataset$RelationshipSatisfaction,levels = c(1,2,3,4),labels = c("Low","Medium","High","Very High"))
my_dataset$StockOptionLevel <- as.factor(my_dataset$StockOptionLevel)
my_dataset$WorkLifeBalance <- factor(my_dataset$WorkLifeBalance,levels = c(1,2,3,4), labels =c("Bad","Good","Better","Best"))
my_dataset$RelationshipSatisfaction <- factor(my_dataset$RelationshipSatisfaction, levels = c(1:4), labels = c("Low","Medium","High","Very High"))
str(my_dataset)

summary(my_dataset)
my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating)
names(my_dataset[20])
my_dataset <- my_dataset[-20]

## end of categorizing and removing pointless data

str(my_dataset)
names(my_dataset[20])
my_dataset <- my_dataset[-7]
my_dataset <- my_dataset[-18]
my_dataset <- my_dataset[-21] #remove std hrs

#missing values
sapply(my_dataset,function(x) sum(is.na(x)))
#to remove missing values
#my_Dataset <- na.omit(my_dataset)
#
#To impute missing values
library(missMDA)
#Get only numeric data to do PCA

#n <- sapply(my_dataset,function(x) {is.numeric(x)})
#n
#numerics <- my_dataset[,n]
str(my_dataset)
numerics <- my_dataset[,c(1,4,6,9,11)]
#To find the optimal ncp
miss1 <- estim_ncpPCA(numerics, ncp.min = 0, ncp.max = 6)
#Imputiing the missing values
miss2 <- imputePCA(numerics, ncp = miss1$ncp)
#Data conversion 
miss3 <- data.frame(miss2$completeObs)

#cleaning
#my_dataset$<> <- gsub("%","",data$<>)


boxplot(subset(my_dataset,select = c(1,4)))
boxplot(subset(my_dataset, select=c(1,6,18,20,21:24))) 
boxplot(subset(my_dataset, select=c(26:29))) 

boxplot(my_dataset$DailyRate)
boxplot(my_dataset$HourlyRate)

boxplot(my_dataset$MonthlyIncome) 

#library(dplyr)
#To remove outliers
summary(my_dataset$MonthlyIncome)
bench <- 6556 +1.5*IQR(my_dataset$MonthlyIncome)
#my_dataset$MonthlyIncome[my_dataset$MonthlyIncome>bench]<-median(my_dataset$MonthlyIncome)
my_dataset$MonthlyIncome[my_dataset$MonthlyIncome>bench]<-bench
#my_dataset$MonthlyIncome[my_dataset$MonthlyIncome>bench]

#outliers <- my_dataset$MonthlyIncome[my_dataset$MonthlyIncome > bench]
#my_dataset <- my_dataset[-my_dataset$MonthlyIncome > bench]
#my_dataset <- my_dataset[-outliers,]

boxplot(my_dataset$MonthlyRate)
boxplot(subset(my_dataset,select = c(1,4,5)))
boxplot(subset(my_dataset,select = c(8,9,11:13)))
boxplot(subset(my_dataset,select = c(15,17:19)))



#F2:
table(my_dataset$Attrition)
prop.table(table(my_dataset$Attrition))

#F3:
#Random Sample
index <- sample(1:dim(my_dataset)[1],dim(my_dataset)[1] * .75, replace = FALSE)
training <- my_dataset[index,]
testing <- my_dataset[-index,]

## OTHR WAY
ind <- sample(2,nrow(my_dataset),replace = TRUE,prob = c(0.75,0.25))
train <- my_dataset[ind==1,]
test <- my_dataset[ind==2,]

#stratified sample
##for imbalanced data
library(caret)
#install.packages("DMwR")
library(DMwR)
#install.packages("TTR", dependencies = TRUE)
sample_bal <- DMwR::SMOTE(Attrition ~.,my_dataset,perc.over = 100, perc.under = 200)
table(sample_bal$Attrition)


sample <- createDataPartition(my_dataset$Attrition,p=0.75,list=FALSE)
training2 <- my_dataset[sample,]
testing2 <- my_dataset[-sample,]



#F4
#b1 - everyone stays
str(testing2$Attrition)
AttritionModel <- rep(0,dim(testing2)[1])
table(AttritionModel)
AttritionModel <- factor(AttritionModel,levels = c(0,1),labels = c("No","Yes"))
(AttritionAccuracy <- 1 - mean(AttritionModel !=  testing2$Attrition))


#women are more loyal and tend to stay
str(my_dataset$Gender)

str(my_dataset$JobSatisfaction)
#table(my_dataset$JobSatisfaction)
b2 <- rep(0,dim(testing2)[1])
b2[testing2$JobSatisfaction == 'Low'] <- 1
b2[testing2$JobSatisfaction == 'Medium'] <- 1
b2 <- factor(b2,levels = c(0,1), labels = c("No","Yes"))
(JSAccuracy <- 1-mean(b2 != testing2$Attrition))

table(training2$Attrition,training2$Gender)

table(training2$Attrition,training2$PerformanceRating)

table(training2$Attrition)
prop.table(table(training2$Attrition))


#B questions
#B1
str(my_dataset)
str(subset(my_dataset,select = c(1,4,6,8,10,12,16,17,18,22,24,25,26)))
cor(subset(my_dataset,select = c(1,4,6,7,9,11,15,16,17,20,21,24,25)))
    #,use="pairwise",method="spearman"))

#B2
str(my_dataset)
boxplot(my_dataset$ï..Age ~ my_dataset$Attrition, xlab = "Attrition",ylab="Education")
?boxplot

spineplot(my_dataset$Attrition ~ my_dataset$EnvironmentSatisfaction, xlab = "env_satis", ylab ="attrition") 
#B3
#2 Gender based observations
spineplot(my_dataset$Attrition ~ my_dataset$Gender)
boxplot(my_dataset$DailyRate ~ my_dataset$Gender)

#2 overtime based observations
spineplot(my_dataset$Attrition ~ my_dataset$OverTime)
spineplot(my_dataset$JobLevel ~ my_dataset$OverTime )

#2 marital status based observations
spineplot(my_dataset$Attrition ~ my_dataset$MaritalStatus)
spineplot(my_dataset$Gender ~ my_dataset$MaritalStatus)

#B4
plot(my_dataset$ï..Age,my_dataset$HourlyRate)
plot(my_dataset$ï..Age,my_dataset$DailyRate)
plot(my_dataset$ï..Age,my_dataset$MonthlyRate)

#B5
counts <- table(my_dataset$Attrition, my_dataset$JobSatisfaction)
barplot(counts,main="Attrition against Job satisfaction", legend = row.names(counts),beside = T)
barplot(counts,main="Attrition against Job satisfaction",xlab = "Job Satisfaction",ylab="Attrition", col=c("darkblue","red"),legend = row.names(counts),beside = T)



#Intermediate Qsns
#I1
#install.packages("C50")
library(C50)
c50_Model <- C5.0(Attrition ~.,data = training2, trials = 100)
summary(c50_Model)
c50_predict <- predict(c50_Model,testing2)
#c50_p <- predict(c50_Model,newdata = testing2)

#chkn accuracy of prediction model
(c50_Accuracy <- 1-mean(c50_predict != testing2$Attrition))

#comparing with Benchmark 1
(c50_Accuracy - AttritionAccuracy)
#comparing with Benchmark 2
(c50_Accuracy - JSAccuracy)

caret::confusionMatrix(c50_predict, testing2$Attrition, positive = "Yes")


##tuning c5.0



#I2
library(class)
library(randomForest)

summary(my_dataset)
#KNN Lab soln
n <- sapply(my_dataset,function(x) {is.numeric(x)})
n
numerics <- my_dataset[,n]
summary(numerics)
normalize <- function(x){return((x-min(x))/(max(x) - min(x)))}
normalnumeircs <- normalize(numerics)
summary(normalnumeircs)

myKNN <- my_dataset[,!n]
myKNN <- cbind(myKNN,normalnumeircs)
summary(myKNN)
library(dummies)
#install.packages("dummies")
tknn <- dummy.data.frame(myKNN[,-1])
summary(tknn)
length(tknn)
#TempTrainAttr <- myKNN[,1]

train_KNN <- tknn[sample,]
test_KNN <- tknn[-sample,]
train_KNN_Atrr <- myKNN[sample,1]
test_KNN_Atrr <- myKNN[-sample,1]
##length(train_KNN_Atrr)

k1 <- round(sqrt(dim(train_KNN)[1]))
k2 <- round(sqrt(dim(train_KNN)[2]))
k3 <- 7

library(class)
knn1 <- knn(train = train_KNN, test = test_KNN, cl = train_KNN_Atrr, k = k1)
knn2 <- knn(train = train_KNN, test = test_KNN, cl = train_KNN_Atrr, k = k2)
knn3 <- knn(train = train_KNN, test = test_KNN, cl = train_KNN_Atrr, k = k3)

(knn1Acc <- 1-mean(knn1 != test_KNN_Atrr))
(knn2Acc <- 1-mean(knn2 != test_KNN_Atrr))
(knn3Acc <- 1-mean(knn3 != test_KNN_Atrr))

(knn1Acc - AttritionAccuracy)
(knn2Acc - AttritionAccuracy)
(knn3Acc - AttritionAccuracy)

#none
#Trees
#rpart - recursive partitioning
regressiontree <- rpart(Attrition ~ .,data = training2, method = "class")
plot(regressiontree)
text(regressiontree)

#########
?rpart.control
newrpart <- rpart(Attrition ~ ., data = training2, method = "class",control = rpart.control(minsplit = 2,cp=0))


#I3
logit <- glm(training2$Attrition ~ ., family = binomial(link='logit'),data=training2, maxit=100)
summary(logit)
plot(logit)
anova(logit,test="Chisq")
#install.packages("pscl")
library(pscl)
pR2(logit)

logit.pred <- predict(logit, newdata = testing2, type='response')
results.logit <- ifelse(logit.pred > 0.5, 'Yes', 'No')
prop.table(table(results.logit))
(logit.acc1 <- 1 - mean(results.logit != testing2$Attrition))

logit.pred2 <- predict(logit, newdata = testing2, type='response')
results.logit2 <- ifelse(logit.pred2 > 0.6, 'Yes', 'No')
prop.table(table(results.logit2))
(logit.acc2 <- 1 - mean(results.logit2 != testing2$Attrition))

logit.pred3 <- predict(logit, newdata = testing2, type='response')
results.logit3 <- ifelse(logit.pred3 > 0.75, 'Yes', 'No')
prop.table(table(results.logit3))
(logit.acc2 <- 1 - mean(results.logit3 != testing2$Attrition))



#i6
#randomforest - 
library(randomForest)
forest <- randomForest(Attrition ~ ., data = training2, importance= TRUE, ntree = 2000)
#chk wch r important variables
varImpPlot(forest)

#plot the 4 most important variables
spineplot(my_dataset$Attrition ~ my_dataset$ï..Age)
spineplot(my_dataset$Attrition ~ my_dataset$MonthlyIncome)
spineplot(my_dataset$Attrition ~ my_dataset$OverTime)
spineplot(my_dataset$Attrition ~ my_dataset$MonthlyRate)


#chkn accuracy (not part of this qsn)
RFpred <- predict(forest,testing2[,-2],type = "class")
(RFacc <- 1-mean(RFpred != testing2$Attrition))

#
Rfc <- predict(forest, testing2, type = "class")
caret::confusionMatrix(RFpred, testing2$Attrition, positive = "Yes")

#A3
#CI Trees
#conditional Inference Trees
library(partykit)
ctree <- ctree(Attrition ~ ., data = training2)
print(ctree)
plot(ctree,type = "simple")

ctreepred <- predict(ctree,testing2)
(ctreeacc <- 1-mean(ctreepred != testing2$Attrition))

#CForest
#install.packages("party")
library(party)
cForest <- cforest(Attrition ~ .,data = training2, controls=cforest_unbiased(ntree=2000,mtry=3))
cFp <- predict(cForest,newdata = testing2[,-2] )
(cFAcc <- 1-mean(cFp != testing2$Attrition))



#NB
#install.packages("naiveBayes")
library(e1071)
#install.packages("OneR")
library(OneR)
str(my_dataset)
#n <- sapply(my_dataset,function(x) {is.numeric(x)})
#n
myNB <- my_dataset[,n]
summary(myNB)
binmyNB <- bin(myNB,nbin=5, method = "clusters",na.omit=TRUE)
str(binmyNB)
myNBbin <- cbind(binmyNB,my_dataset[,!n])

trainnb <- myNBbin[sample,] 
testnb <- myNBbin[-sample,]

nb <- naiveBayes(Attrition ~ ., data = trainnb)
nbp <- predict(nb, newdata = testnb[,-15])
(nbacc <- 1-mean(nbp != testnb$Attrition)) 


#RandomForest 
library(randomForest)
forest <- randomForest(Attrition ~ ., data = training2, importance= TRUE, ntree = 200)
#chkn accuracy (not part of this qsn)
RFpred <- predict(forest,testing2[,-2],type = "class")
(RFacc <- 1-mean(RFpred != testing2$Attrition))


#kfold for all modules#
tunegrid <- expand.grid(.mtry=5)
control <- trainControl(method = "repeatedcv",number = 10, repeats = 3, search = "grid")

CFmodel <- train(Attrition ~ .,data = training2, method = "cforest", 
                 metric="Accuracy",tuneGrid = tunegrid, 
                 controls=cforest_unbiased(ntree=250))

cFpp <- predict(CFmodel,newdata = testing2[,-2] )
(cFAccp <- 1-mean(cFpp != testing2$Attrition))
(cFAccp - cFAcc) #0 - no improvement

RFmodel <- train(Attrition ~., data = training2, method = "rf",
                 metric="Accuracy",tuneGrid = tunegrid,
                 trControl = control)

RFppred <- predict(RFmodel,testing2[,-2])
(RFaccp <- 1-mean(RFppred != testing2$Attrition))
(RFaccp - RFacc)

nbModel <- train(Attrition ~ ., data=training2, method = "nb", trcontrol = control)
nbpp <- predict(nbModel, newdata = testing2[,-2])
(nbaccp <- 1-mean(nbpp != testing2$Attrition)) 
(nbaccp - nbacc)


#nb > rf > cf

#tuning c5.0
c50model <- train(Attrition ~ ., data=training2, method = "C5.0", metric="Accuracy",trControl = control)
c50_predict_tune <- predict(c50model,testing2)
#chkn accuracy of prediction model
(c50_Accuracy_tune <- 1-mean(c50_predict_tune != testing2$Attrition))
(c50_Accuracy_tune - c50_Accuracy)


#rotation forest
#install.packages("rotationForest")
#for rotation forest, isolate categoricals and numericals, dummy encode categoricals, combine them back
#isolate dependent
#same as knn but without the normalize
library(dummies)
library(rotationForest)
n <- sapply(my_dataset,function(x) {is.numeric(x)})
n
#numerics <- my_dataset[,n]
#summary(numerics)
#normalize <- function(x){return((x-min(x))/(max(x) - min(x)))}
#normalnumeircs <- normalize(numerics)
#summary(normalnumeircs)

myRotF <- my_dataset[,!n]
myRotF <- cbind(myRotF,my_dataset[,n])
myRotF <- dummy.data.frame(myRotF[,-1])

summary(myRotF)
Attrition <- factor(my_dataset$Attrition, levels = c("Yes","No"), labels = c(1,0))
str(myRotF)
#create testing and training datasets nw
train_rotF <- myRotF[sample,]
test_rotF <- myRotF[-sample,]

rotForest <- rotationForest(y=Attrition[sample], x=train_rotF)
#predict dint wrk
#rotpredict <- predict(rotForest,newdata = test_rotF,all=FALSE)

#Only numerics for PCA
#
y <- cbind(titanicData$Survived, titanicData$PassengerId, titanicData$Age, titanicData$Fare)
summary(y)
cor(y)
y <- data.frame(y)
#Principle Component Analysis on Y
pca1 <- princomp(y[,-1], scores = TRUE, cor = TRUE)
#Summary with show the cummulative proportion and Std. deviation. Squaring the Std.deviation
#will give us the eigen values. We will consider the components those who has the eigen 
#values greater than 1
summary(pca1)
library(ggplot2)
#plot to identify the number of components
plot(pca1)
screeplot(pca1, type = "line", main = "Scree Plot")
autoplot(pca1, data = y1)

y1 <- cbind(my_dataset$HourlyRate,my_dataset$DailyRate,my_dataset$MonthlyRate)
summary(y1)
cor(y1)

y1 <- data.frame(y1)
pca1 <- princomp(y1, scores = TRUE, cor = TRUE)
summary(pca1)

pcs <- prcomp(~ ., data=normalnumeircs)
plot(pcs, type="l")


#
install.packages("missMDA")