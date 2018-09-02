mydata <- read.csv(file.choose(), header =T)
str(mydata)
set.seed(123)
#Converting neccessary fields to factors
mydata$Survived <- as.factor(mydata$Survived)
mydata$Pclass <- as.factor(mydata$Pclass)
mydata$SibSp <- as.factor(mydata$SibSp)
mydata$Parch <- as.factor(mydata$Parch)

#Checking missing values
sapply(mydata, function(x) sum(is.na(x)))

#To impute missing values
library(missMDA)
numdata <- mydata[,c(1,6,10)]
a <- estim_ncpPCA(numdata,ncp.min = 0,ncp.max = 5)
numdata <- imputePCA(numdata,ncp=a$ncp)
newdata <- data.frame(numdata$completeObs)

#Replace the original Age field with the imputed field
indata <- cbind(mydata[,c(1:5)],newdata$Age,mydata[,c(7:12)])
#Renaming the column of a dataframe
colnames(indata)[6] <- "Age"

# To recheck the missing values
sapply(indata, function(x) sum(is.na(x)))
str(indata)

#To check class balance
prop.table(table(indata$Survived)) #The data is fairly balanced

#Split data into train and test
library(caret)
idx <- createDataPartition(indata$Survived, p=0.75, list = FALSE)
train <- indata[idx,]
test <- indata[-idx,]
titanicData <- indata
#Models on Titanic Data dependent variable - Survived
dependentColumnNumber<-which(colnames(train)=="Survived" )


#Violin Plot
ggplot(titanicData, aes(x=titanicData$Survived, y=titanicData$Age, fill=Survived)) + 
  geom_violin(trim=FALSE)


#Logistic Regression
logit<-glm(train$Survived~.,family = binomial(link = 'logit'),data=train[,c(-4,-9,-11)], maxit=100)
summary(logit)  
plot(logit)
anova(logit,test="Chisq")
library(pscl)
pR2(logit)  

results.1.logit <- predict(logit,newdata=test[,-dependentColumnNumber],type='response')
results.1.logit <- ifelse(results.1.logit > 0.5,1,0)
(logitAcc1 <- 1- mean(results.1.logit != test$Survived))

results.2.logit <- predict(logit,newdata=test[,-dependentColumnNumber],type='response')
# making model more conservative by pushing threshold closer to 1
results.2.logit <- ifelse(results.2.logit > 0.6,1,0)
(logitAcc2 <- 1- mean(results.2.logit != test$Survived))

results.3.logit <- predict(logit,newdata=test[,-dependentColumnNumber],type='response')
results.3.logit <- ifelse(results.3.logit > 0.75,1,0)
(logitAcc3 <- 1- mean(results.3.logit != test$Survived))


#Knn
#normalize data
n<-sapply(titanicData,function(x){is.numeric(x)})
numerics<-titanicData[,n]
summary(numerics)
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
numericsNormal <- normalize(numerics)
summary(numericsNormal)
titanicDataKNN<-titanicData[,!n]
titanicDataKNN<-cbind(titanicDataKNN,numericsNormal)
#install.packages("dummies")
library(dummies)
#handle even categorical data to represent as 1 and 0
#not taking dependent variable
tkNN<-dummy.data.frame(titanicData[,-2])
summary(tkNN)
train_KNN<-tkNN[idx,]
test_KNN<-tkNN[-idx,]
k2<-round(sqrt(dim(train_KNN)[2])) #sqrt of number of attribute
k1 <- round(sqrt(dim(test_KNN)[1])) #sqrt of number of instances
k3 <- 7 #a number between 3 and 10
library(class)

knn1 <- knn(train = train_KNN, test = test_KNN, cl = train$Survived, k=k1)
knn2<-knn(train = train_KNN, test = test_KNN, cl = train$Survived,k=k2)
knn3<-knn(train = train_KNN, test = test_KNN, cl = train$Survived,k=k3)

#Accuracy
(knn1Acc <- 1- mean(knn1 != test$Survived))
(knn2Acc <- 1- mean(knn2 != test$Survived))
(knn3Acc <- 1- mean(knn3 != test$Survived))

#Naive Bayes
# requires only categorical data hence lets discretize numeric features, by binning.
NBTitanicData<-titanicData
str(NBTitanicData) 
table(NBTitanicData$SibSp)
table(NBTitanicData$Parch)
#convert SibSp,Parch,Age and Fare to categorical Fsize is removed as FsizeD is derived from it.
NBTitanicData$SibSp<-as.factor(NBTitanicData$SibSp)
NBTitanicData$Parch<-as.factor(NBTitanicData$Parch)
str(NBTitanicData)  
NBTitanicData$Fsize<-NULL
#Fare
hist(NBTitanicData$Fare, breaks = 30)
library(OneR)
res <- bin(NBTitanicData$Fare, nbins = 3, method = "content", na.omit = TRUE)
NBTitanicData<-cbind(NBTitanicData,BinnedFare=res)
table(NBTitanicData$BinnedFare,NBTitanicData$Pclass)
#Age
hist(NBTitanicData$Age) #seems normally distributed
res <- bin(NBTitanicData$Age, nbins = 10, method = "content", na.omit = TRUE)
NBTitanicData<-cbind(NBTitanicData,BinnedAges=res)
table(NBTitanicData$BinnedAges,NBTitanicData$Sex)
NBTitanicData$Age<-NULL
NBTitanicData$Fare<-NULL 

#everything is now a factor/categorical data
str(NBTitanicData)  

trainNB<-NBTitanicData[idx,]
testNB<-NBTitanicData[-idx,]
nb <- e1071::naiveBayes(trainNB, trainNB$Survived)
nbP <- predict(nb, newdata=testNB[,-dependentColumnNumber], type = "class")
(nbAcc <- 1- mean(nbP != testNB$Survived))


#C5.0 - Decision Tree

library(C50)
cFiftyModel<- C5.0(train$Survived ~ ., data = train, trails=100)  
class(cFiftyModel)
summary(cFiftyModel)
#plot(cFiftyModel)
cFiftyModelPrediction<-predict(cFiftyModel,newdata=test[,-dependentColumnNumber])
(cFiftyModelAccuracy <- mean(cFiftyModelPrediction == test$Survived))  

#CART [Classification and Regression Tree]- Decision Tree

library(rpart)
library(rpart.plot)
library(RColorBrewer)
regressionTree <- rpart::rpart(Survived ~ ., data=train, method="class")
library(rattle)
fancyRpartPlot(regressionTree)
rpartPrediction <- predict(regressionTree, train, type = "class")
(CARTModelAccuracy <- mean(rpartPrediction == test$Survived))


#Performance of Random Forest
library(randomForest)
str(train)

#Removing the columns that has higher number of levels as I'm getting error
trainRF <- train[,c(-4,-9,-11)]

forest <- randomForest(Survived ~ ., data = trainRF, importance = TRUE, ntree = 2000)
varImpPlot(forest)
rfPrediction <- predict(forest, test[,-3], type = "class")
(rfAccuracy <- 1 - mean(rfPrediction != test$plc))

rfConfusionMat <- confusionMatrix(rfPrediction, test$Survived, dnn = c("Ceased", "Survived"))


#Performance of Conditional Inference Trees
library(partykit)

#Removing the columns that has higher number of levels as I'm getting error
trainCIT <- train[,c(-4,-9,-11)]

cTree <- ctree(Survived ~ ., data = trainCIT)
print(cTree)
plot(cTree, type = "simple")

cForest <- cforest(Survived ~ ., data = trainCIT)
cForestPrediction <- predict(cForest, newdata = test[,-14])
(cForestAccuracy <- 1 - mean(cForestPrediction != test$Survived))


#Only numerics for PCA
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

#plot to identify the number of components
plot(pca1)
screeplot(pca1, type = "line", main = "Scree Plot")
autoplot(pca1, data = y)

#Rotation of components
varimax(pca1$scores)

#Only numerics for PCA
str(titanicData$Survived)
y <- cbind(titanicData$PassengerId, titanicData$Age, titanicData$Fare, titanicData$Survived)
summary(y)
cor(y)
y <- data.frame(y)
colnames(y)[1:4] <- c("PassengerId", "Age", "Fare", "Survived")
y$Survived <- factor(y$Survived, levels = c(1,2), labels = c(0,1))

#Principle Component Analysis on Y
pca2 <- prcomp(y[,-4], scale. = F, center = F)
#Summary with show the cummulative proportion and Std. deviation. Squaring the Std.deviation
#will give us the eigen values. We will consider the components those who has the eigen 
#values greater than 1
summary(pca2)

PCA(y[,-4], scale.unit = TRUE, ncp = 2)
#plot to identify the number of components
plot(pca2)
screeplot(pca2, type = "line", main = "Scree Plot")
autoplot(pca2, data = y)

##############################################################################

