################################################################
#                                                              #
#                  CA 1 - ADM - x17132631                      #
#                                                              #
################################################################
#setwd("C:\Users\dprra\Desktop\CA 1") #change this to where you downloaded the .csv
ca_dataset <- read.csv("CA_Dataset.csv", stringsAsFactors = F, na.strings = "")
# remove columns that don't carry any predictive value whatsoever
ca_dataset <- ca_dataset[,-c(2:5)] 
# ok, now we need to make a dataset unique to you
set.seed(17132631) # <-- put your student number here WITHOUT the x!! Leave off a starting zero if you have one
# e.g.: set.seed(16245678)

index <- sample(1:nrow(ca_dataset), 1260, replace=FALSE)
my_ca_dataset <- ca_dataset[index, ] 

#Unfortunately, due to a technical error, some columns of the data were lost :(
dependent <- my_ca_dataset$Target
my_ca_dataset$Target <- NULL

easier <- my_ca_dataset[, c(4,6,8,15,16,18,19,20,36)]
harder <- my_ca_dataset[, -c(4,6,8,15,16,18,19,20,36)]

index2 <- sample(1:(ncol(easier)), 5, replace=FALSE)
easier <- easier[, index2]
index2 <- sample(1:(ncol(harder)), 35, replace=FALSE)
harder <- harder[, index2]
my_ca_dataset <- cbind(dependent, easier, harder)

names(my_ca_dataset)[1] <- "Target"
print(paste("I have:", names(my_ca_dataset)))

v <- round(runif(ncol(my_ca_dataset), min=1, max=6))
if (max(v) > 4) {
  v <- cut(v, breaks = c(0,4,max(v)), labels = c("a","b"))
} else {
  v <- cut(v, breaks = c(0,max(v)-1,max(v)), labels = c("a","b")) 
}
Pna <- runif(1000, min=0, max=0.13)
Pna <- Pna - .03
Pna[Pna < 0] <- 0
v[1] <- "a"

for (i in 1:length(v)) {
  if (v[i] == "b") {
    nadex <- sample(1:nrow(my_ca_dataset), nrow(my_ca_dataset) * Pna[sample(1:length(Pna), 1, replace=FALSE)], replace=FALSE)
    my_ca_dataset[nadex, i] <- NA
    v[i] <- "a"
  }
}

Pna <- runif(1000, min=0, max=0.13)
Pna <- Pna - .03
Pna[Pna < 0] <- 0
v[1] <- "a"

for (i in 1:length(v)) {
  if (v[i] == "b") {
    nadex <- sample(1:nrow(my_ca_dataset), nrow(my_ca_dataset) * Pna[sample(1:length(Pna), 1, replace=FALSE)], replace=FALSE)
    my_ca_dataset[nadex, i] <- NA
    v[i] <- "a"
  }
}

Pna <- runif(1000, min=0, max=0.13)
Pna <- Pna - .03
Pna[Pna < 0] <- 0
v[1] <- "a"

for (i in 1:length(v)) {
  if (v[i] == "b") {
    nadex <- sample(1:nrow(my_ca_dataset), nrow(my_ca_dataset) * Pna[sample(1:length(Pna), 1, replace=FALSE)], replace=FALSE)
    my_ca_dataset[nadex, i] <- NA
    v[i] <- "a"
  }
}

rm(v)
rm(Pna)
rm(ca_dataset)
rm(index)
rm(index2)
rm(i)
rm(nadex)
rm(dependent)
rm(easier)
rm(harder)


write.csv(my_ca_dataset, file="my_ca_dataset.csv", row.names = F, na = " ")

my_ca_dataset <- read.csv("my_ca_dataset.csv", stringsAsFactors = T, na.strings = " ")

summary(my_ca_dataset)

############################################################################################
#F1:

#Removing NA:
my_ca_dataset=na.omit(my_ca_dataset)

#Removing the %
str(my_ca_dataset)
my_ca_dataset$Percent.White=gsub("%","",my_ca_dataset$Percent.White)
str(my_ca_dataset)
my_ca_dataset$Collaborative.Teachers..=gsub("%","",my_ca_dataset$Collaborative.Teachers..)
str(my_ca_dataset)
my_ca_dataset$Percent.Hispanic=gsub("%","",my_ca_dataset$Percent.Hispanic)
str(my_ca_dataset)

#Removing the $
my_ca_dataset$School.Income.Estimate = gsub("$","",my_ca_dataset$School.Income.Estimate)

#Running correlation between the numerical variables

n <- sapply(my_ca_dataset,function(x) {is.numeric(x)})
n
numerics <- my_ca_dataset[,n]
cor(numerics)

#Removing correlated variables
my_ca_dataset=my_ca_dataset[-7]
my_ca_dataset=my_ca_dataset[-12]
my_ca_dataset=my_ca_dataset[-13]
my_ca_dataset=my_ca_dataset[-30]
my_ca_dataset=my_ca_dataset[-25]
my_ca_dataset=my_ca_dataset[-18]
my_ca_dataset=my_ca_dataset[-41]
my_ca_dataset=my_ca_dataset[-40]
my_ca_dataset=my_ca_dataset[-23]
summary(my_ca_dataset)
str(my_ca_dataset)

#R|emoving zero variance
library(caret)
nearZeroVar(my_ca_dataset,saveMetrics = TRUE)

#Removing useless data:
my_ca_dataset=my_ca_dataset[-8]
my_ca_dataset=my_ca_dataset[-9]
my_ca_dataset=my_ca_dataset[-13]
my_ca_dataset=my_ca_dataset[-14]
my_ca_dataset=my_ca_dataset[-15]
my_ca_dataset=my_ca_dataset[-17]
my_ca_dataset=my_ca_dataset[-26]
my_ca_dataset=my_ca_dataset[-27]
my_ca_dataset=my_ca_dataset[-11]
#Removing space and $
str(my_ca_dataset)
my_ca_dataset$Percent.White = as.numeric(my_ca_dataset$Percent.White)
my_ca_dataset=na.omit(my_ca_dataset)
my_ca_dataset$Collaborative.Teachers..=as.numeric(my_ca_dataset$Collaborative.Teachers..)
my_ca_dataset$Percent.Hispanic=as.numeric(my_ca_dataset$Percent.Hispanic)


#############################################################################################################
#Imputing Data:using FAMD
#F2:

library(caret)
nearZeroVar(my_ca_dataset,saveMetrics = TRUE)
my_ca_dataset=my_ca_dataset[-7]
my_ca_dataset=my_ca_dataset[-11]
my_ca_dataset=my_ca_dataset[-12]
my_ca_dataset=my_ca_dataset[-17]
my_ca_dataset=my_ca_dataset[-20]
my_ca_dataset=my_ca_dataset[-21]

summary(my_ca_dataset)

#FAMD
#install.packages("FactoMineR")
library(FactoMineR)

res.famd <- FAMD(my_ca_dataset, graph=FALSE)
summary(res.famd)
#ctr - how much the dimension contributes 
#cos2 - if its close to 1, then the sample is well projected by the dimension(quality of representation)


#to show percentage of variances explained by each principal component
eig.val <- res.famd$eig
barplot(eig.val[, 1], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")

#Add connected line segments  to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

#Graph of individuals
plot(res.famd, choix = "ind")


#correlation between variables(quantitive and qualitative) with principal dimensions
#
plot(res.famd, choix = "var")

#Correlation circle of quantitative variables
plot(res.famd, choix = "quanti")

#end of FAMD

#################################################################################################
#F3:
#Process and converitng variables:
summary(my_ca_dataset)
my_ca_dataset$Percent.White = as.numeric(my_ca_dataset$Percent.White)
my_ca_dataset=na.omit(my_ca_dataset)
my_ca_dataset$Collaborative.Teachers..=as.numeric(my_ca_dataset$Collaborative.Teachers..)
my_ca_dataset$Percent.Hispanic=as.numeric(my_ca_dataset$Percent.Hispanic)

###################################################################################################
#F4:
#DataPartition

#Checking the imbalance:
prop.table(table(my_ca_dataset$Target))

sample <- createDataPartition(my_ca_dataset$Target,p=0.75,list=FALSE)
training2 <- my_ca_dataset[sample,]
testing2 <- my_ca_dataset[-sample,]
#####################################################################################################

#I1: 
#Benchmark:

my_ca_dataset=my_ca_dataset[-15]
my_ca_dataset=my_ca_dataset[-3]
my_ca_dataset=my_ca_dataset[-5]

sample <- createDataPartition(my_ca_dataset$Target,p=0.75,list=FALSE)
training2 <- my_ca_dataset[sample,]
testing2 <- my_ca_dataset[-sample,]

str(testing2$Target) # remain (High)
table(testing2$Target)
b1 <- rep("High", dim(testing2)[1])
(accuracyB1 <- 1 - mean(b1 != testing2$Target))



str(training2)
#Creating a random forest sample
library(randomForest)
forest <- randomForest(Target ~ ., data = training2, importance= TRUE, ntree = 2000)
#chk wch r important variables
varImpPlot(forest)

#chkn accuracy (not part of this qsn)
RFpred <- predict(forest,testing2[,-1],type = "class")
(RFacc <- 1-mean(RFpred != testing2$Target))

#
Rfc <- predict(forest, testing2, type = "class")
caret::confusionMatrix(RFpred, testing2$Attrition, positive = "Yes")

#############################################################################################
#B1:

summary(my_ca_dataset)

sample <- createDataPartition(my_ca_dataset$Target,p=0.75,list=FALSE)
training2 <- my_ca_dataset[sample,]
testing2 <- my_ca_dataset[-sample,]
###############################################################################################

#B2:
boxplot(subset(my_ca_dataset,select = c(1,4,8)))
spineplot(my_ca_dataset$Target ~ my_ca_dataset$Strong.Family.Community.Ties.Rating)
###################################################################################################
#A3:
tunegrid <- expand.grid(.mtry=5)
control <- trainControl(method = "repeatedcv",number = 10, repeats = 3, search = "grid")

CFmodel <- train(Target ~ .,data = training2, method = "cforest", 
                 metric="Accuracy",tuneGrid = tunegrid, 
                 controls=cforest_unbiased(ntree=250))

cFpp <- predict(CFmodel,newdata = testing2[,-1] )
(cFAccp <- 1-mean(cFpp != testing2$Target))
(cFAccp - cFAcc) 

#Random forest acuracy
RFmodel <- train(Target ~., data = training2, method = "rf",
                 metric="Accuracy",tuneGrid = tunegrid,
                 trControl = control)

RFppred <- predict(RFmodel,testing2[,-1])
(RFaccp <- 1-mean(RFppred != testing2$Target))
(RFaccp - RFacc)
####################################################################################################
str(my_ca_dataset)
my_ca_dataset=na.omit(my_ca_dataset)
str(my_ca_dataset)
