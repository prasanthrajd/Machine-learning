##################################################################################################################

newdata = read.csv("C:/Users/dprra/Desktop/Horse/results.csv")
summary(newdata)
str(newdata)


nearZeroVar(newdata, saveMetrics = TRUE)
library(caret)
newdata = newdata[,-24]
View(newdata)

### Cleaning the data frame
 newdata$X = as.numeric(newdata$X)
newdata$row = as.numeric(newdata$row)
# newdata$plc = as.numeric(newdata$plc)
newdata$horse = as.character(newdata$horse)
newdata$jockey = as.character(newdata$jockey)
newdata$trainer = as.character(newdata$trainer)
newdata$actualwt = as.numeric(newdata$actualwt)
newdata$declarwt = as.numeric(newdata$declarwt)
newdata$draw = as.numeric(newdata$draw)
newdata$lbw = as.numeric(newdata$lbw)
newdata$runningpos = as.numeric(newdata$runningpos)
newdata$finishtime = as.numeric(newdata$finishtime)
##newdata$date = as.Date(newdata$date)
newdata$raceno = as.numeric(newdata$raceno)
newdata$class = as.numeric(newdata$class)
newdata$distance = as.numeric(newdata$distance)
newdata$handicap = as.character(newdata$handicap)
newdata$stake = as.numeric(newdata$stake)

levels(newdata$venue)
newdata$going <- factor(newdata$going, levels = c('FAST', 'GOOD', 'GOOD TO FIRM', 'GOOD TO YIELDING', 'WET FAST', 'WET SLOW', 'YIELDING'), labels = c(0,1,2,3,4,5,6))
newdata$course <- factor(newdata$course, levels = c('ALL WEATHER TRACK', 'TURF - A COURSE', 'TURF - A+3 COURSE', 'TURF - B COURSE', 'TURF - B+2 COURSE', 'TURF - C COURSE', 'TURF - C+3 COURSE'), labels = c(0,1,2,3,4,5,6))
newdata$venue <- factor(newdata$venue, levels = c('Happy Valley', 'Sha Tin'), labels = c(0,1))
# newdata$date1 <- as.Date.numeric(newdata$date)
str(newdata)

sapply(newdata, function(x) sum(is.na(x)))

### Imputing PCA for the NA Values##########
##install.packages("missMDA")
library(missMDA)
miss <- newdata[,c(-3,-5,-6,-7,-15,-19,-20,-21,-23)]
str(miss)
missONCP = estim_ncpPCA(miss,ncp.min = 0, ncp.max = 5)
misingvalues = imputePCA(miss,ncp = missONCP$ncp)
complete_df = data.frame(misingvalues$completeObs)

newdata <- cbind(newdata[,c(3,5:7,15,19:21,23)], complete_df)

# newdata$X <- complete_df$X
# newdata$row <- complete_df$row
# newdata$horseno = complete_df$horseno
# newdata$actualwt = complete_df$actualwt
# newdata$declarwt = complete_df$declarwt
# newdata$draw = complete_df$draw
# newdata$lbw <- complete_df$lbw
# newdata$runningpos <- complete_df$runningpos
# newdata$finishtime <- complete_df$finishtime
# newdata$winodds = complete_df$winodds

newdata$date
summary(newdata$plc)
class(newdata$plc)

newdatatemp <- newdata
write.csv(newdatatemp,"horse_cleaned.csv")
newdatachar$plc <- as.factor(newdatachar$plc)
class(newdatatemp$plc)

newdatachar=newdatatemp %>% mutate_if(is.character, as.factor)
prop.table(table(newdatachar$plc))

##Plotting dependant  variable imbalance
hist(prop.table(table(newdatachar$plc)))
#write.csv(newdatachar, "imbalancetest.csv")
newdatachar = read.csv("imbalancetest.csv")

newdatachar <- newdata

##Class Imabalance manipulation
library(UBL)
str(newdatachar)
table(newdatachar$plc)
df <- newdatachar[,c(-2:-9)]

df <- SmoteClassif(plc ~ ., df, C.perc = "balance", repl = FALSE, k=2)
table(df$plc)
str(df)

library(dplyr)
idx <- createDataPartition(df$plc, p=0.75, list = FALSE)
train <- df[idx,]
test <- df[-idx,]




##Data Partition
#library(caret)
#library(dplyr)
#idx <- createDataPartition(newdatachar$plc, p=0.80, list = FALSE)
#train <- newdatachar[idx,]
#test <- newdatachar[-idx,]

str(train)
# forest <- randomForest(plc ~ ., data = train[-c(5,6,7,15,20)], importance = TRUE, ntree = 200)
forest <- randomForest(plc ~ ., data = train, importance = TRUE, ntree = 200)
varImpPlot(forest)
rfPrediction <- predict(forest, test$plc, type = "class")

(rfAccuracy <- 1 - mean(rfPrediction != test$Survived))

##Checking class imbalance

prop.table(table(newdatatemp))



##Data Partition
#library(caret)
#idx <- createDataPartition(newdata$plc, p=0.80, list = FALSE)
#train <- newdata[idx,]
#test <- newdata[-idx,]

library(randomForest)
str(train)

#Removing the columns that has higher number of levels as I'm getting error
trainRF <- train

forest <- randomForest(plc ~ ., data = trainRF, importance = TRUE, ntree = 2000)
varImpPlot(forest)
rfPrediction <- predict(forest, newdatachar = test[,-4], type = "class")

(rfAccuracy <- 1 - mean(rfPrediction != test$plc))


## KNN Model 

n<-sapply(newdata,function(x){is.numeric(x)})
numerics<-newdata[,n]
summary(numerics)
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
numericsNormal <- normalize(numerics)
summary(numericsNormal)
HorseDataKNN<-newdata[,!n]
HorseDataKNN<-cbind(HorseDataKNN,numericsNormal)

##install.packages("dummies")
library(dummies)
#handle even categorical data to represent as 1 and 0
#not taking dependent variable
tkNN<-dummy.data.frame(HorseDataKNN[,-3])

summary(tkNN)
y<- newdata$plc
index<-sample(1:length(y),0.2*length(y),replace=FALSE)
kNNTraining<-tkNN[-index,]
kNNTesting<-tkNN[index,]

DependantTrain<-HorseDataKNN$plc[-index]
DependantTesting<-HorseDataKNN$plc[index]

k2<-round(sqrt(dim(kNNTraining)[2])) #sqrt of number of attribute
k1 <- round(sqrt(dim(kNNTesting)[1])) #sqrt of number of instances
k3 <- 7 #a number between 3 and 10
library(class)

knn1 <- knn(train = kNNTraining, test = kNNTesting, cl = DependantTrain, k=k1)
knn2 <- knn(train = kNNTraining, test = kNNTesting, cl = DependantTrain, k=k2)
knn3 <- knn(train = kNNTraining, test = kNNTesting, cl = DependantTrain, k=k3)


#Accuracy
(knn1Acc <- 1- mean(knn1 != test$plc))
(knn2Acc <- 1- mean(knn2 != test$plc))
(knn3Acc <- 1- mean(knn3 != test$plc))

##################################################################################
#Hyperparameter Optimization using Deep Learning
library(h2o)
hidden_opt <- list(c(100, 100, 100, 100), c(200, 200, 200, 200), c(300, 300, 300, 300)) 
l1_opt <- c(1e-5,1e-7)
activations <- c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
hyper_params <- list(hidden = hidden_opt, l1 = l1_opt, activation=activations)
#hyper_params <- list(ntrees = c(1,300), learn_rate = c(0.1, 0.001))


h2o.init(ip = "localhost", port = 54321)
#train$plc <- factor(train$plc)
str(train)
h2otrain <- train
h2otrain <- as.h2o(h2otrain)
#test$Label <- as.integer(test$Label)
h2otest <- test
h2otest <- as.h2o(h2otest)

model_grid <- h2o.grid("deeplearning",
                       hyper_params = hyper_params,
                       x = c(2:23),  # column numbers for predictors
                       y = 1,   # column number for label
                       training_frame = h2otrain,
                       validation_frame = h2otest)

dlPerf <- c()

for (model_id in model_grid@model_ids){
  model <- h2o.getModel(model_id)
  pred <- h2o.predict(model, h2otest)
  pred <- as.data.frame(pred)
  #  results.DL <- ifelse(pred$predict > 0.5,1,0)
  dlPerformance <- 1 - mean(pred$predict != test$plc)
  dlPerf <- rbind(dlPerf, dlPerformance)
}
#The best accuracy
(bestDL <- max(dlPerf))


