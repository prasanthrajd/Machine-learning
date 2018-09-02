mydata = read.csv(file.choose())
library(caret)

#Data Cleaning
mean(mydata$Age)
install.packages("caret")
mydata$Age[which(is.na(mydata$Age))] = 29.70
summary(mydata)
mydata$Survived = factor(mydata$Survived,levels = c(0,1), labels = c("No","Yes"))
table(mydata$Ticket)
summary(mydata)

nearZeroVar(mydata, saveMetrics = TRUE)

mydata$Pclass = factor(mydata$Pclass,levels = c(1:3), labels = c("First","second","third"))
replace(mydata$Cabin,"NULL","unknown")
summary(mydata)
mydata$Cabin[mydata$Cabin==" "] = NA
summary(mydata)
mydata$Name = trimws(mydata$Name,which = "right")
mydata = mydata [, -11]
mydata$Embarked = factor(mydata$Embarked, levels = c("C","Q","S"), labels = c("Cherbourg", "Queenstown","Southampton"))

#C|lass Imbalance
table(mydata$Survived)
prop.table(table(mydata$Survived))

#Making tRain and test dataset
library(caret)
split = createDataPartition(mydata$Survived,p=0.80, list = FALSE)
train = mydata[split, ]
test = mydata[-split, ]
summary(train)
summary(test)

#making a draft model
perishmodel = rep("No",length(test))

table(test,perishmodel)
summary(mydata$Ticket)
