library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

crm_churn = read.csv(choose.files())
str(crm_churn)

#Dependent variable is CHURN

#Finding the null values
sapply(crm_churn, function(x) sum(is.na(x)))

#removing the null value rows

crm_churn = na.omit(crm_churn)

#Factorising the columns which has "no internet service" as factor level to "No"

levels(crm_churn$OnlineSecurity)[levels(crm_churn$OnlineSecurity)=="No internet service"]="No"
levels(crm_churn$OnlineBackup)[levels(crm_churn$OnlineBackup)=="No internet service"]="No"
levels(crm_churn$DeviceProtection)[levels(crm_churn$DeviceProtection)=="No internet service"]="No"
levels(crm_churn$TechSupport)[levels(crm_churn$TechSupport)=="No internet service"]="No"
levels(crm_churn$StreamingTV)[levels(crm_churn$StreamingTV)=="No internet service"]="No"
levels(crm_churn$StreamingMovies)[levels(crm_churn$StreamingMovies)=="No internet service"]="No"
levels(crm_churn$MultipleLines)[levels(crm_churn$MultipleLines)=="No internet service"]="No"

#Finding the min and max of month column
min(crm_churn$tenure)
max(crm_churn$tenure)

tenure_grp = function(tenure_grp){
  if (tenure_grp>=0 & tenure_grp<=12)
  {return('0-12')}
  else if (tenure_grp >12 & tenure_grp <=24) {
    return('12-24')}
  else if (tenure_grp >24 & tenure_grp <=48) {
    return('24-48')}
  else if (tenure_grp >48 & tenure_grp <=60) {
    return('48-60')}
  else if (tenure_grp >60 & tenure_grp <=72) {
    return('60-72')}
  }

crm_churn$tenure = sapply(crm_churn$tenure,tenure_grp)
crm_churn$tenure=as.factor(crm_churn$tenure)
str(crm_churn)

crm_churn$SeniorCitizen = as.factor(mapvalues(crm_churn$SeniorCitizen, from=c("0","1"), to=c("No","Yes")))

#Looking into columns that are not required
nearZeroVar(crm_churn, saveMetrics = TRUE)

crm_churn$tenure = NULL
crm_churn$customerID = NULL

numeric = sapply(crm_churn, is.numeric)
cor_matrix = cor(crm_churn[,numeric])
corrplot(cor_matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#|Removing any of the highly correlated data
crm_churn$TotalCharges = NULL

p1 <- ggplot(crm_churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(crm_churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(crm_churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(crm_churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol=2)

p5 <- ggplot(crm_churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(crm_churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(crm_churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(crm_churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(crm_churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(crm_churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(crm_churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(crm_churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(crm_churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(crm_churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(crm_churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(crm_churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(p13, p14, p15, p16, ncol=2)

sample = createDataPartition(crm_churn$Churn,p=.75, list = FALSE)
set.seed(17132631)
training = crm_churn[sample,]
testing = crm_churn[-sample,]

LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

anova(LogModel, test = "Chisq")

crm_churn$TotalCharges = NULL

rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

varImpPlot(rfModel)
summary(crm_churn)

tree <- ctree(Churn~Contract+tenure+PaperlessBilling, training)
plot(tree)

cTree <- ctree(Churn ~., data=training)
cTree_Prediction = predict(cTree, newdata = testing)
(cTreeAccuracy <- 1 - mean(cTree_Prediction != testing$Churn))

plot(cTree)

write.csv(training,file = "C:/Users/dprra/Documents/crm_testing.csv")

cForest <- cforest(Churn ~., data=training, controls=party::cforest_unbiased(ntree=2000, mtry=3))
cForestPrediction <- predict(cForest, newdata = testing)
(cForestAccuracy <- 1 - mean(cForestPrediction != testing$Churn))

tb1 = table(crm_churn$gender, crm_churn$Churn)
tb1

chisq.test(tb1)
anova(crm_churn, test = "Chisq")


names(getModelInfo())
library(party)
tunegrid <- expand.grid(.mtry=5)
control <- trainControl(method = "repeatedcv",number = 10, repeats = 3, search = "grid")

CFmodel <- train(Churn ~ .,data = training, method = "cforest", 
                 metric="Accuracy",tuneGrid = tunegrid, 
                 controls=cforest_unbiased(ntree=250))

cFpp <- predict(CFmodel,newdata = testing[,-18] )
(cFAccp <- 1-mean(cFpp != testing$Churn))
