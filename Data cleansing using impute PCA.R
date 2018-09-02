mydata = read.csv(choose.files())
summary(mydata)
str(mydata)
mydata$Number.of.Occupants
replace(mydata$Number.of.Occupants," ","NA")
replace(mydata)


#imputing PCA
install.packages("missMDA")
library(missMDA)
miss <- mydata[,c(numerical values column numbers)]
missONCP = estim_ncpPCA(miss,ncp.min = 0, ncp.max = number of columns mentioend in "miss")
misingvalues = imputePCA(miss,ncp = missONCP$ncp)
mydata$Number.of.Occupants = round(missfinal$Number.of.Occupants)


