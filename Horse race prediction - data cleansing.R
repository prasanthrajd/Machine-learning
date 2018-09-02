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

##Level encoding
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