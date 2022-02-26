                               ## cleaned up code ##  
install.packages('quantreg')
install.packages('SparseM')
install.packages('MatrixModels')
install.packages('devtools')
install_github("vqv/ggbiplot")
install.packages('car')

library('car')
library(devtools)
library(ggbiplot)
library('quantreg')
library(dplyr)

data <- read.csv(file = '/Users/jpate/Dropbox/Jacob Pate/DraftedRbsTest.csv')
head(data)

#there were two spacer columns in the csv file, and when imported to R they became
#X and X.1 so we're dropping them

data2 = select(data, -c(X,X.1))


#replacing NA values with column mean

data2$X40yd[is.na(data2$X40yd)] <- mean(data2$X40yd,na.rm=TRUE)
data2$Vertical[is.na(data2$Vertical)] <- mean(data2$Vertical,na.rm=TRUE)
data2$Bench[is.na(data2$Bench)] <- mean(data2$Bench,na.rm=TRUE)
data2$Broad.Jump[is.na(data2$Broad.Jump)] <- mean(data2$Broad.Jump,na.rm=TRUE)
data2$X3Cone[is.na(data2$X3Cone)] <- mean(data2$X3Cone,na.rm=TRUE)
data2$Shuttle[is.na(data2$Shuttle)] <- mean(data2$Shuttle,na.rm=TRUE)
summary(data2)


#for ease, we put all predictors in a single command (so that's all of the ncaa stats
# and all of the combine stats)

all.predictors <- c(5:9, 15:22)
all.predictors <- as.vector(all.predictors)
is.vector(all.predictors)
#this returns FALSE - so I'm not sure how to fix the error message from line 63/64


#now we train the full model with all predictors

season <- 2020

y_train <- data2$NFL.Rushing.Yards[which(data2$Year <= (season-1))]
x_train <- data2[which(data2$Year <= (season-1)), all.predictors]
x_train = as.matrix(x_train)

y_test <- data2$NFL.Rushing.Yards[which(data2$Year == (season))]
x_test <- data2[which(data2$Year == (season)), all.predictors]

output1 <- lm(y_train~x_train)
output1$coefficients
correlation <- cor(x_train)

#the following correlations were of interest to me
#games played and rushing yards: 0.57
#games played and rushing attempts: 0.54
#games played and touchdowns: 0.48
#rushing yards and rushing attempts: 0.933
#rushing attempts and touchdowns: 0.799
#rushing yards and touchdowns: 0.846
#height and weight: 0.600
#weight and 40yd dash: 0.550
#vertical jump and broad jump: 0.688
#cone and shuttle: 0.590

summary(output1)


#now we rename variables to more easily select individual features

ncaa.gp <-  x_train[,1]
ncaa.attempts <- x_train[,2]
ncaa.rushing <- x_train[,3]
ncaa.ypc <- x_train[,4]
ncaa.td<- x_train[,5]
combine.height <- x_train[,6]
combine.weight <- x_train[,7]
combine.40yd <- x_train[,8]
combine.vert <- x_train[,9]
combine.bench <- x_train[,10]
combine.broad <- x_train[,11]
combine.cone <- x_train[,12]
combine.shuttle <- x_train[,13]


#now we train the full qr model with all predictors

quantOutput <- rq(y_train~x_train, tau=0.5)
coef(quantOutput)
summary(quantOutput)