d$Weekday <- weekdays(d$date_filed)
d$Month <- months(d$date_filed)

library(lubridate)
d$Weekday <- wday(d$date_filed)
d$Month   <- month(d$date_filed)

table(d$Weekday)
table(d$Month)


p <- select(d,case_type,court_name,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NumHearing,
            AvgDaysBetweenHearing,NewStatus,Weekday,Month)

###
# filter p
###

q <- filter(p,NewStatus != "Pending")
table(q$NewStatus)

q1 <- na.omit(q) 
dim(q1)
table(q1$NewStatus)
summary(q1)

###
# first option - convert to factors
###

str(q1)
q1[sapply(q1, is.character)] <- lapply(q1[sapply(q1, is.character)], 
                                             as.factor)

str(q1)
###
# option 2 - convert all to numeric
###

str(q1)
q1$case_type <- as.numeric(as.factor(q1$case_type)) - 1
q1$court_name <- as.numeric(as.factor(q1$court_name)) - 1
q1$CivilOrCriminal <- as.numeric(as.factor(q1$CivilOrCriminal)) - 1
q1$NewStatus <- as.numeric(as.factor(q1$NewStatus)) - 1
str(q1)

###
# partition the data
###

# for xgb, 2nd try...
q1$case_type <- NULL
q1$CivilOrCriminal <- NULL
q1$Weekday <- NULL
# rmse increases...!!! dont do

# take log and check ...

q1$AgeinDays <- log(q1$AgeinDays)
q1$DaysToFirstHearing <- log(q1$DaysToFirstHearing)
q1$NumHearing <- log(q1$NumHearing)
q1$AvgDaysBetweenHearing <- log(q1$AvgDaysBetweenHearing)
# rmse increases...!!! dont do
# a <- exp(xb1.yhat)
# b <- exp(test$AgeinDays)
# sqrt(mean((a-b)^2)) # rmse is 216.6735

str(q1)
library(caret)
set.seed(100)
inTrain <- createDataPartition(q1$NewStatus, p = .8)[[1]]
train <- q1[ inTrain, ]
test  <- q1[-inTrain, ]


###
#
###
# http://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/

# Baseline model - predict the mean of the training data
best.guess <- mean(train$AgeinDays) 
best.guess


# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-test$AgeinDays)^2))
RMSE.baseline
exp(RMSE.baseline)

MAE.baseline <- mean(abs(best.guess-test$AgeinDays))
MAE.baseline



###
#
###
lm.fit1 <- lm(AgeinDays ~ NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit1)

lm.fit2 <- lm(AgeinDays ~ court_name + CivilOrCriminal + NewStatus + Weekday + Month + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit2)

# remove case_type and CivilOrCriminal
lm.fit3 <- lm(AgeinDays ~ court_name + NewStatus + Weekday + Month + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit3)

lm.fit1 <- lm(AgeinDays ~ .,data=train)
summary(lm.fit1)

# Multicollinearity can be detected using a statistic called the variance inflation factor
library(car)
vif(lm.fit1)
sqrt(vif(lm.fit1)) > 2 # problem?
plot(lm.fit1)

plot(predict (lm.fit3), residuals (lm.fit3))
plot(predict (lm.fit3), rstudent (lm.fit3))
plot(hatvalues (lm.fit3))
which.max (hatvalues (lm.fit3))

lm.fit4 <- lm(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing + Weekday + Month ,data=train)
summary(lm.fit4)

#lm.fit5 <- lm(AgeinDays ~ NewStatus + log(DaysToFirstHearing) + NumHearing + AvgDaysBetweenHearing,data=train)
lm.fit5 <- lm(AgeinDays ~ NewStatus + log(DaysToFirstHearing) + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit5)


plot(lm.fit1)
plot(predict (lm.fit1), residuals (lm.fit1))
plot(hatvalues (lm.fit1))
which.max (hatvalues (lm.fit1))


lm1.yhat <- predict(lm.fit1,test, se.fit = FALSE)
lm2.yhat <- predict(lm.fit2,test, se.fit = FALSE)
lm3.yhat <- predict(lm.fit3,test, se.fit = FALSE)
lm4.yhat <- predict(lm.fit4,test, se.fit = FALSE)
lm5.yhat <- predict(lm.fit5,test, se.fit = FALSE)

RMSE.lm1 <- sqrt(mean((lm1.yhat-test$AgeinDays)^2))
RMSE.lm1

MAE.lm1 <- mean(abs(lm1.yhat-test$AgeinDays))
MAE.lm1

RMSE.lm2 <- sqrt(mean((lm2.yhat-test$AgeinDays)^2))
MAE.lm2 <- mean(abs(lm2.yhat-test$AgeinDays))

RMSE.lm3 <- sqrt(mean((lm3.yhat-test$AgeinDays)^2))
MAE.lm3 <- mean(abs(lm3.yhat-test$AgeinDays))

RMSE.lm4 <- sqrt(mean((lm4.yhat-test$AgeinDays)^2))
MAE.lm4 <- mean(abs(lm4.yhat-test$AgeinDays))

RMSE.lm5 <- sqrt(mean((lm5.yhat-test$AgeinDays)^2))
MAE.lm5 <- mean(abs(lm5.yhat-test$AgeinDays))

# summarize accuracy - calculate mean squared error
# nned to divide by no of rows...and then take sqrt
rmse1 <- sqrt(sum((test$AgeinDays - lm1.yhat)^2)/nrow(test)) ### Check the division....
rmse1
rmse2 <- sqrt(mean((test$AgeinDays - lm1.yhat)^2))
rmse2




# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Calculate error
error <- test$AgeinDays - lm1.yhat

# Example of invocation of functions
rmse(error)
mae(error)

# Calculate error
error2 <- test$AgeinDays - lm2.yhat
# Example of invocation of functions
rmse(error2)
mae(error2)

###
#
###
# http://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/
library(rpart)
#install.packages("rattle")
library(rattle)
#install.packages("rpart.plot")
library(rpart.plot)	
rt.fit1 <- rpart(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
rt.fit2 <- rpart(AgeinDays ~ court_name + NewStatus + Weekday + Month + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)

rt.yhat1 <- predict(rt.fit1,test) 
rt.yhat2 <- predict(rt.fit2,test) 

RMSE.rt <- sqrt(mean((rt.yhat1-test$AgeinDays)^2))
RMSE.rt

MAE.rt <- mean(abs(rt.yhat1-test$AgeinDays))
MAE.rt

RMSE.rt1 <- sqrt(mean((rt.yhat2-test$AgeinDays)^2))
MAE.rt1 <- mean(abs(rt.yhat2-test$AgeinDays))
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html

fancyRpartPlot(rt.fit1)
varImp(rt.fit1)
varImp(rt.fit2)
plot(rt.fit1)
text(rt.fit1)

fancyRpartPlot(rt.fit2)
plot(rt.fit2)
text(rt.fit2)
prp(rt.fit2) #, main="type = 4, extra = 6", type=4, extra=6, faclen=0)

# prune the tree
printcp(rt)
min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
min.xerror


###
#
###
# http://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/
# https://heuristically.wordpress.com/2013/07/12/calculate-rmse-and-mae-in-r-and-sas/
library(randomForest)
set.seed(123)

rf1 <- randomForest(AgeinDays ~ court_name + NewStatus + Weekday + Month + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,
                    data=train, importance = TRUE, ntree=1000)

which.min(rf1$mse)
# 24 trees are enough :)

plot(rf1)
imp <- as.data.frame(sort(importance(rf1)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
varImp(rf1)
rf1.yhat <- predict(rf1,test)
RMSE.rf1 <- sqrt(mean((rf1.yhat-test$AgeinDays)^2))
RMSE.rf1

MAE.rf1 <- mean(abs(rf1.yhat-test$AgeinDays))
MAE.rf1

varImpPlot(rf1)

rf.fit2 <- randomForest(AgeinDays ~ court_name + NewStatus + Month + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,
                    data=train, importance = TRUE, ntree=1000)

rf.yhat2 <- predict(rf.fit2,test)
RMSE.rf2 <- sqrt(mean((rf.yhat2-test$AgeinDays)^2))
RMSE.rf2

MAE.rf2 <- mean(abs(rf.yhat2-test$AgeinDays))
MAE.rf2
varImpPlot(rf.fit2)
varImp(rf.fit2)

rf.fit2 <- randomForest(AgeinDays ~ court_name + NewStatus + Month + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,
                        data=train, importance = TRUE, ntree=1000)

rf.yhat2 <- predict(rf.fit2,test)
RMSE.rf2 <- sqrt(mean((rf.yhat2-test$AgeinDays)^2))
RMSE.rf2

MAE.rf2 <- mean(abs(rf.yhat2-test$AgeinDays))
MAE.rf2

###
#
###
library(xgboost)

str(train)

feature.names <- c("case_type","court_name","CivilOrCriminal","")
t1 <- train[,-AgeinDays]
t1 <- subset(train, select = -AgeinDays)
str(t1)

# http://wiselily.com/2015/07/12/xgboost-data-mining-example-1/

train$case_type <- NULL
test$case_type <- NULL

xb.cv <- xgb.cv(data        = data.matrix(subset(train, select = -AgeinDays)),
                label       = train$AgeinDays,
                nfold=5, nrounds=200, eta=0.02,
                max_depth=5, subsample=0.6, colsample_bytree=0.85,min_child_weight=1,
                objective   = "reg:linear",
                eval_metric = "rmse")

plot(xb.cv)
xb.cv
print(xb.cv)
summary(xb.cv)
xb.fit1 <- xgboost(data        = data.matrix(subset(train, select = -AgeinDays)),
                   label       = train$AgeinDays,
                   nrounds     = 200,
                   objective   = "reg:linear",
                   eval_metric = "rmse")

xb1.yhat <- predict(xb.fit1, data.matrix(subset(test, select = -AgeinDays)))
RMSE.xb <- sqrt(mean((xb1.yhat-test$AgeinDays)^2))
RMSE.xb

MAE.xb <- mean(abs(xb1.yhat-test$AgeinDays))
MAE.xb

###
# feature importanc
###

# get the trained model
model = xgb.dump(xb.fit1, with.stats=TRUE)
# get the feature real names
names = dimnames(train)[[2]]
names
names = dimnames(subset(train, select = -AgeinDays))[[2]]
names

# compute feature importance matrix
importance_matrix = xgb.importance(names, model=xb.fit1)

# plot
gp = xgb.plot.importance(importance_matrix)
print(gp) 

#xgb.plot.tree(names, model = xb.fit1)

# 2
xb.fit2 <- xgboost(data        = data.matrix(subset(train, select = -AgeinDays)),
                   label       = train$AgeinDays,
                   nfold=5, nrounds=200, eta=0.02,
                   max_depth=5, subsample=0.6, colsample_bytree=0.85,min_child_weight=1,
                   objective   = "reg:linear",
                   eval_metric = "rmse")


xb2.yhat <- predict(xb.fit2, data.matrix(subset(test, select = -AgeinDays)))
RMSE.xb2 <- sqrt(mean((xb2.yhat-test$AgeinDays)^2))
RMSE.xb2

MAE.xb2 <- mean(abs(xb2.yhat-test$AgeinDays))
MAE.xb2

xb.fit3 <- xgboost(data        = data.matrix(subset(train, select = -AgeinDays)),
                   label       = train$AgeinDays,
                   nrounds     = 1200,
                   objective   = "reg:linear",
                   eval_metric = "rmse")

xb3.yhat <- predict(xb.fit3, data.matrix(subset(test, select = -AgeinDays)))
RMSE.xb3 <- sqrt(mean((xb3.yhat-test$AgeinDays)^2))
RMSE.xb3

MAE.xb3 <- mean(abs(xb3.yhat-test$AgeinDays))
MAE.xb3

###
# Create a df
###
accuracy <- data.frame(Method = c("Baseline","Linear Regression1","Linear Regression2","Linear Regression3","Rpart","Rpart2","Random forest"),
                       RMSE   = c(RMSE.baseline,RMSE.lm1,RMSE.lm2,RMSE.lm3,RMSE.rt,RMSE.rt1,RMSE.rf1),
                       MAE    = c(MAE.baseline,MAE.lm1,MAE.lm2,MAE.lm3,MAE.rt,MAE.rt1,MAE.rf1)) 

accuracy <- data.frame(Method = c("Baseline","Linear Regression1","Linear Regression2","Linear Regression3","Rpart","Random forest","X Boost1","X Boost2","X Boost3"),
                       RMSE   = c(RMSE.baseline,RMSE.lm1,RMSE.lm2,RMSE.lm3,RMSE.rt,RMSE.rf1,RMSE.xb,RMSE.xb2,RMSE.xb3),
                       MAE    = c(MAE.baseline,MAE.lm1,MAE.lm2,MAE.lm3,MAE.rt,MAE.rf1,MAE.xb,MAE.xb2,MAE.xb3)) 

accuracy
accuracy1 <- accuracy 
accuracy1
RMSE.xb
RMSE.lm4
RMSE.lm5

miniAccuracy <- data.frame(Method = c("Baseline","Linear Regression1","XGB"),
                       RMSE   = c(RMSE.baseline,RMSE.lm1,RMSE.xb),
                       MAE    = c(MAE.baseline,MAE.lm1,MAE.xb)) 

miniAccuracy

miniAccuracy2 <- data.frame(Method = c("Baseline","Linear Regression1","XGB"),
                           RMSE   = c(RMSE.baseline,RMSE.lm1,RMSE.xb),
                           MAE    = c(MAE.baseline,MAE.lm1,MAE.xb)) 
miniAccuracy2

###
#
###
library(plotly)
plot_ly(test, x = AgeinDays, y = xb1.yhat, text = paste("Case Type: ", case_type),
        mode = "markers", color = court_name, size = DaysToFirstHearing)

