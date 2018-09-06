
###
#
###


disp$NewStatus <- NULL

#disp <- na.omit(disp) 
dim(disp)
PlotMissing(disp)

str(disp)
###
# For Prediction Convert to numeric
###
#disp$NumHearingImputed <- as.numeric(disp$NumHearingImputed)
#disp$AvgDaysBetweenHearingImputed <- as.numeric(disp$AvgDaysBetweenHearingImputed)
str(disp)

###
# 01 - data matrix -> dont do..do the second one 
###
disp <- as.data.frame(data.matrix(disp))
str(disp)

###
# 02 - one hot encoding
###

# one-hot encoding of the factor variables
# leave out the intercept column

sparseDisp <- as.data.frame(model.matrix( ~ . + 0, data = disp))

View(sparseDisp)
## partition into  train and test
## 80% of the sample size
smp_size <- floor(0.80 * nrow(sparseDisp))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(sparseDisp)), size = smp_size)

train <- sparseDisp[train_ind, ]
test <- sparseDisp[-train_ind, ]

### or ###

smp_size <- floor(0.80 * nrow(disp))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(disp)), size = smp_size)

train <- disp[train_ind, ]
test <- disp[-train_ind, ]


dim(train)
dim(test)

###############################################################
#######################
# Baseline model - predict the mean of the training data
best.guess <- mean(train$AgeinDays) 
best.guess


# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-test$AgeinDays)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$AgeinDays))
MAE.baseline

###
#
###
lm.fit1 <- lm(AgeinDays ~ .,data=train)
summary(lm.fit1)

# Multicollinearity can be detected using a statistic called the variance inflation factor
library(car)
vif(lm.fit1)
sqrt(vif(lm.fit1)) > 2 # problem?
plot(lm.fit1)

plot(predict (lm.fit1), residuals (lm.fit1)) # not OK ????
plot(predict (lm.fit1), rstudent (lm.fit1))
plot(hatvalues (lm.fit1))
which.max (hatvalues (lm.fit1))
# rse
summary(lm.fit1)$sigma

lm1.yhat <- predict(lm.fit1,test, se.fit = FALSE)

RMSE.lm1 <- sqrt(mean((lm1.yhat-test$AgeinDays)^2))
RMSE.lm1

MAE.lm1 <- mean(abs(lm1.yhat-test$AgeinDays))
MAE.lm1

###
#
###
library(rpart)
#install.packages("rattle")
library(rattle)
#install.packages("rpart.plot")
library(rpart.plot)	
rt.fit1 <- rpart(AgeinDays ~ .,data=train)

rt.yhat1 <- predict(rt.fit1,test) 


RMSE.rt <- sqrt(mean((rt.yhat1-test$AgeinDays)^2))
RMSE.rt

MAE.rt <- mean(abs(rt.yhat1-test$AgeinDays))
MAE.rt

# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html

fancyRpartPlot(rt.fit1)

library(caret)
varImp(rt.fit1)

plot(rt.fit1)
text(rt.fit1)

###
# random forest
###
library(randomForest)
set.seed(123)

# default mtry
rf.fit1 <- randomForest(AgeinDays ~ .,
                    data=train, importance = TRUE, ntree=500)

rf1.yhat <- predict(rf.fit1,test)
RMSE.rf1 <- sqrt(mean((rf1.yhat-test$AgeinDays)^2))
RMSE.rf1

MAE.rf1 <- mean(abs(rf1.yhat-test$AgeinDays))
MAE.rf1

# explore
which.min(rf.fit1$mse)

plot(rf.fit1)
imp <- as.data.frame(sort(importance(rf.fit1)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
varImp(rf.fit1)

varImpPlot(rf.fit1)

### get mtry 
set.seed(10)
names(train) # check the column for y
bestmtry <- tuneRF(train[,-c(5)],train$AgeinDays,ntreeTry=500,
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
bestmtry
# mtry 9
set.seed(12)
rf.fit2 <- randomForest(AgeinDays ~ .,
                        data=train, importance = TRUE, ntree=500,mtry=9)

rf2.yhat <- predict(rf.fit2,test)
RMSE.rf2 <- sqrt(mean((rf2.yhat-test$AgeinDays)^2))
RMSE.rf2

MAE.rf2 <- mean(abs(rf2.yhat-test$AgeinDays))
MAE.rf1

# mtry 10
set.seed(11)
rf.fit3 <- randomForest(AgeinDays ~ .,
                        data=train, importance = TRUE, ntree=500,mtry=10)

rf3.yhat <- predict(rf.fit3,test)
RMSE.rf3 <- sqrt(mean((rf3.yhat-test$AgeinDays)^2))
RMSE.rf3

MAE.rf3 <- mean(abs(rf3.yhat-test$AgeinDays))
MAE.rf3

# mtry = all, bagging
set.seed(12)
rf.fit4 <- randomForest(AgeinDays ~ .,
                        data=train, importance = TRUE, ntree=500,mtry=12)

rf4.yhat <- predict(rf.fit4,test)
RMSE.rf4 <- sqrt(mean((rf4.yhat-test$AgeinDays)^2))
RMSE.rf4

MAE.rf4 <- mean(abs(rf4.yhat-test$AgeinDays))
MAE.rf4

###
# xgb
###
library(xgboost)

str(train)

# http://wiselily.com/2015/07/12/xgboost-data-mining-example-1/

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
                   nrounds     = 500,
                   objective   = "reg:linear",
                   eval_metric = "rmse")

xb1.yhat <- predict(xb.fit1, data.matrix(subset(test, select = -AgeinDays)))
RMSE.xb1 <- sqrt(mean((xb1.yhat-test$AgeinDays)^2))
RMSE.xb1

MAE.xb1 <- mean(abs(xb1.yhat-test$AgeinDays))
MAE.xb1


test$PredictedAge <- xb1.yhat
plot(test$PredictedAge,test$AgeinDays)
ggplot(test, aes(AgeinDays,PredictedAge, colour=case_type)) + geom_point()
ggplot(test, aes(AgeinDays,PredictedAge, colour=case_type)) + geom_point(size=3)
ggplot(test, aes(AgeinDays,PredictedAge, colour=case_type)) + geom_point(shape=1,size=3)
ggplot(test, aes(AgeinDays,PredictedAge)) + geom_point(size=2, aes(color=case_type))
ggplot(test, aes(AgeinDays,PredictedAge)) + geom_point(size=2, aes(color=case_type,shape=CivilOrCriminal))
ggplot(test, aes(AgeinDays,PredictedAge)) + geom_point(size=2, aes(shape=CivilOrCriminal))
ggplot(test, aes(AgeinDays,PredictedAge)) + geom_point(size=2, aes(color=case_type,shape=CivilOrCriminal))

ggplot(test, aes(AgeinDays,PredictedAge)) + geom_point(size=4, aes(color=case_type,shape=CivilOrCriminal)) + 
  geom_point(colour="grey90", size = 1.5)

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

###
# results
###
accuracy <- data.frame(Method = c("Baseline","Linear Regression","Rpart","RF-defaultmtry","RF-mtry6","RF-mtry7","RF-bagging","X Boost"),
                       RMSE   = c(RMSE.baseline,RMSE.lm1,RMSE.rt,RMSE.rf1,RMSE.rf2,RMSE.rf3,RMSE.rf4,RMSE.xb1),
                       MAE    = c(MAE.baseline,MAE.lm1,MAE.rt,MAE.rf1,MAE.rf2,MAE.rf3,MAE.rf4,MAE.xb1)) 

accuracy

# for compare....removed all na's...
accuracy0 <- accuracy
accuracy0

###
# explore results
###


test$PredictedAge <- xb1.yhat


a <- select(test,AgeinDays,PredictedAge)
View(a)
a$Diff <- round((a$AgeinDays - a$PredictedAge),0)

a$NumMatrixPredictAge <- xb1.yhat
a$Diff2 <- round((a$AgeinDays - a$NumMatrixPredictAge),0)

# http://stackoverflow.com/questions/29725265/how-can-i-calculate-residual-standard-error-in-r-for-test-data-set
# residual standard error
sqrt(sum(a$PredictedAge-a$AgeinDays)^2)/(nrow(a)-2)

#standard deviation of the predicted residuals
sd(a$PredictedAge-a$AgeinDays)

# http://stats.stackexchange.com/questions/110999/r-confused-on-residual-terminology


# http://stats.stackexchange.com/questions/51046/how-to-check-if-my-regression-model-is-good

###
# http://stats.stackexchange.com/questions/110999/r-confused-on-residual-terminology
###
# test$error <- with(test, pred-actual)
# Diff and Diff2
test1.mse <- mean(a$Diff^2)
test2.mse <- mean(a$Diff2^2)
test1.mse
test2.mse

test1.rmse <- sqrt(test1.mse)
test2.rmse <- sqrt(test2.mse)
test1.rmse
test2.rmse

# http://people.duke.edu/~rnau/compare.htm
# ToDoToDoToDo
# http://stats.stackexchange.com/questions/51046/how-to-check-if-my-regression-model-is-good
