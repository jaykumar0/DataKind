# http://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/
# https://www.kaggle.com/timevans/prudential-life-insurance-assessment/xgboost-example-0-61249
# http://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
# https://blog.ouseful.info/2016/05/02/when-documents-become-databases-tabulizer-r-wrapper-for-tabula-pdf-table-extractor/
# http://ml4a.github.io/classes/itp-S16/

cor(d$AgeinDays,d$DaysToFirstHearing,na.rm=TRUE)
cor(d$AgeinDays,d$DaysToFirstHearing,use="complete.obs")

d$Weekday <- weekdays(d$date_filed)
d$Month <- months(d$date_filed)
table(d$Weekday)
table(d$Month)

library(lubridate)
d$Weekday <- wday(d$date_filed)
d$Month   <- month(d$date_filed)

table(d$Weekday)
table(d$Month)

p <- select(d,case_type,court_name,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NumHearing,
            AvgDaysBetweenHearing,NewStatus,Weekday,Month)
View(p)
###
# remove Pending cases
###
p <- na.omit(p) 
dim(p)
dim(d)
table(p$NewStatus)
table(d$NewStatus)
###
#
###
library(VIM)
aggr_plot <- aggr(p, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))


aggr_plot <- aggr(d, col=c("skyblue", "red", "orange"), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))



plot(d, col = c("skyblue", "red", "orange"), bars = TRUE,
     numbers = FALSE, prop = TRUE, combined = FALSE, varheight = FALSE,
     only.miss = FALSE, border = par("fg"), sortVars = FALSE,
     sortCombs = TRUE, ylabs = NULL, axes = TRUE, labels = axes,
     cex.lab = 1.2, cex.axis = par("cex"), cex.numbers = par("cex"),
     gap = 4)

###
#
###
#install.packages("DataExplorer")
library(DataExplorer)

GenerateReport(iris)
GenerateReport(p)
GenerateReport(d)
PlotMissing(p)
PlotMissing(d)

p1 <- SplitColType(p)
names(p1)

p1$discrete
p1$continuous

p1$num_discrete
p1$num_continuous
p1$num_all_missing

### Plot continuous values - corelation 
plot(p1$continuous,col="salmon")

# Basic Scatterplot Matrix
names(p1$continuous)
pairs(~AgeinDays+DaysToFirstHearing+NumHearing+AvgDaysBetweenHearing,data=p,
      main="Simple Scatterplot Matrix")

# First Correlogram Example
#install.packages("corrgram")
library(corrgram)
corrgram(p1$continuous, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order") 


corrgram(p1$continuous, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Car Milage Data in PC2/PC1 Order")

corrgram(p1$continuous, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Car Milage Data (unsorted)")

cor(p1$continuous,use="complete.obs")
# missing values
###
# Get the columns with missing values
###
#dM <-sapply(train, function(y) sum(length(which(is.na(y)))))
dM <- sapply(p,function(x) sum(is.na(x)))
dM <- data.frame(dM)
dM

dM <- add_rownames(dM, "Cols")
names(dM) <- c("Column","NumMissingValues")
dM

###
#
###
cor(p$AgeinDays,p$DaysToFirstHearing,use="complete.obs")
cor(p$AgeinDays,p$NumHearing,use="complete.obs")
cor(p$AgeinDays,p$AvgDaysBetweenHearing,use="complete.obs")

chisq.test(p$case_type,p$court_name)
chisq.test(p$case_type,p$CivilOrCriminal)
chisq.test(p$CivilOrCriminal,p$court_name)


#predict age

table(d$current_stage)
table(d$current_stage,d$NewStatus)


###
# convert all to numeric
###

str(p)
p$case_type <- as.numeric(as.factor(p$case_type)) - 1
p$court_name <- as.numeric(as.factor(p$court_name)) - 1
p$CivilOrCriminal <- as.numeric(as.factor(p$CivilOrCriminal)) - 1
p$NewStatus <- as.numeric(as.factor(p$NewStatus)) - 1
str(p)


library(caret)
set.seed(100)
inTrain <- createDataPartition(p$NewStatus, p = .8)[[1]]
train <- p[ inTrain, ]
test  <- p[-inTrain, ]


###
#
###
# http://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/

# Baseline model - predict the mean of the training data
best.guess <- mean(train$AgeinDays) 



# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-test$AgeinDays)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$AgeinDays))
MAE.baseline



###
#
###
lm.fit1 <- lm(AgeinDays ~ NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit1)

lm.fit2 <- lm(AgeinDays ~ case_type + court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit2)

lm.fit3 <- lm(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)
summary(lm.fit3)

lm.fit4 <- lm(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing + Weekday1 + Month1 ,data=train)
summary(lm.fit4)

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

rt <- rpart(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,data=train)

fancyRpartPlot(rt)
test.pred.rtree <- predict(rt,test) 

RMSE.rt <- sqrt(mean((test.pred.rtree-test$AgeinDays)^2))
RMSE.rt

MAE.rt <- mean(abs(test.pred.rtree-test$AgeinDays))
MAE.rt

varImp(rt)
plot(rt)
text(rt)

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

rf1 <- randomForest(AgeinDays ~ court_name + NewStatus + DaysToFirstHearing + NumHearing + AvgDaysBetweenHearing,
                    data=train, importance = TRUE, ntree=1000)

which.min(rf1$mse)
# 24 trees are enough :)

plot(rf1)
imp <- as.data.frame(sort(importance(rf1)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

rf1.yhat <- predict(rf1,test)
RMSE.rf1 <- sqrt(mean((rf1.yhat-test$AgeinDays)^2))
RMSE.rf1

MAE.rf1 <- mean(abs(rf1.yhat-test$AgeinDays))
MAE.rf1

varImpPlot(rf1)

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
p2 <- p
p <- p2
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

accuracy <- data.frame(Method = c("Baseline","Linear Regression1","Linear Regression2","Linear Regression3","Rpart","Random forest","X Boost"),
                       RMSE   = c(RMSE.baseline,RMSE.lm1,RMSE.lm2,RMSE.lm3,RMSE.rt,RMSE.rf1,RMSE.xb),
                       MAE    = c(MAE.baseline,MAE.lm1,MAE.lm2,MAE.lm3,MAE.rt,MAE.rf1,MAE.xb)) 

accuracy
accuracy1 <- accuracy 
accuracy1
RMSE.xb
RMSE.lm4
