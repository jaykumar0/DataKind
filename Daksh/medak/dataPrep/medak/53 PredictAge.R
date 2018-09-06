###
#
###
library(lubridate)
d$Weekday <- wday(d$date_filed)
d$Month   <- month(d$date_filed)

table(d$Weekday)
table(d$Month)


p <- select(d,case_type,court_name,before_honourable_judges,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NumHearing,
            AvgDaysBetweenHearing,NewStatus,Weekday,Month)

###
# filter p
###

disp <- filter(p,NewStatus == "Disposed")
pend <- filter(p,NewStatus != "Disposed")

table(disp$NewStatus)
table(pend$NewStatus)
table(p$NewStatus)

q1 <- na.omit(disp) 
dim(q1)
###
#
###
library(DataExplorer)

GenerateReport(iris)
GenerateReport(p)
GenerateReport(d)
PlotMissing(p)
PlotMissing(disp)
PlotMissing(pend)

###
#
###
dSum <- disp %>% group_by(case_type,court_name,before_honourable_judges) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())

dSum <- disp %>% group_by(case_type) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())

dSum <- disp %>% group_by(case_type,court_name) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())

dSum <- disp %>% group_by(court_name) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())

View(dSum)

tSum <- pend %>% group_by(case_type,court_name,before_honourable_judges) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
tSum <- pend %>% group_by(NewStatus) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
View(tSum)

###
#
###

###
# impute missing values for Pending cases
###
pend["NumHearing"][is.na(pend["NumHearing"])] <- 0
pend["AvgDaysBetweenHearing"][is.na(pend["AvgDaysBetweenHearing"])] <- 0

View(pend)
View(disp)
dim(disp)
dim(pend)
table(p$NewStatus)

dSum <- disp %>% group_by(CivilOrCriminal) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
View(dSum)

###
# Impute missing value using Hmisc
###
# http://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/

library(Hmisc)
?aregImpute

dispImputed <- aregImpute(~ case_type + court_name + before_honourable_judges + CivilOrCriminal + 
                             AgeinDays + DaysToFirstHearing + NumHearing + 
                             AvgDaysBetweenHearing + NewStatus + Weekday + Month,data=disp,n.impute=5)

dispImputed <- aregImpute(~ NumHearing + AvgDaysBetweenHearing,data=disp,n.impute=5)

dispImputed

summary(disp)

summary(dispImputed$imputed$NumHearing)
summary(dispImputed$imputed$NumHearing)


disp$NumHearingImputed <- with(disp, impute(NumHearing, mean))
disp$AvgDaysBetweenHearingImputed <- with(disp, impute(AvgDaysBetweenHearing, mean))
summary(disp)

PlotMissing(disp)
disp$NumHearing <- NULL
disp$AvgDaysBetweenHearing <- NULL
PlotMissing(disp)
dim(disp[rowSums(is.na(disp)) > 0,])
View(disp[rowSums(is.na(disp)) > 0,])
disp$DaysToFirstHearing <- with(disp, impute(DaysToFirstHearing, mean))
PlotMissing(disp)

PlotMissing(pend)
dim(pend[rowSums(is.na(pend)) > 0,])
View(pend[rowSums(is.na(pend)) > 0,])

pend$DaysToFirstHearing <- with(pend, impute(DaysToFirstHearing, mean))
PlotMissing(pend)


###############################################################################
###############################################################################
# Predict
###############################################################################
###############################################################################

### convert to numeric

disp[sapply(disp, is.character)] <- lapply(disp[sapply(disp, is.character)], 
                                             as.factor)
str(disp)

disp$NumHearingImputed <- as.numeric(disp$NumHearingImputed)
disp$AvgDaysBetweenHearingImputed <- as.numeric(disp$AvgDaysBetweenHearingImputed)
str(disp)

disp <- as.data.frame(data.matrix(disp))
str(disp)

## partition into  train and test
## 80% of the sample size
smp_size <- floor(0.80 * nrow(disp))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(disp)), size = smp_size)

train <- disp[train_ind, ]
test <- disp[-train_ind, ]
dim(train)
dim(test)

###
#
###

# convert pend to numeric
str(pend)
pend$DaysToFirstHearing <- as.numeric(pend$DaysToFirstHearing)
pend[sapply(pend, is.character)] <- lapply(pend[sapply(pend, is.character)], 
                                           as.factor)
str(pend)
pend <- as.data.frame(data.matrix(pend))
str(disp)

train <- disp
test <- pend

names(train)
names(test)

train$NumHearing <- train$NumHearingImputed
train$AvgDaysBetweenHearing <- train$AvgDaysBetweenHearingImputed

train$NumHearingImputed <- NULL
train$AvgDaysBetweenHearingImputed <- NULL
