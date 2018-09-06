library(ggplot2)

###
# This for viz
###

d$Weekday <- weekdays(d$date_filed)
d$Month <- months(d$date_filed)
# Order Weekday 
d$Weekday <- factor(d$Weekday, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# Order Month
d$Month <- factor(d$Month, ordered=TRUE, levels=c("January", "February", "March", "April", "May", "June", "July","August","September",
                                                  "October","November","December"))

table(d$Weekday)
table(d$Month)

d$WeekdayD <- weekdays(d$decision_date)
d$MonthD <- months(d$decision_date)
# Order Weekday 
d$WeekdayD <- factor(d$WeekdayD, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# Order Month
d$MonthD <- factor(d$MonthD, ordered=TRUE, levels=c("January", "February", "March", "April", "May", "June", "July","August","September",
                                                  "October","November","December"))

table(d$WeekdayD)
table(d$MonthD)

###
# This for prediction
###
library(lubridate)
d$Weekday <- wday(d$date_filed)
d$Month   <- month(d$date_filed)

table(d$Weekday)
table(d$Month)

###
# na's
###
dSum <- d %>%
  group_by(case_type) %>%
  summarise(numDTFH=sum(is.na(DaysToFirstHearing)),
            numAvgDbH=sum(is.na(AvgDaysBetweenHearing)),
            numH=sum(is.na(NumHearing)),
            TotCase = n())

View(dSum)

d %>% 
  summarise_each(funs(100*mean(is.na(.))))


###
# filter the dataset
###
# for Predict
p <- select(d,case_type,court_name,before_honourable_judges,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NumHearing,
            AvgDaysBetweenHearing,NewStatus,Weekday,Month)
# for viz
p <- select(d,case_type,court_name,before_honourable_judges,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NumHearing,
            AvgDaysBetweenHearing,NewStatus,Weekday,Month,WeekdayD,MonthD)

disp <- filter(p,NewStatus == "Disposed")
#pend <- filter(p,NewStatus != "Disposed")


###
# Check for missing values - 01
###

library(DataExplorer)

disp$NewStatus <- NULL
GenerateReport(disp)

###
#how many  missing values
###

table(is.na(disp))
colSums(is.na(disp))

###
# Plot missing vals
###
ggplot(disp, aes(x=case_type, y=AvgDaysBetweenHearing, fill=case_type)) + geom_boxplot() + 
  theme(legend.position="none")

###
# # filter the missing values
###
disp <- na.omit(disp) 

PlotMissing(p)
PlotMissing(disp)
PlotMissing(pend)

GenerateReport(disp)
cor(disp$AgeinDays,disp$DaysToFirstHearing,use="complete.obs")
cor(disp$AgeinDays,disp$AvgDaysBetweenHearing,use="complete.obs")
cor(disp$AgeinDays,disp$NumHearing,use="complete.obs")

###
# Check for missing values - 02
###

library(mice)
md.pattern(disp)

library(VIM)
aggr_plot <- aggr(disp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))




temp <- select(d,DaysToFirstHearing,NumHearing)
temp <- select(d,DaysToFirstHearing,AvgDaysBetweenHearing)
temp <- select(d,NumHearing,AvgDaysBetweenHearing)

marginplot(temp)
?marginplot

###
# explore and try to find any pattern in missing values
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

### explore
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
# 01 Impute missing value using Hmisc
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

#how many  missing values
table(is.na(disp))
colSums(is.na(disp))

### ignore does not work....by  case_type is also NA'...
dispI <- disp
dispI <- disp %>% group_by(case_type) %>% 
  mutate(AvgDaysBetweenHearing = ifelse(is.na(AvgDaysBetweenHearing), mean(AvgDaysBetweenHearing, na.rm=TRUE), AvgDaysBetweenHearing))

dispI <- disp %>% group_by(case_type) %>% 
  mutate(NumHearing = ifelse(is.na(NumHearing), mean(NumHearing, na.rm=TRUE), NumHearing))

dispI <- disp %>% group_by(case_type) %>% 
  mutate(DaysToFirstHearing = ifelse(is.na(DaysToFirstHearing), mean(DaysToFirstHearing, na.rm=TRUE), DaysToFirstHearing))

dispI$DaysToFirstHearing1 = ifelse(is.na(dispI$DaysToFirstHearing), mean(dispI$DaysToFirstHearing, na.rm=TRUE), dispI$DaysToFirstHearing)

dispI <- dispI %>% group_by(case_type) %>% 
  mutate(DaysToFirstHearing1 = ifelse(is.na(DaysToFirstHearing), mean(DaysToFirstHearing, na.rm=TRUE), DaysToFirstHearing))

dispI <- dispI %>% 
  mutate(DaysToFirstHearing1 = ifelse(is.na(DaysToFirstHearing), mean(DaysToFirstHearing, na.rm=TRUE), DaysToFirstHearing))


###
# 02 Impute Uisng mice
###
# http://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
library(mice)
tempD <- mice(disp,m=5,maxit=50,meth='pmm',seed=500)
summary(tempD)

tempD$imp$NumHearing

completedData <- complete(tempD,1)
completedData <- complete(tempD,2)
completedData <- complete(tempD,3)
completedData <- complete(tempD,4)
completedData <- complete(tempD,5)
summary(completedData)

summary(completedData$DaysToFirstHearing)
summary(disp$DaysToFirstHearing)

summary(completedData$NumHearing)
summary(disp$NumHearing)

summary(completedData$AvgDaysBetweenHearing)
summary(disp$AvgDaysBetweenHearing)

library(lattice) 
xyplot(tempD,DaysToFirstHearing ~ NumHearing+AvgDaysBetweenHearing,pch=18,cex=1)
xyplot(tempD,AvgDaysBetweenHearing ~ NumHearing,pch=18,cex=1)

densityplot(tempD)

#how many  missing values
table(is.na(disp))
colSums(is.na(disp))
# let mice impute the missing visibilities
imputedD <- mice(disp,m=1,maxit=10,meth='pmm',seed=10)
summary(imputedD)
completedData <- complete(imputedD,1)
str(completedData)
table(is.na(completedData))
colSums(is.na(completedData))
str(completedData)

PlotMissing(completedData)
###
#
###
PlotMissing(disp)
#disp$NumHearing <- NULL
#disp$AvgDaysBetweenHearing <- NULL
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
# Explore
###############################################################################
###############################################################################
disp <- completedData

###
# Ignore missing values
###
disp <- na.omit(disp) 
GenerateReport(disp)
cor(disp$AgeinDays,disp$AvgDaysBetweenHearing,use="complete.obs")
cor(disp$AgeinDays,disp$NumHearing,use="complete.obs")
cor(disp$AgeinDays,disp$DaysToFirstHearing,use="complete.obs")

### convert to factors
str(disp)
disp[sapply(disp, is.character)] <- lapply(disp[sapply(disp, is.character)], 
                                             as.factor)
str(disp)

summary(disp)

dim(filter(disp,DaysToFirstHearing == 0))
View(filter(disp,DaysToFirstHearing == 0))
disp <- filter(disp,DaysToFirstHearing != 0)
summary(disp)
View(disp)
###
# stop here for viz..explore...
###



###
# move this to prediction
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


###
#
###
# http://r4stats.com/examples/graphics-ggplot2/
# http://stackoverflow.com/questions/15014595/how-to-use-black-and-white-fill-patterns-instead-of-color-coding-on-calendar-hea
# http://rgraphgallery.blogspot.in/2013/04/rg-heatmap-plot-of-calender.html
# http://stackoverflow.com/questions/15014595/how-to-use-black-and-white-fill-patterns-instead-of-color-coding-on-calendar-hea
# http://www.scoop.it/t/things-about-r/p/2108480383/2012/07/05/margintale-ggplot2-time-series-heatmaps
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# https://plot.ly/ggplot2/geom_bar/
# https://blog.clevertap.com/a-brief-primer-on-linear-regression-part-ii/
