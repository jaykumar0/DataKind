library(tidyr)
###
# working on h and not on d !
###

#h$before_honourable_judges <- ifelse(h$before_honourable_judges == "","No Judge",h$before_honourable_judges)
#table(h$before_honourable_judges)
# filter out "No Judge"
# q <- filter(h,before_honourable_judges != "No Judge")

# Filter out blank Judges
dim(filter(h,before_honourable_judges == ""))
dim(h)

q <- filter(h,before_honourable_judges != "")
dim(q)

# Filter out na hearing dates
dim(filter(q,is.na(hearing_date)))
dim(filter(h,is.na(hearing_date)))

q <- filter(q,!is.na(hearing_date))
dim(q)

table(q$before_honourable_judges)
table(h$before_honourable_judges)

###
# No of hearings
###

p <- as.data.frame(table(q$hearing_date))
sum(p$Freq)

names(p) <- c("Date","NumOfHearing")
View(p)
summary(p$NumOfHearing)
write.csv(p,"../dataForD3/MedakHearingNums.csv",row.names=FALSE)

###
# filter before 2011...v less...
###

t1 <- as.Date("2011-12-31", format = "%Y-%m-%d")
t1
class(p$Date)

p1 <- filter(q,hearing_date > t1)
dim(p1)
p2 <- as.data.frame(table(p1$hearing_date))
names(p2) <- c("Date","NumOfHearing")
summary(p2$NumOfHearing)
dim(p2)
dim(q)
View(p2)
write.csv(p2,"../dataForD3/MedakHearingNumsFilter.csv",row.names=FALSE)
summary(p2$NumOfHearing)

###
# No of hearings by judge
###
p3 <- as.data.frame(table(q$hearing_date,q$before_honourable_judges))
dim(filter(p3,Freq == 0))
dim(p3)
p3 <- filter(p3,Freq != 0)
View(p3)
names(p3) <- c("Date","Judge","NumOfHearing")

p31 <- p3 %>% spread(Judge,NumOfHearing)
p31[is.na(p31)] <- 0
View(p31)

write.csv(p31,"../dataForD3/MedakHearingsNumByJudge.csv",row.names=FALSE)

###
# Filter
###

t1 <- as.Date("2011-12-31", format = "%Y-%m-%d")
t1

p1 <- filter(q,hearing_date > t1)
dim(p1)
p4 <- as.data.frame(table(p1$hearing_date,p1$before_honourable_judges))
dim(filter(p4,Freq == 0))
dim(p1)
p4 <- filter(p4,Freq != 0)
View(p4)
names(p4) <- c("Date","Judge","NumOfHearing")

p41 <- p4 %>% spread(Judge,NumOfHearing)
p41[is.na(p41)] <- 0
View(p41)

write.csv(p41,"../dataForD3/MedakHearingsNumByJudgeFilter.csv",row.names=FALSE)


###
# Calculate average hearings and max hearing
###
# q is the master dataframe

mSumH <- q %>% group_by(hearing_date,before_honourable_judges) %>% summarise(TotalHearing = n())
View(mSumH)
dim(filter(mSumH,is.na(hearing_date)))

q1 <- (filter(mSumH,!is.na(hearing_date)))
View(q1)
summary(q1)

# Filter and see diff
t1 <- as.Date("2011-12-31", format = "%Y-%m-%d")
t1
q11 <- filter(q1,hearing_date > t1)
dim(q11)
dim(q1)

summary(q1)
q2 <- q1 %>% group_by(before_honourable_judges) %>% summarise(TotalHearing = n())
View(q2)
q21 <- q11 %>% group_by(before_honourable_judges) %>% summarise(TotalHearing = n())
View(q21)

q3 <- q1 %>% group_by(before_honourable_judges) %>% summarise(AvgHearing = round(mean(TotalHearing,na.rm=TRUE),0))
View(q3)
q31 <- q11 %>% group_by(before_honourable_judges) %>% summarise(AvgHearing = round(mean(TotalHearing,na.rm=TRUE),0))
View(q31)

length(unique(q1$hearing_date)) 
# it is group by mean..so will have ot find the no of occuenaces for individual judges

q4 <- q1 %>% group_by(before_honourable_judges) %>% summarise(MaxHearing = max(TotalHearing,na.rm=TRUE))
View(q4)
q41 <- q11 %>% group_by(before_honourable_judges) %>% summarise(MaxHearing = max(TotalHearing,na.rm=TRUE))
View(q41)

mSum <- q1 %>% group_by(before_honourable_judges) %>%
  summarise(AvgHearing = round(mean(TotalHearing,na.rm=TRUE),0), 
            MaxHearing = max(TotalHearing,na.rm=TRUE))

write.csv(mSum,"../dataForD3/MedakHearingStats.csv",row.names=FALSE)

View(mSum)

###
# Calculate no of days when the hearings per day is >= mean by Judge
# q1 is grouped by date and judge - filer from from mSumH ) 
# mSum is q1 grouped by judge and has the average  
###

View(q1)
View(mSum)
q2 <- q1
q2 <- inner_join(q2, mSum)
q2$MaxHearing <- NULL
View(q2)
View(filter(q2,before_honourable_judges == "Senior Civil Judge"))

q2$AboveAvg <- ifelse(q2$TotalHearing >= q2$AvgHearing,TRUE,FALSE)       
q2$BelowAvg <- ifelse(q2$TotalHearing < q2$AvgHearing,TRUE,FALSE)       
View(q2)

mSum1 <- q2 %>% group_by(before_honourable_judges) %>%
  summarise(DaysAboveAvgHearing = sum(AboveAvg), 
  DaysBelowAvgHearing = sum(BelowAvg))

View(mSum1)
write.csv(mSum1,"../dataForD3/MedakNoOfDaysOfHearingAboveAndBelowAverage.csv",row.names=FALSE)
mSum2 <- mSum1
mSum2$Tot <- mSum2$DaysAboveAvgHearing + mSum2$DaysBelowAvgHearing 
View(mSum2)
mSum2$PercentDaysAboveAvgHearing <- round((mSum2$DaysAboveAvgHearing / mSum2$Tot),4) * 100
mSum2$PercentDaysBelowAvgHearing <- round((mSum2$DaysBelowAvgHearing / mSum2$Tot),4) * 100
mSum2$Tot <- NULL 
mSum2$DaysAboveAvgHearing <- NULL
mSum2$DaysBelowAvgHearing <- NULL
View(mSum2)
write.csv(mSum2,"../dataForD3/MedakNoOfDaysOfHearingAboveAndBelowAverage2.csv",row.names=FALSE)

###
# explore.....
###
# check how many days are above mean
j1 <- filter(q1,before_honourable_judges == "Senior Civil Judge" & TotalHearing >= 6)
View(j1)
dim(j1)
dim(q1)
dim(filter(q1,before_honourable_judges == "Senior Civil Judge"))
View(filter(q1,before_honourable_judges == "Senior Civil Judge"))
2542/102
(102/258)*100
## get mean for all
p1 <- q1 %>% group_by(hearing_date) %>% summarise(TotalHearing = sum(TotalHearing))
View(p1)
summary(p1$TotalHearing)
p2 <- filter(p1,TotalHearing >= 7)
dim(p2)

###
# 1. get year of hearing
# then count in year - total  AND total days in year
# year wise
# case assigned
# case decided
#####
format(q$hearing_date, "%Y")

q$HearingYear <- format(q$hearing_date, "%Y")
q$HearingDay <- 1
table(q$HearingYear)

p1 <- as.data.frame(table(q$before_honourable_judges,q$HearingYear))
View(p1)
p12 <- p1 %>% spread(Var2,Freq)
View(p12)

p2 <- q %>% group_by(before_honourable_judges,HearingYear) %>% summarise(DaysHearing = n())
View(p2)
p21 <- p2 %>% spread(HearingYear,DaysHearing)
View(p21)

p3 <- q %>% group_by(before_honourable_judges,HearingYear) %>% summarise(DaysHearing = sum(HearingDay))
View(p3)
p31 <- p3 %>% spread(HearingYear,DaysHearing)
View(p31)

# p12, p21, p31 - same results - 21 and 31 - NA needs to ne marked 0
colnames(p12)[1] <- "before_honourable_judges"
write.csv(p12,"../dataForD3/MedakTotalHearingByJudgeByYear.csv",row.names=FALSE)


p4 <- q %>% group_by(hearing_date,before_honourable_judges) %>% summarise(TotalHearing = n())
p41 <- (filter(p4,!is.na(hearing_date)))
p41$HearingYear <- format(p41$hearing_date, "%Y")
View(p41)
p5 <- p41 %>% group_by(before_honourable_judges,HearingYear) %>% summarise(DaysHearing = n())
p51 <- p5 %>% spread(HearingYear,DaysHearing)
p51[is.na(p51)] <- 0
View(p51)

write.csv(p51,"../dataForD3/MedakNoOfDaysOfHearingByJudgeByYear.csv",row.names=FALSE)


# decisions

table(d$before_honourable_judges,format(d$decision_date, "%Y"))
table(format(d$decision_date, "%Y"))
p6 <- as.data.frame(table(d$before_honourable_judges,format(d$decision_date, "%Y")))
View(p6)
p6 <- filter(p6,Freq != 0)
p61 <- p6 %>% spread(Var2,Freq)
View(p61)
p61[is.na(p61)] <- 0
colnames(p61)[1] <- "before_honourable_judges"
write.csv(p61,"../dataForD3/MedakNoOfCaseDecidedByJudgeByYear.csv",row.names=FALSE)

table(d$before_honourable_judges,format(d$date_filed, "%Y"))
p7 <- as.data.frame(table(d$before_honourable_judges,format(d$date_filed, "%Y")))
View(p7)
p7 <- filter(p7,Freq != 0)
View(filter(p7,Var2 == 2009))
p7$Var2 <- as.character(p7$Var2)
p7 <- filter(p7,Var2 > 2010)

p71 <- p7 %>% spread(Var2,Freq)
View(p71)
p71[is.na(p71)] <- 0
colnames(p71)[1] <- "before_honourable_judges"
write.csv(p71,"../dataForD3/MedakNoOfCaseFiledAndAssignedToJudgeByYear.csv",row.names=FALSE)

###
# How many cases do a Judge hear in a day
###

#Get unique case
names(q)
View(q)


dim(q)
q$HearingYear <- format(q$hearing_date, "%Y")


p8 <- q[!duplicated(q$Key),]
p8 <- q[!duplicated(q$combined_case_number),]
dim(p8)


table(d$NewStatus)

table(p8$before_honourable_judges)
table(p8$before_honourable_judges,p8$HearingYear)


p8 <- q %>% group_by(before_honourable_judges,HearingYear) %>% summarise(CasesHeard = length(unique(combined_case_number)))
p81 <- p8 %>% spread(HearingYear,CasesHeard)
p81[is.na(p81)] <- 0
View(p8)
View(p81)
write.csv(p81,"../dataForD3/MedakUniqueCasesHeardByJudgeByYear.csv",row.names=FALSE)
