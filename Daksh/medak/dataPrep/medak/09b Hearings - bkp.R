###
# No of hearings
###

dim(filter(h,is.na(hearing_date)))
dim(h)
View(h)

p <- as.data.frame(table(h$hearing_date))
sum(p$Freq)
7488 + 730
dim(h)
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
dim(h)
p1 <- filter(h,hearing_date > t1)
dim(p1)
p2 <- as.data.frame(table(p1$hearing_date))
names(p2) <- c("Date","NumOfHearing")
View(p2)
write.csv(p2,"../dataForD3/MedakHearingNumsFilter.csv",row.names=FALSE)
summary(p2$NumOfHearing)

###
# No of hearings by judge
###
h$before_honourable_judges <- ifelse(h$before_honourable_judges == "","No Judge",h$before_honourable_judges)
table(h$before_honourable_judges)
#p3 <- as.data.frame(table(h1$hearing_date,h1$before_honourable_judges))
p3 <- as.data.frame(table(h$hearing_date,h$before_honourable_judges))
dim(filter(p3,Freq == 0))
dim(p3)
p3 <- filter(p3,Freq != 0)
View(p3)
names(p3) <- c("Date","Judge","NumOfHearing")

p31 <- p3 %>% spread(Judge,NumOfHearing)
p31[is.na(p31)] <- 0
View(p31)

write.csv(p31,"../dataForD3/MedakHearingsNumByJudge.csv",row.names=FALSE)

# Filter
t1 <- as.Date("2011-12-31", format = "%Y-%m-%d")
t1

dim(h)
p1 <- filter(h,hearing_date > t1)
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
# Summary by Judge [ Interesting ]
###

mSum <- d %>% group_by(before_honourable_judges) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
View(mSum)

mSum <- d %>% group_by(before_honourable_judges,NewStatus) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
View(mSum)

###
# Calculate average hearings in a day ????
###
mSumH <- h %>% group_by(hearing_date,before_honourable_judges) %>% summarise(TotalHearing = n())
View(mSumH)
dim(filter(mSumH,is.na(hearing_date)))
dim(filter(h,is.na(hearing_date)))
dim(h)
sum(mSumH$TotalHearing)
dim(filter(mSumH,is.na(hearing_date)))
q1 <- (filter(mSumH,!is.na(hearing_date)))
View(q1)
dim(q1)
dim(mSumH)
sum(q1$TotalHearing)
dim(filter(h,is.na(hearing_date)))
7488 + 730
dim(h)
summary(q1)

# filter out "No Judge"
q1 <- filter(q1,before_honourable_judges != "No Judge")
View(q1)
# Filter and see diff
t1 <- as.Date("2011-12-31", format = "%Y-%m-%d")
t1
dim(h)
q11 <- filter(q1,hearing_date > t1)
dim(q11)
dim(q1)

q2 <- q1 %>% group_by(before_honourable_judges) %>% summarise(TotalHearing = n())
View(q2)
q21 <- q11 %>% group_by(before_honourable_judges) %>% summarise(TotalHearing = n())
View(q21)

q3 <- q1 %>% group_by(before_honourable_judges) %>% summarise(AvgHearing = round(mean(TotalHearing,na.rm=TRUE),0))
View(q3)
q31 <- q11 %>% group_by(before_honourable_judges) %>% summarise(AvgHearing = round(mean(TotalHearing,na.rm=TRUE),0))
View(q31)

q4 <- q1 %>% group_by(before_honourable_judges) %>% summarise(MaxHearing = max(TotalHearing,na.rm=TRUE))
View(q4)
q41 <- q11 %>% group_by(before_honourable_judges) %>% summarise(MaxHearing = max(TotalHearing,na.rm=TRUE))
View(q41)

# check how many days are above mean
j1 <- filter(q1,before_honourable_judges == "Senior Civil Judge" & TotalHearing >= 6)
View(j1)
dim(j1)
dim(q1)
2542/102

## get mean for all
p1 <- q1 %>% group_by(hearing_date) %>% summarise(TotalHearing = sum(TotalHearing))
View(p1)
summary(p1$TotalHearing)
p2 <- filter(p1,TotalHearing >= 7)
dim(p2)
