###
# Summary by Judge
###

mSum <- d %>% group_by(NewStatus,before_honourable_judges) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
View(mSum)

mSum1 <- d %>% group_by(before_honourable_judges,NewStatus) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
View(mSum1)
mSum1[is.na(mSum1)] <- 0
mSum1 <- filter(mSum1,before_honourable_judges != "No Judge")

# Average Days to First Hearing
m1 <- select(mSum1,c(before_honourable_judges,NewStatus,AvgDaysToFirstHearing))
View(m1)
m1 <- filter(m1,AvgDaysToFirstHearing != 0)
m11 <- m1 %>% spread(NewStatus,AvgDaysToFirstHearing)
m11 <- m1 %>% spread(before_honourable_judges,AvgDaysToFirstHearing)
View(m11)
m11[is.na(m11)] <- 0
write.csv(m11,"../dataForD3/medakJudgeAvgDaysToFirstHearing.csv",row.names=FALSE)

# Average Age 
m2 <- select(mSum1,c(before_honourable_judges,NewStatus,AvgAgeinDays))
View(m2)
m2 <- filter(m2,AvgAgeinDays != 0)
m21 <- m2 %>% spread(before_honourable_judges,AvgAgeinDays)
m21 <- m2 %>% spread(NewStatus,AvgAgeinDays)
View(m21)
m21[is.na(m21)] <- 0
write.csv(m21,"../dataForD3/medakJudgeAvgAgeinDays.csv",row.names=FALSE)

# AvgNumOfHrng 
m3 <- select(mSum1,c(before_honourable_judges,NewStatus,AvgNumOfHrng))
m3 <- filter(m3,AvgNumOfHrng != 0)
m31 <- m3 %>% spread(NewStatus,AvgNumOfHrng)
View(m31)
m31[is.na(m31)] <- 0
write.csv(m31,"../dataForD3/medakJudgeAvgNumOfHrng.csv",row.names=FALSE)

# AvgDaysBetweenHrng 
m4 <- select(mSum1,c(before_honourable_judges,NewStatus,AvgDaysBetweenHrng))
m4 <- filter(m4,AvgDaysBetweenHrng != 0)
m41 <- m4 %>% spread(NewStatus,AvgDaysBetweenHrng)
View(m41)
m41[is.na(m41)] <- 0
write.csv(m41,"../dataForD3/medakJudgeAvgDaysBetweenHrng.csv",row.names=FALSE)
