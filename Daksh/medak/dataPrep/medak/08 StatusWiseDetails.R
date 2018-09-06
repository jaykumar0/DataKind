# https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html
# http://stackoverflow.com/questions/20061997/r-test-if-string-contains-string
# http://stackoverflow.com/questions/10128617/test-if-characters-in-string-in-r
# http://www.stat.berkeley.edu/~nolan/stat133/Fall05/lectures/RegEx.html

# http://stackoverflow.com/questions/16822426/r-dealing-with-true-false-na-and-nan
# 
library(stringr)
library(tidyr)
###
# Get Hearings Summary by Status
###

mSum3 <- d %>% group_by(NewStatus) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())

View(mSum3)
# mark na as 0
mSum3[is.na(mSum3)] <- 0
write.csv(mSum3,"../dataForD3/medakDetailsByStatus.csv",row.names=FALSE)

mSum31 <- filter(mSum3,NewStatus != "Pending")
View(mSum31)
write.csv(mSum31,"../dataForD3/medakDetailsByStatus2.csv",row.names=FALSE)


### end #####
### end #####
### end #####

#################################
hSummary <- d %>% group_by(case_type,NewStatus) %>%
  summarise(avgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            avgAvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0))


View(hSummary)
# omit na's
hSum <- na.omit(hSummary)
View(hSum)
# mark na as 0
hSummary[is.na(hSummary)] <- 0

names(dSummary)

hSummary1 <- d %>% group_by(NewStatus) %>%
  summarise(avgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            avgAvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0))

View(hSummary1)
# omit na's
hSum1 <- na.omit(hSummary1)
View(hSum1)
# mark na as 0
hSummary1[is.na(hSummary1)] <- 0


###
# Age and First Hearing
###
mSum2 <- d %>% group_by(case_type,NewStatus) %>%
  summarise(avgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            avgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            totalCases = n())


View(mSum2)

mSum2 <- d %>% group_by(NewStatus) %>%
  summarise(avgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            avgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            totalCases = n())


View(mSum2)

###

mSum4 <- d %>% group_by(before_honourable_judges,NewStatus) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())


View(mSum4)

mSum4[is.na(mSum4)] <- 0

write.csv(mSum4,"dataForD3/medakSummaryByJudge.csv")

mSum4$Status <- ifelse(mSum4$NewStatus == "In Process","IP",
                       ifelse(mSum4$NewStatus == "Pending","P","D"))

table(mSum4$NewStatus,mSum4$Status)
#mSum4$JudgeStatus <- paste(mSum4$before_honourable_judges,"(",mSum4$NewStatus,")")
#mSum4$JudgeStatus <- str_c(mSum4$before_honourable_judges,"(",mSum4$NewStatus,")",sep="")
mSum4$JudgeStatus <- str_c(mSum4$before_honourable_judges,"(",mSum4$Status,")",sep="")

mSum4$before_honourable_judges <- NULL
mSum4$NewStatus <- NULL
mSum4$Status <- NULL
write.csv(mSum4,"dataForD3/medakSummaryByJudge.csv")

mSum4$AgeInYears <- round((mSum4$AvgAgeinDays/365),2)
mSum4$AvgYrsToFirstHearing <- round((mSum4$AvgDaysToFirstHearing/365),2)


###
# Select Columns
###

sc <- select(d,combined_case_number,case_type,NewStatus,CivilOrCriminal,
             DaysToFirstHearing, NumHearing, AvgDaysBetweenHearing, AgeinDays)

sc[is.na(sc)] <- 0
View(sc)
summary(sc)
#sc$AvgDaysBetweenHearing <- round(sc$AvgDaysBetweenHearing,0)
sc$DaysToFirstHearing <- abs(sc$DaysToFirstHearing)

###
# Filter case types
###

View(as.data.frame(table(d$case_type)) %>% arrange(desc(Freq)))

# Case > 20
CaseGT20 <- filter(d, case_type %in% c("CC","EP","OS","STC","L.A.O.P","MC","SC","H.M.O.P","MVOP","IP","DVC"))
View(as.data.frame(table(CaseGT20$case_type)) %>% arrange(desc(Freq)))

CaseLT20 <- filter(d,case_type %in% c("AS","PRC","LGOP","SCC","CMA","OP","CRL.MP","ID","S.O.P","A.R.B.O.P","CRIME NO","RCA"))
View(as.data.frame(table(CaseLT20$case_type)) %>% arrange(desc(Freq)))

###
# Get SUmmary for NewStatus on All data
###

dSummary <- CaseGT20 %>% group_by(case_type,NewStatus) %>%
  summarise(avgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            avgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            avgAvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            avgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            totalCases = n())

View(dSummary)

# mark na as 0
dSummary[is.na(dSummary)] <- 0

names(dSummary)

# Can follow - 14 MedakUnderActCivilOrCriminal.R
p1 <- select(dSummary,case_type,NewStatus,avgDaysToFirstHearing)
names(p1)
p1 <- filter(p1,avgDaysToFirstHearing != 0)
View(p1)
p12 <- p1 %>% spread(NewStatus,avgDaysToFirstHearing)
View(p12)
p12[is.na(p12)] <- 0
#write.csv(p12,"dataForD3/medakCorCAvgDaysToFirstHearing.csv")
