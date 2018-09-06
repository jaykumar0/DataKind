# https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html
# http://stackoverflow.com/questions/20061997/r-test-if-string-contains-string
# http://stackoverflow.com/questions/10128617/test-if-characters-in-string-in-r
# http://www.stat.berkeley.edu/~nolan/stat133/Fall05/lectures/RegEx.html

# http://stackoverflow.com/questions/16822426/r-dealing-with-true-false-na-and-nan
# 
library(stringr)
library(tidyr)
###
# Under Acts
###

View(as.data.frame(table(d$under_acts)) %>% arrange(desc(Freq)))

### CivilOrCriminal Done in preprocess...dont do now ...23/05/2016
###
# Clean under_acts
###

d$UnderActsNew <- d$under_acts
d$UnderActsNew <- ifelse(d$UnderActsNew == "","IPC",d$UnderActsNew)

#View(as.data.frame(table(d$UnderActsNew)) %>% arrange(desc(Freq)))
d$UnderActsNew <- str_replace_all(d$UnderActsNew,"INDIAN PENAL CODE","IPC")
d$UnderActsNew <- str_replace_all(d$UnderActsNew,"MOTOR VEHICLE ACT","IPC")
d$UnderActsNew <- str_replace_all(d$UnderActsNew,"CODE OF CRIMINAL PROCEDURE","IPC")
#d$UnderActsNew <- str_replace_all(d$UnderActsNew,"MV ACT","IPC")

d$CivilOrCriminal <- ifelse(str_detect(d$UnderActsNew,"IPC"),"Criminal","Civil")
table(d$CivilOrCriminal)

###
#
###
sc <- select(d,combined_case_number,case_type,NewStatus,CivilOrCriminal,
             DaysToFirstHearing, NumHearing, AvgDaysBetweenHearing, AgeinDays)

sc[is.na(sc)] <- 0
View(sc)
summary(sc)
View(filter(d,DaysToFirstHearing < 0))
#sc$AvgDaysBetweenHearing <- round(sc$AvgDaysBetweenHearing,0)
#sc$DaysToFirstHearing <- abs(sc$DaysToFirstHearing)

#write.csv(sc,"dataForD3/medakCivilOrCriminalAll.csv")

###
# Get Summary for Civil and Criminal on All data
###

dSummary <- d %>% group_by(CivilOrCriminal, NewStatus) %>%
  summarise(avgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            avgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            avgAvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            avgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            totalCases = n())

View(dSummary)

# mark na as 0
dSummary[is.na(dSummary)] <- 0

names(dSummary)

p1 <- select(dSummary,CivilOrCriminal,NewStatus,avgDaysToFirstHearing)
names(p1)
p1 <- filter(p1,avgDaysToFirstHearing != 0)
p12 <- p1 %>% spread(NewStatus,avgDaysToFirstHearing)
View(p12)
write.csv(p12,"../dataForD3/medakCorCAvgDaysToFirstHearing.csv",row.names=FALSE)


p2 <- select(dSummary,CivilOrCriminal,NewStatus,avgNumOfHrng)
p2 <- filter(p2,avgNumOfHrng != 0)
View(p2)
p21 <- p2 %>% spread(NewStatus,avgNumOfHrng)
View(p21)
write.csv(p21,"../dataForD3/medakCorCAvgNumOfHearing.csv",row.names=FALSE)


p3 <- select(dSummary,CivilOrCriminal,NewStatus,avgAvgDaysBetweenHrng)
p3 <- filter(p3,avgAvgDaysBetweenHrng != 0)
p31 <- p3 %>% spread(NewStatus,avgAvgDaysBetweenHrng)
View(p31)
write.csv(p31,"../dataForD3/medakCorCAvgDaysBetweenHearing.csv",row.names=FALSE)


p4 <- select(dSummary,CivilOrCriminal,NewStatus,avgAgeinDays)
p41 <- p4 %>% spread(NewStatus,avgAgeinDays)
View(p41)
write.csv(p41,"../dataForD3/medakCorCAvgAgeInDays.csv",row.names=FALSE)

###
# Total Civil and Criminal
###

q1 <- as.data.frame(table(d$CivilOrCriminal))
q1
names(q1) <- c("Type","NumCases")
q1$Percentage <- round(q1$NumCases/sum(q1$NumCases),4)*100
write.csv(q1,"../dataForD3/medakCorCNum.csv",row.names=FALSE)

###
# C or C and Status
###
q1 <- as.data.frame(table(d$CivilOrCriminal,d$NewStatus))
q1
#rename(q1, c("Var1"="Type","Var2" = "Status"))
#select(q1, c("Var1"="Type","Var2" = "Status"))
names(q1)[1] <- "Type"
q1
q12 <- q1 %>% spread(Var2,Freq)
View(q12)
write.csv(q12,"../dataForD3/medakCorCAndStatus.csv",row.names=FALSE)

###
# Get case types which have both civil and criminal
###

q1 <- as.data.frame(table(d$case_type,d$CivilOrCriminal))
View(q1)
q1 <- filter(q1,Freq != 0)

# filter from d now and get summary
q2 <- filter(q1,Var1 %in% c("CC","DVC","EP","IP","L.A.O.P","LGOP","MC","MVOP","OP","OS","PRC"))
View(q2)
q3 <- filter(d,case_type %in% c("CC","DVC","EP","IP","L.A.O.P","LGOP","MC","MVOP","OP","OS","PRC"))

qSummary <- q3 %>% group_by(case_type,CivilOrCriminal) %>%
  summarise(avgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            avgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            avgAvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            avgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            totalCases = n())

View(qSummary)

# mark na as 0
qSummary[is.na(qSummary)] <- 0


q4 <- as.data.frame(table(q3$case_type,q3$CivilOrCriminal))
View(q4)
q41 <- q4 %>% spread(Var2,Freq)
View(q41)

q4 <- as.data.frame(table(d$case_type,d$CivilOrCriminal))
View(q4)
q41 <- q4 %>% spread(Var2,Freq)
View(q41)
