library(tidyr)

###
# Total Cases - number and Percentage
###
p1 <- as.data.frame(table(d$NewStatus))
p1
tot <- sum(p1$Freq)
tot
p1$Percentage <- round(p1$Freq/tot,4) * 100
p1
names(p1) <- c("Status","NumCases","Percentage")
p1
write.csv(p1,"../dataForD3/MedakStatusWiseTotAndPercent.csv",row.names=FALSE)

###
# Total Cases - Case Type and Status
###
p1 <- as.data.frame(table(d$case_type,d$NewStatus))
View(p1)
names(p1) <- c("CaseType","Var2","Freq")
p12 <- p1 %>% spread(Var2,Freq)
View(p12)
write.csv(p12,"../dataForD3/medakCaseTypeStatusTotal.csv",row.names=FALSE)

###
# Pending Cases - Case Type and Old Status
###
p1 <- as.data.frame(table(d$case_type,d$current_status))
View(p1)
names(p1) <- c("CaseType","Var2","Freq")
p12 <- p1 %>% spread(Var2,Freq)
View(p12)
write.csv(p12,"../dataForD3/medakCaseTypeOldStatusTotal.csv",row.names=FALSE)

###
# Date Filed
###

table(format(d$date_filed, "%Y"))
table(format(d$date_filed, "%Y"),d$NewStatus)
p1 <- as.data.frame(table(format(d$date_filed, "%Y"),d$NewStatus))
View(p1)
names(p1) <- c("YearFiled","Status","NumCases")

p21 <- p1 %>% spread(Status,NumCases)
View(p21)

write.csv(p21,"../dataForD3/medakYearFiledStatus.csv",row.names=FALSE)

###
# Cases By Judge and Status - Not done
###
p1 <- as.data.frame(table(d$before_honourable_judges,d$current_status))
View(p1)
names(p1) <- c("Judge","Var2","Freq")
p12 <- p1 %>% spread(Var2,Freq)
View(p12)
p12 <- filter(p12,Judge != "No Judge")
write.csv(p12,"../dataForD3/medakJudgeAndStatus.csv",row.names=FALSE)

###
# Judge and Pending Status number
###
p1 <- as.data.frame(table(d$before_honourable_judges,d$current_status))
p1 <- filter(p1,Var2 != "Disposed")
p1 <- filter(p1,Freq != 0)

View(p1)
names(p1) <- c("Judge","Var2","Freq")
p12 <- p1 %>% spread(Var2,Freq)
View(p12)
p12 <- filter(p12,Judge != "No Judge")
write.csv(p12,"../dataForD3/medakJudgePending.csv",row.names=FALSE)

###
# Court and Status
###
p1 <- as.data.frame(table(d$court_name,d$NewStatus))
p1 <- filter(p1,Freq != 0)
View(p1)

names(p1) <- c("Court","Var2","Freq")
p12 <- p1 %>% spread(Var2,Freq)
View(p12)
p12[is.na(p12)] <- 0
write.csv(p12,"../dataForD3/medakCourtAndStatus.csv",row.names=FALSE)
