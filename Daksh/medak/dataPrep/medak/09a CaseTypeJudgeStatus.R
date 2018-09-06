###
# 01 judge and status
###
# for chord
q1 <- as.data.frame(table(d$before_honourable_judges,d$NewStatus))
View(q1)

# https://rpubs.com/bradleyboehmke/data_wrangling
q12 <- q1%>% spread(Var2,Freq)
colnames(q12)[1] <- "BeforeJudge" 
#rename(q12,Var1 = "BeforeJudge")

View(q12)

write.csv(q12,"../dataForD3/medakJudgeAndStatus.csv",row.names=FALSE)

###
# Case Type, Judge and CorC - do CorC before
###
# For Sankey
# Link 1 - Medak and Case Types
p1 <- as.data.frame(table(d$case_type)) 
View(p1)
p1 <- cbind(Var11 = "Medak",p1)

dim(filter(p1,Freq == 0))
names(p1) <- c("source","target","value")

# Link 2 - Case Type and Court
dim(filter(d,d$court_name == ""))
table(d$court_name)

p2 <- as.data.frame(table(d$case_type,d$court_name))
dim(filter(p2,Freq == 0))
p2 <- filter(p2,Freq != 0)
View(p2)
View(p2 %>% spread(Var2,Freq))
names(p2) <- c("source","target","value")
#write.csv(p2,"dataForD3/medakCaseTypeAndJudge.csv")


# Link 3 - Court Name and Judges
dim(filter(d,d$before_honourable_judges == ""))
table(d$before_honourable_judges)

p3 <- as.data.frame(table(d$court_name,d$before_honourable_judges))
dim(filter(p3,Freq == 0))
p3 <- filter(p3,Freq != 0)
names(p3) <- c("source","target","value")


# Link 4 - Judge and Status
p4 <- as.data.frame(table(d$before_honourable_judges,d$NewStatus))
dim(filter(p4,Freq == 0))
p4 <- filter(p4,Freq != 0)
View(p4)
View(p4 %>% spread(Var2,Freq))
names(p4) <- c("source","target","value")

###
# Merge
###

pAll <- rbind(p1,p2,p3,p4)
View(pAll)

write.csv(pAll,"../dataForD3/medakCaseTypeCourtJudgeAndStatus.csv",row.names=FALSE)
