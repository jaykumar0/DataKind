###
# 01 judge and Case Type
###
# for chord
q1 <- as.data.frame(table(d$before_honourable_judges,d$case_type))
View(q1)
q1 <- filter(q1,Freq != 0)

# https://rpubs.com/bradleyboehmke/data_wrangling
q12 <- q1%>% spread(Var2,Freq)
colnames(q12)[1] <- "BeforeJudge" 
#rename(q12,Var1 = "BeforeJudge")
q12[is.na(q12)] <- 0
View(q12)

write.csv(q12,"../dataForD3/medakJudgeAndCaseType.csv",row.names=FALSE)

# for Sankey

# Link 1 - Case Type and Judges
p1 <- as.data.frame(table(d$case_type,d$before_honourable_judges))
dim(filter(d,d$before_honourable_judges == ""))
table(d$before_honourable_judges)

dim(filter(p1,Freq == 0))
p1 <- filter(p1,Freq != 0)
names(p1) <- c("source","target","value")

# Link 2 - Judge abd C or C [ Run C or C before to get the flag
p2 <- as.data.frame(table(d$before_honourable_judges,d$CivilOrCriminal))
dim(filter(p2,Freq == 0))
p2 <- filter(p2,Freq != 0)
names(p2) <- c("source","target","value")
###
# Merge
###

pAll <- rbind(p1,p2)
View(pAll)

write.csv(pAll,"../dataForD3/medakCaseTypeJudgeAndCorC.csv",row.names=FALSE)

