###
# For Sankey
###

# Link 1 - Cor C - [ Run CorC to get the filed first ]
table(d$CivilOrCriminal)
table(d$CivilOrCriminal)


p1 <- as.data.frame(table(d$CivilOrCriminal)) 
View(p1)
p1 <- cbind(Var11 = "Medak",p1)

dim(filter(p1,Freq == 0))
names(p1) <- c("source","target","value")



# Link 2 - Medak and Case Types 
p2 <- as.data.frame(table(d$CivilOrCriminal,d$case_type))
View(p2)
dim(filter(p2,Freq == 0))
p2 <- filter(p2,Freq != 0)
names(p2) <- c("source","target","value")

# Link 3 - Case Type and Judges
p3 <- as.data.frame(table(d$case_type,d$before_honourable_judges))
dim(filter(d,d$before_honourable_judges == ""))
table(d$before_honourable_judges)

dim(filter(p3,Freq == 0))
p3 <- filter(p3,Freq != 0)
names(p3) <- c("source","target","value")

# Link 4 - Judge and Court

p4 <- as.data.frame(table(d$before_honourable_judges,d$court_name))
dim(filter(p4,Freq == 0))
p4 <- filter(p4,Freq != 0)
View(p4 %>% spread(Var2,Freq))
names(p4) <- c("source","target","value")

# Link 5 - Court and Status
p5 <- as.data.frame(table(d$court_name,d$NewStatus))
dim(filter(p5,Freq == 0))
p5 <- filter(p5,Freq != 0)
names(p5) <- c("source","target","value")

###
# Merge
###

pAll <- rbind(p1,p2,p3,p4,p5)
View(pAll)

write.csv(pAll,"../dataForD3/medakCorCCaseTypeJudgeCourtAndStatus.csv",row.names=FALSE)
