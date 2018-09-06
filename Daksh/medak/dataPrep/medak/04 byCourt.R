library(dplyr)

###
# Total Cases - Case Type and Status
###
q1 <- as.data.frame(table(d$case_type,d$court_name))
View(q1)
names(q1) <- c("CaseType","Var2","Freq")
q12 <- q1 %>% spread(Var2,Freq)
View(q12)
write.csv(q12,"../dataForD3/medakCaseTypeAndCourt.csv",row.names=FALSE)
