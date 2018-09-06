###
# Summary with new status
###

dSummaryN <- d %>% group_by(CivilOrCriminal, NewStatus) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
View(dSummaryN)
dSummaryN[is.na(dSummaryN)] <- 0
colnames(dSummaryN)[1] <- "CaseType"
colnames(dSummaryN)[2] <- "Status" 
write.csv(dSummaryN,"../dataForD3/medakCorCSummaryStatus.csv",row.names=FALSE)
