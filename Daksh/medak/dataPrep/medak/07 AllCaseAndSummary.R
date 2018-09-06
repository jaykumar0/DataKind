# http://www.r-bloggers.com/using-r-quickly-calculating-summary-statistics-with-dplyr/
# http://genomicsclass.github.io/book/pages/dplyr_tutorial.html
# https://rpubs.com/justmarkham/dplyr-tutorial
# http://analyticspro.org/2015/09/18/machine-learning-accuracy-memorization-vs-learning/
# http://r4ds.had.co.nz/transform.html

###
# Select Columns
###

sc <- select(d,combined_case_number,case_type,NewStatus,
             DaysToFirstHearing, NumHearing, AvgDaysBetweenHearing, AgeinDays)


View(sc)
# mark na as 0
sc[is.na(sc)] <- 0

#sc$AvgDaysBetweenHearing <- round(sc$AvgDaysBetweenHearing,0)
#sc$DaysToFirstHearing <- abs(d$DaysToFirstHearing)

write.csv(sc,"../dataForD3/medakAllCases.csv",row.names=FALSE)

###
# Summary with new status
###

dSummaryN <- d %>% group_by(case_type, NewStatus) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())
View(dSummaryN)
dSummaryN[is.na(dSummaryN)] <- 0
colnames(dSummaryN)[1] <- "CaseType"
colnames(dSummaryN)[2] <- "Status" 
write.csv(dSummaryN,"../dataForD3/medakSummaryStatus.csv",row.names=FALSE)

