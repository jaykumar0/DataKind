library(tidyr)

###
#
###

# Day of week filed, day of week closed, month filed, month closed....

d$WeekDay <- weekdays(d$date_filed)
d$Month <- months(d$date_filed)
# Order Weekday 
d$WeekDay <- factor(d$WeekDay, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# Order Month
d$Month <- factor(d$Month, ordered=TRUE, levels=c("January", "February", "March", "April", "May", "June", "July","August","September",
                                                  "October","November","December"))
###
# for sankey, to dofferentiate between filed and decision months and weekday(abbreviated and May is Mai)
###
#d$DecisionDay <- weekdays(d$decision_date,abbreviate = T)
#d$DecisionMonth <- months(d$decision_date,abbreviate = T)

# Order Decision Weekday 
#d$DecisionDay <- factor(d$DecisionDay, ordered=TRUE, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
# Order Decision Month
#d$DecisionMonth <- factor(d$DecisionMonth, ordered=TRUE, levels=c("Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul","Aug","Sep",
                                                                  "Oct","Nov","Dec"))

###
# for Line/Bar
###
d$DecisionDay <- weekdays(d$decision_date)
d$DecisionMonth <- months(d$decision_date)
# Order Weekday 
d$DecisionDay <- factor(d$DecisionDay, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# Order Month
d$DecisionMonth <- factor(d$DecisionMonth, ordered=TRUE, levels=c("January", "February", "March", "April", "May", "June", "July","August","September",
                                                                  "October","November","December"))



###
# Weekday Filed and Decided count
###

q1 <- as.data.frame(table(d$WeekDay,d$DecisionDay))

q1 <- as.data.frame(table(d$WeekDay))
View(q1)
names(q1) <- c("WeekDay","FiledCount")

q2 <- as.data.frame(table(d$DecisionDay))
names(q2) <- c("WeekDay","DecisionCount")
View(q2)

q12 <- full_join(q1,q2,by = "WeekDay")
View(q12)

write.csv(q12,"../dataForD3/medakWeekDayAndCount.csv",row.names=FALSE)

###
# Month Filed and Decided count
###
p1 <- as.data.frame(table(d$Month))
View(p1)
names(p1) <- c("Month","FiledCount")

p2 <- as.data.frame(table(d$DecisionMonth))
names(p2) <- c("Month","DecisionCount")
View(q2)

p12 <- full_join(p1,p2,by = "Month")
View(p12)

write.csv(p12,"../dataForD3/medakMonthAndCount.csv",row.names=FALSE)

#######################################################################################
#
#######################################################################################
# for line
p1 <- as.data.frame((table(d$case_type,d$WeekDay)))
p12 <- p1 %>% spread(Var2,Freq)
View(p12)
write.csv(p12,"dataForD3/medakWeekDayCreatedAndCaseTypeForLine.csv")

p11 <- as.data.frame((table(d$WeekDay,d$case_type)))
p112 <- p11 %>% spread(Var2,Freq)
View(p112)
write.csv(p112,"dataForD3/medakWeekDayCreatedAndCaseTypeForLine2.csv")


p2 <- as.data.frame((table(d$case_type,d$Month)))
p21 <- p2 %>% spread(Var2,Freq)
View(p21)
write.csv(p21,"dataForD3/medakMonthCreatedAndCaseTypeForLine.csv")

p4 <- as.data.frame((table(d$Month,d$case_type)))
p41 <- p4 %>% spread(Var2,Freq)
View(p41)
write.csv(p41,"dataForD3/medakMonthCreatedAndCaseType2ForLine.csv")

### Decison date and Month
p5 <- as.data.frame((table(d$case_type,d$DecisionDay)))
p51 <- p5 %>% spread(Var2,Freq)
View(p51)
write.csv(p51,"dataForD3/medakWeekDayDecidedAndCaseTypeForLine.csv")

p6 <- as.data.frame((table(d$DecisionDay,d$case_type)))
p61 <- p6 %>% spread(Var2,Freq)
write.csv(p61,"dataForD3/medakWeekDayDecidedAndCaseType2ForLine.csv")

p7 <- as.data.frame((table(d$DecisionMonth,d$case_type)))
p71 <- p7 %>% spread(Var2,Freq)
write.csv(p71,"dataForD3/medakMonthDecidedAndCaseType2ForLine.csv")




####
p1 <- as.data.frame(table(d$case_type,d$WeekDay))
View(p1)
p12 <- p1 %>% spread(Var2,Freq)
View(p12)
p1 <- as.data.frame(table(d$case_type,d$DecisionDay))

as.data.frame(table(d$case_type)) %>% arrange(desc(Freq))


