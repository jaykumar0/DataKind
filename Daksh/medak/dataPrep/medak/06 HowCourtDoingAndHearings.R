library(tidyr)
###
# How is the court doing - Line and Bar chart for D3
###
q1 <- as.data.frame(table(format(d$date_filed,"%Y")))
q1
names(q1) <- c("Year","NewCasesFiled")

q2 <- as.data.frame(table(format(d$decision_date,"%Y")))
q2
names(q2) <- c("Year","CasesDisposed")
q2

q <- merge(x = q1, y = q2, by = "Year", all = TRUE)
q
q[is.na(q)] <- 0
q
q$TotPen <- cumsum(q$NewCasesFiled)
q
q$PendingFromPrevious <- q$TotPen - q$CasesDisposed - q$NewCasesFiled
q
q$TotPen <- NULL
q
write.csv(q, "../dataForD3/medakHowCourtIsDoing2.csv", row.names=FALSE)
q$Year <- paste(q$Year,"0101",sep="")
write.csv(q, "../dataForD3/medakHowCourtIsDoing.csv", row.names=FALSE)


###########################################################################
###########################################################################
###
# No of hearings
###

dim(filter(h,hearing_date == ""))
h$hearing_date <- as.Date(strptime(h$hearing_date, "%Y-%m-%d %H:%M:%S"))
h$hearing_date <- as.Date(strptime(h$hearing_date, "%Y-%m-%d %H:%M:%S"))
dim(filter(h,is.na(hearing_date)))
dim(h)
View(h)

r <- as.data.frame(table(h$hearing_date))
head(r)
sum(r$Freq)
7488 + 730
names(r) <- c("Date","NumOfHearing")
View(r)
#filter before 2011...v less...
write.csv(r,"dataForD3/MedakHearingNums.csv")


#h["hearing_date"][is.na(d["hearing_date"])] <- "2016-01-01"

###
# No of hearings by judge
###

h$before_honourable_judges <- ifelse(h$before_honourable_judges == "","No Judge",h$before_honourable_judges)

r2 <- as.data.frame(table(h1$hearing_date,h1$before_honourable_judges))
r2 <- as.data.frame(table(h$hearing_date,h$before_honourable_judges))

dim(r2)
r2 <- filter(r2,Freq != 0)


head(r2)
names(r2) <- c("Date","Judge","NumOfHearing")
View(r2)

table(r2$Judge)
r21 <- r2 %>% spread(Judge,NumOfHearing)
r21[is.na(r21)] <- 0
View(r21)

write.csv(r21,"dataForD3/MedakJudgeAndHearingAll.csv")



###############################################################

dim(h)
View(r)
table(c$next_hearing_date)
dim(h)
r1 <- as.data.frame(table(h$next_hearing_date))
r11 <- as.data.frame(table(c$next_hearing_date))
dim(r1)
r1 <- filter(r1,Freq != 0)

dim(r11)
r11 <- filter(r11,Freq != 0)

# add next hearing date in h...
h1$next_hearing_date <- as.Date(strptime(h1$next_hearing_date, "%Y-%m-%d %H:%M:%S"))
r2 <- as.data.frame(table(h1$next_hearing_date,h1$before_honourable_judges))
dim(r2)
r2 <- filter(r2,Freq != 0)
sum(r1$Freq)
head(r1)
names(r1) <- c("Date","NumOfHearing")

write.csv(r1,"dataForD3/MedakHearingNums.csv")

head(r2)
names(r2) <- c("Date","Judge","NumOfHearing")
View(r2)
r21 <- r2 %>% spread(Judge,NumOfHearing)
r21[is.na(r21)] <- 0
View(r21)

write.csv(r21,"dataForD3/MedakJudgeAndHearing.csv")

#d12 <- add_rownames(d12, "BeforeJudge")
#names(d12) <- c("BeforeeJudge",)
View(r1  %>% arrange(desc(Freq)))
View(r11  %>% arrange(desc(Freq)))

View(r2  %>% arrange(desc(Freq)))

# https://rpubs.com/bradleyboehmke/data_wrangling
d123 <- d12 %>% spread(Var2,Freq)
