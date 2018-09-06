# http://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/
# https://www.kaggle.com/timevans/prudential-life-insurance-assessment/xgboost-example-0-61249
# http://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
# https://blog.ouseful.info/2016/05/02/when-documents-become-databases-tabulizer-r-wrapper-for-tabula-pdf-table-extractor/
# http://ml4a.github.io/classes/itp-S16/

cor(d$AgeinDays,d$DaysToFirstHearing,na.rm=TRUE)
cor(d$AgeinDays,d$DaysToFirstHearing,use="complete.obs")

d$Weekday <- weekdays(d$date_filed)
d$Month <- months(d$date_filed)
table(d$Weekday)
table(d$Month)



p <- select(d,case_type,court_name,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NumHearing,
            AvgDaysBetweenHearing,NewStatus,Weekday,Month)
View(p)
###
# remove Pending cases
###
#p <- na.omit(p) 
dim(p)
dim(d)
table(p$NewStatus)
table(d$NewStatus)
###
#
###
library(VIM)
aggr_plot <- aggr(p, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))


aggr_plot <- aggr(d, col=c("skyblue", "red", "orange"), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))



plot(d, col = c("skyblue", "red", "orange"), bars = TRUE,
     numbers = FALSE, prop = TRUE, combined = FALSE, varheight = FALSE,
     only.miss = FALSE, border = par("fg"), sortVars = FALSE,
     sortCombs = TRUE, ylabs = NULL, axes = TRUE, labels = axes,
     cex.lab = 1.2, cex.axis = par("cex"), cex.numbers = par("cex"),
     gap = 4)

###
#
###
#install.packages("DataExplorer")
library(DataExplorer)

GenerateReport(iris)
GenerateReport(p)
GenerateReport(d)
PlotMissing(p)
PlotMissing(d)

p1 <- SplitColType(p)
names(p1)

p1$discrete
p1$continuous

p1$num_discrete
p1$num_continuous
p1$num_all_missing

### Plot continuous values - corelation 
plot(p1$continuous,col="salmon")

# Basic Scatterplot Matrix
names(p1$continuous)
pairs(~AgeinDays+DaysToFirstHearing+NumHearing+AvgDaysBetweenHearing,data=p,
      main="Simple Scatterplot Matrix")

# First Correlogram Example
#install.packages("corrgram")
library(corrgram)
corrgram(p1$continuous, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order") 


corrgram(p1$continuous, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Car Milage Data in PC2/PC1 Order")

corrgram(p1$continuous, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Car Milage Data (unsorted)")

cor(p1$continuous,use="complete.obs")
# missing values
###
# Get the columns with missing values
###
#dM <-sapply(train, function(y) sum(length(which(is.na(y)))))
dM <- sapply(p,function(x) sum(is.na(x)))
dM <- data.frame(dM)
dM

dM <- add_rownames(dM, "Cols")
names(dM) <- c("Column","NumMissingValues")
dM

###
# filter p
###

q <- filter(p,NewStatus != "Pending")
table(q$NewStatus)
PlotMissing(q)

q1 <- na.omit(q) 
PlotMissing(q1)

table(q1$NewStatus)

q12 <- SplitColType(q1)
names(q12)
### Plot continuous values - corelation 
plot(q12$continuous,col="salmon")
cor(q12$continuous)
names(q12$continuous)
# http://stackoverflow.com/questions/10680658/how-can-i-create-a-correlation-matrix-in-r

set.seed(1)
x <- data.frame(q1$AgeinDays,q1$DaysToFirstHearing)
y <- data.frame(q1$NumHearing,q1$AvgDaysBetweenHearing)

COR <- cor(x,y)
COR
image(x=seq(dim(x)[2]), y=seq(dim(y)[2]), z=COR, xlab="x column", ylab="y column")
text(expand.grid(x=seq(dim(x)[2]), y=seq(dim(y)[2])), labels=round(c(COR),2))

# http://stackoverflow.com/questions/5453336/plot-correlation-matrix-into-a-graph?lq=1
install.packages("corrplot")
library(corrplot) #package corrplot
m <- cor(q12$continuous)
m
corrplot(m)
corrplot(m, method = "circle") #plot matrix
corrplot(m, method = "number", col = "black", cl.pos = "n")
pairs(q12$continuous)

###
# average age
###

summary(q)
summary(q1)
###
#
###
cor(p$AgeinDays,p$DaysToFirstHearing,use="complete.obs")
cor(p$AgeinDays,p$NumHearing,use="complete.obs")
cor(p$AgeinDays,p$AvgDaysBetweenHearing,use="complete.obs")

chisq.test(p$case_type,p$court_name)
chisq.test(p$case_type,p$CivilOrCriminal)
chisq.test(p$CivilOrCriminal,p$court_name)


#predict age

table(d$current_stage)
table(d$current_stage,d$NewStatus)

############################################################
############################################################
############################################################

library(ggplot2)
p <- select(d,case_type,court_name,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NumHearing,
            AvgDaysBetweenHearing,NewStatus,Weekday,Month)

q <- filter(p,NewStatus != "Pending")
table(q$NewStatus)

df <- na.omit(q)
df <- q1

hist(df$AgeinDays)
hist(log(df$AgeinDays))

hist(df$DaysToFirstHearing)
hist(log(df$DaysToFirstHearing))

hist(df$NumHearing)
hist(log(df$NumHearing))

hist(df$AvgDaysBetweenHearing)
hist(log(df$AvgDaysBetweenHearing))


a <- log(10)
exp(a)

hist(df$Weekday)
hist(log(df$Weekday))

hist(df$Month)
hist(log(df$Month))

hist(df$case_type)
hist(log(df$case_type))

hist(df$court_name)
hist(log(df$court_name))

hist(df$CivilOrCriminal)
hist(df$NewStatus)
hist(log(df$NewStatus))

# http://www.r-bloggers.com/how-to-make-a-histogram-with-ggplot2/
# https://plot.ly/ggplot2/histograms/
# http://www.cookbook-r.com/Graphs/Plotting_distributions_%28ggplot2%29/
# 