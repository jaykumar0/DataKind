### District Court cases - Counts by District, with Census Data
# http://stats.stackexchange.com/questions/30941/are-tree-map-diagrams-effective-at-conveying-information
# https://www.interworks.com/blog/ccapitula/2014/10/14/tableau-essentials-chart-types-treemap


setwd("C:/workspace-R2015/daksh/DakshViz")

# 

#d <- read.csv("District Courts - Pending Cases bracketed in Five-year buckets.csv",header=T,sep=";",na.strings=c("","NA"))
d <- read.csv("District Courts - Pending Cases bracketed in Five-year buckets.csv",header=T,sep=";",as.is=T)
dim(d)
View(d)
names(d)

d$X..5.years <-  as.numeric(gsub(",","",d$X..5.years))
d$X5.to.10.years <-  as.numeric(gsub(",","",d$X5.to.10.years))
d$X10.to.15.years <-  as.numeric(gsub(",","",d$X10.to.15.years))
d$X..15.years <-  as.numeric(gsub(",","",d$X..15.years))

d[is.na(d)] <- 0
d$TotalCases <- d$X..5.years + d$X5.to.10.years + d$X10.to.15.years + d$X..15.years

colSums(Filter(is.numeric, d))

## For hest map - x
x <- d[order(-d$TotalCases),]
View(x)
##
d <- d[order(-d$X..5.years,-d$X5.to.10.years,-d$X10.to.15.years,-d$X..15.years),]
d1 <- d[order(-d$X5.to.10.years,-d$X10.to.15.years,-d$X..15.years,-d$X..5.years),]
d2 <- d[order(-d$X10.to.15.years,-d$X..15.years,-d$X..5.years,-d$X5.to.10.years),]
d3 <- d[order(-d$X..15.years,-d$X..5.years,-d$X5.to.10.years,-d$X10.to.15.years),]

rowsum(d)

View(d)
View(d1)
View(d2)
View(d3)

##############################################################################################
###
### Plots
###
##############################################################################################
### Treemap

library(treemap)

pd <- head(d,50)
treemap(pd,
        index="Court",
        vSize="X..5.years",
        vColor="TotalCases",
        type="manual", palette="Blues")

treemap(pd,
        index="Court",
        vSize="TotalCases",
        vColor="X..5.years",
        type="value") #,palette="Blues")

pd <- head(d1,50)
treemap(pd,
        index="Court",
        vSize="X5.to.10.years",
        vColor="TotalCases",
        type="manual", palette="Blues")
      #  type="value")

pd <- head(d2,40)
treemap(pd,
        index="Court",
        vSize="X10.to.15.years",
        vColor="TotalCases",
        type="manual", palette="Blues")

pd <- head(d3,20)
treemap(pd,
        index="Court",
        vSize="X..15.years",
        vColor="TotalCases",
        type="manual", palette="Blues")

treemap(pd,
        index="Court",
        vSize="X..15.years",
        vColor="TotalCases",
        type="index")

treemap(pd,
        index="Court",
        vSize="X..15.years",
        vColor="TotalCases",
        type="index",palette="-RdYlBu")

treemap(pd,
        index="Court",
        vSize="X..15.years",
        vColor="TotalCases",
        type="value",palette="Blues")

treemap(pd,
        index="Court",
        vSize="X..15.years",
        vColor="TotalCases",
        type="manual", palette="Blues",range = c(1,2))

YlOrRd
###
library(ggplot2)

ggplot(d, aes(x=Court, y=X..5.years, fill=X5.to.10.years)) + geom_boxplot()



head(d)
dim(d)
length(unique(d$Court))
summary(d)

# remove duplicates
d <- d[!duplicated(d$Court.District), ]
dim(d)
View(d)
dim(d)
# remove NA's
d1 <- na.omit(d)
dim(d1)
str(d1)
View(d1)
summary(d1$Total.Population)


#############
d1 <- read.csv("DistrictCourt-YearstoDecide.csv",header=T,sep=";")
dim(d1)
head(d1)
View(d1)
names(d1)
names(d)

##############################################################################
library(ggplot2)
library("reshape2")

m <- melt(d[1:30,])
m <- melt(d1[1:30,])
m <- melt(d2[1:30,])
m <- melt(d3[1:30,])

# To scale, remove non numeric
d <- x
d$TotalCases <- NULL
head(d)
nums <- sapply(d, is.numeric)
nums
dNum <- d[ , nums]
dNonNum <- as.data.frame(d[ , !nums])

dNumS <- as.data.frame(scale(dNum))

dS <- cbind(dNonNum,dNumS)
colnames(dS)[1] <- 'Court'
names(dS)
View(d)
## Get Top 50 ..otherwise...too many...
d2 <- dS[1:50,]
d2 <- dS[51:100,]
d2 <- dS[101:150,]
d2 <- dS[151:200,]
d2 <- dS[201:250,]
d2 <- dS[251:300,]
d2 <- dS[291:340,]
d2 <- dS
# then order by district
d2 <- d2[order(d2$Court),]
#View(d2)
m <- melt(d2)


head(m)
dim(m)
dim(d)

ggplot(m, aes(variable, Court)) + 
  geom_tile(aes(fill = value), colour = "white") + # light green
  scale_fill_gradient(low = "white", high = "orange") + # white and orange
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))

####################################################################################
####################################################################################
####################################################################################

library(dplyr)
names(d)

names(d) <- c("Court","FiveYears","FiveTo10Years","TenTo15Years","MoreThan15Years")
View(d)

str(d)

MoreThan15 <- filter(d,MoreThan15Years > 0)
View(MoreThan15)
MoreThan15$FiveYears <- NULL
MoreThan15$FiveTo10Years <- NULL
MoreThan15$TenTo15Years <- NULL

write.csv(MoreThan15,"dataForD3/MoreThan15Years.csv")

###
#
###
TenTo15 <- filter(d,TenTo15Years >= 100)
View(TenTo15)
dim(TenTo15)
TenTo15 <- filter(d,TenTo15Years != 0)
View(TenTo15)
TenTo15$FiveYears <- NULL
TenTo15$FiveTo10Years <- NULL
TenTo15$MoreThan15Years <- NULL

{ "name": "Warangal" , "size": 7860  }
?shQuote

TenTo15$a <- paste(c("{",shQuote(TenTo15$Court), ":","size" , ":",shQuote(TenTo15$TenTo15Years)))

TenTo15$b <- paste(c("{",TenTo15$Court, ":","size" , ":",TenTo15$TenTo15Years))

TenTo15$b <- paste("{ name : ",TenTo15$Court," , size : ", TenTo15$TenTo15Years, " },")
getwd()
write.csv(TenTo15,"dataForD3/TenTo15Years.csv")


(shQuote(TenTo15$Court))
