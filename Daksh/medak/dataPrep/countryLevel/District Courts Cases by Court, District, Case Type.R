library(dplyr)
library(stringr)

setwd("C:/project/Daksh/dataWrangling/countryLevel")
#d1 <- read.csv("District Courts Cases by Court, District, Case Type.csv",header=T,sep=";")
d <- read.csv("../dataInput/District Courts Cases by Court, District, Case Type.csv",header=T,sep=";",as.is=T)

View(d)
str(d)
dim(d)
table(d$State)

d$Pendency..Days. <-  as.numeric(gsub(",","",d$Pendency..Days.))
colSums(Filter(is.numeric, d))


dSC <- as.data.frame((table(d$State, d$Case.Type)))
head(dSC)
str(dSC)
View(dSC)

dSum <- d %>% group_by(State, Court.Name,Case.Type) %>%
  summarise(TotalCases = sum(Cases))

View(dSum)
dSum[is.na(dSum)] <- 0
View(filter(d,Court.Name == "Addl. District Courts, Bhimavaram"))
View(filter(dSum,Court.Name == "Addl. District Courts, Bhimavaram"))
dim(d)
dim(dSum)

d1 <- select(d,State,Court.Name,Case.Type,Cases)
View(filter(d1,Court.Name == "Addl. District Courts, Bhimavaram"))

d1$Court.Name <- str_replace_all(d1$Court.Name,"\\.","-")
d1$Court.Name <- str_replace_all(d1$Court.Name,",","-")
View(d1)
d1$Case.Type <- str_replace_all(d1$Case.Type,",","-")
d1$Case.Type <- str_replace_all(d1$Case.Type,"\\.","")

names(d1)
names(d1) <- c("State","Court","CaseType","Cases")
View(d1)

unqSt <- as.character(unique(d1$State))
unqSt <- as.data.frame(unique(d1$State))
names(unqSt) <- "State"
unqSt
p1 <- cbind(TopLvl = "India",unqSt)
View(p1)
###
#
###
View(filter(d1,State %in% c("west Bengal","Chhattisgarh")))
View(filter(d1,State == "Chhattisgarh"))
View(filter(d1,State == "Rajasthan"))
View(filter(d1,Court == "District and Session Court- Raipur"))


###
# 
###
p1 <- as.data.frame(unique(d1$State))
names(p1) <- "id"
View(p1)

p1$id <- paste("India",p1$id,sep=".")
p1$value <- ""
View(p1)


###
#
###
p2 <- as.data.frame(paste(unique(d1$State),unique(d1$Court),sep = "."))
View(p2)
names(p2) <- "id"
p2$value <- ""
p2$id <- paste("India",p2$id,sep=".")
View(p2)

p2 <- as.data.frame(paste(d1$State,d1$Court,sep = "."))
names(p2) <- "id"
p2$id <- paste("India",p2$id,sep=".")
p2$value <- ""
dim(p2)
View(p2)
p21 <- p2[duplicated(p2), ]
p21 <- unique(p2) 
dim(p2)
dim(p21)
###
#
###
all <- as.data.frame(paste("India",d1$State,d1$Court,d1$CaseType,sep = "."))
View(all)
d1$all <- paste("India",d1$State,d1$Court,d1$CaseType,sep = ".")
View(d1)

p3 <- select(d1,all,Cases)
names(p3) <- c("id","value")
View(p3)

###
# Merge
###

pAll <- rbind(p1,p21,p3)
pAll <- rbind(c("India",""),p1,p21,p3)

View(filter(pAll,id == "India.Chhattisgarh.District and Session Court- Raipur.MACT"))
View(filter(pAll,id == "India.Chhattisgarh.District and Session Court- Raipur"))
dim(pAll)
p <- filter(pAll,id != "India.Chhattisgarh.District and Session Court- Raipur.MACT") 
dim(p)
dim(pAll)
#colnames(dSummaryN)[1] <- "CaseType"
#colnames(dSummaryN)[2] <- "Status" 
write.csv(p,"../dataForD3/IndiaAllCourts.csv",row.names=FALSE)


View(filter(p,id == "India.Chhattisgarh"))
View(filter(p,id == "India.Rajasthan.ACJM Tijara"))
View(p21)
View(filter(d1,State == "Rajasthan"))

###
# State and Case Type ONLY
###
d1 <- select(d,State,Case.Type,Cases)

d1$Case.Type <- str_replace_all(d1$Case.Type,",","-")
d1$Case.Type <- str_replace_all(d1$Case.Type,"\\.","")

names(d1)
names(d1) <- c("State","CaseType","Cases")
View(d1)

dSum <- d1 %>% group_by(State, CaseType) %>%
  summarise(TotalCases = sum(Cases))

View(dSum)
dSum[is.na(dSum)] <- 0

###
# 
###
p1 <- as.data.frame(unique(dSum$State))
names(p1) <- "id"
View(p1)

p1$id <- paste("India",p1$id,sep=".")
p1$value <- ""
View(p1)


dSum$all <- paste("India",dSum$State,dSum$CaseType,sep = ".")
View(dSum)

p2 <- select(dSum,all,TotalCases)
p2$State <- NULL
names(p2) <- c("id","value")
View(p2)

###
# Merge
###

p <- rbind(c("India",""),p1,p2)
View(p)
write.csv(p,"../dataForD3/StateCaseType.csv",row.names=FALSE)


View(filter(p,id == "India.Chhattisgarh"))
View(filter(p,id == "India.Rajasthan.ACJM Tijara"))
View(p21)
View(filter(d1,State == "Rajasthan"))












######
#### Get a subset 
######
table(d$State)
kar <- subset(d,State=="Karnataka")
dim(kar)
###
table(d$State,d$Cases)
sc <- aggregate(Cases ~ State,d , sum)
##
View(sc)
write.csv(kar, "kar.csv", row.names=FALSE)
write.csv(sc, "state.csv", row.names=FALSE)

head(kar)

colSums(kar$Cases)
apply(kar$Cases,1,sum)

colSums(Filter(is.numeric, kar))
is.numeric(kar$Pendency..Days.)


pd <- as.character(kar$Pendency..Days.)
pd
class(pd)
as.numeric(pd[3])
as.numeric(gsub(",","",pd))

pd[3] <- "1363"

r<-"1200"
as.numeric(gsub(",","",r))


##############################################################################################
###
### Tree map
###
##############################################################################################

#install.packages("treemap")

library(treemap)
library(RColorBrewer)
display.brewer.all()

### For Kar
View(kar)
treemap(kar,
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="YlGnBu")

treemap(kar,
        index="Case.Type",
        vSize="Cases",
        vColor="Pendency..Days.",
        type="value",palette="Oranges")

table(d$State)
length(unique(d$State))

treemap(kar,
        index="Court.Name",
        vSize="Cases",
        vColor="Pendency..Days.",
        type="index",palette="Oranges")

treemap(kar,
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Oranges")

treemap(kar,
        index="Case.Type",
        vSize="Cases",
        vColor="Pendency..Days.",
        type="value",palette="Oranges")

########################################################################
### 
########################################################################

treemap(subset(d,State=="Bihar"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="YlGnBu")

treemap(subset(d,State=="Punjab"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Blues")

treemap(subset(d,State=="Tamil Nadu"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Oranges")

treemap(subset(d,State=="Kerala"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="manual",palette="Pastel2")

treemap(subset(d,State=="Telangana"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Greens")

treemap(subset(d,State=="Andhra Pradesh"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="BuPu")

treemap(subset(d,State=="Chhattisgarh"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Purples")

treemap(subset(d,State=="Chhattisgarh"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Purples")

### For all data
treemap(d,
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Blues")

treemap(d,
        index="State",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Blues")

##############################################################################################
###
### Tree map - help
###
##############################################################################################
# http://spatial.ly/2011/08/improved-tree-maps-with-r/
#A treemap generally requires 4 pieces of information:
# the item- in this case the London Borough's or "id"- each will be assigned a rectangle,
# a value to scale the size of the rectangle by- in this case the population or "pop",
# a value for assigning the colour- in this case the average earnings per person or "earnings",
# and a broader group to which the item belongs- in this case the ruling political party or "party".


library(RColorBrewer)
library(portfolio)

map.market(d$Case.Type, d$Cases, d$State, d$Pendency..Days., lab = c(TRUE, TRUE), main="District Courts Cases by Court, District, Case Type")
map.market(kar$Case.Type, kar$Cases, kar$State, kar$Pendency..Days., lab = c(TRUE, TRUE), main="Karnataka District Courts Cases by Court, District, Case Type")

#install.packages("treemap")
library(treemap)
library(RColorBrewer)
display.brewer.all()

### example 
data(GNI2010)
tmPlot(GNI2010,
       index="iso3",
       vSize="population",
       vColor="GNI",
       type="value")

treemap(GNI2010,
        index="iso3",
        vSize="population",
        vColor="GNI",
        type="value")

data(business)
business <- transform(business, data.available = factor(!is.na(turnover)), x = 1)
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="x",
        vColor="data.available",
        type="categorical")


# color treemap
business$color <- rainbow(nlevels(business$NACE2))[business$NACE2]
treemap(business,
        index=c("NACE1", "NACE2"), 
        vSize="x",
        vColor="color",
        type="color")

business$color <- rainbow(nlevels(business$NACE2))[business$NACE2]
treemap(business,
        index=c("NACE1", "NACE2"), 
        vSize="turnover",
        vColor="employees",
        type="manual",
        palette=terrain.colors(10))

##############################################################################################
###
### Cat Heat Map
###
##############################################################################################
library(ggplot2)

# Create a counts table 
table(d$State, d$Case.Type)
x <- table(d$Case.Type)
View(x)
length(unique(d$Case.Type))

# Save this to a data frame:
dSC <- as.data.frame((table(d$State, d$Case.Type)))
head(dSC)
str(dSC)
View(dSC)
dSC <- as.data.frame((table(kar$Court.Name, kar$Case.Type)))

g <- ggplot(dSC, aes(x = Var2, y = Var1)) + geom_tile(aes(fill = Freq)) + theme(axis.title.y = element_blank(),axis.title.x = element_blank())

g + scale_fill_gradient(name="Total State and Case Type",low="light blue", high="red")
g + scale_fill_gradient(name="Total State and Case Type",low="light blue", high="black")
g + scale_fill_gradient(name="Total State and Case Type",low="pink", high="red")

g + scale_fill_gradient(name="Total State and Case Type",low="light yellow", high="light green")
g + scale_fill_gradient(name="Total State and Case Type",low="light blue", high="light green")

g + scale_fill_gradient(name="Total State and Case Type",low="light blue", high="red")
g + scale_fill_gradient(name="Total State and Case Type",low="light blue", high="blue")


p <- ggplot(dSC, aes(x = Var1, y = Var2)) + geom_tile(aes(fill = Freq)) + theme(axis.title.y = element_blank(),axis.title.x = element_blank())
p + scale_fill_gradient(name="Total State and Case Type",low="light blue", high="black")
