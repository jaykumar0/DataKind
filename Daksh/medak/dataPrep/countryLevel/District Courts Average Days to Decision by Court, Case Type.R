setwd("C:/project/Daksh/dataWrangling/countryLevel")
#d1 <- read.csv("District Courts Cases by Court, District, Case Type.csv",header=T,sep=";")


#d1 <- read.csv("District Courts Cases by Court, District, Case Type.csv",header=T,sep=";")
d1 <- read.csv("../dataInput/District Courts Average Days to Decision by Court, Case Type.csv",header=T,sep=";",as.is=T)
d2 <- read.csv("../dataInput/District Courts Cases by Court, District, Case Type.csv",header=T,sep=";",as.is=T)

View(d1)
View(d2)

dim(d1)
dim(d2)
names(d1)
names(d2)
d <- merge(d1,d2,by=c("Court.Name","Case.Type"))
dim(d)
names(d)
d <- d[ ,c(5,1,2,6,7,3,4)]

View(d)

length(unique(d$Court.Name))
length(unique(d1$Court.Name))
table(d$State)
length(unique(d$Case.Type))
length(unique(d1$Case.Type))

str(d)
d$Pendency..Days. <-  as.integer(gsub(",","",d$Pendency..Days.))
d$Avg.Days.to.Decide <-  as.integer(gsub(",","",d$Avg.Days.to.Decide))
d$Cases.Decided <-  as.integer(gsub(",","",d$Cases.Decided))

table(d$State)
ds <- subset(d,State=="Karnataka")
ds <- subset(d,Court.Name=="ADDL. CITY CIVIL and SESSIONS JUDGES, MAYO HALL AND FTC, MAYOHALL")
View(ds)
length(unique(ds$Case.Type))
table(ds$Case.Type)
dim(ds)


##############################################################################################
###
### Plots
### http://flowingdata.com/2010/11/23/how-to-make-bubble-charts/
##############################################################################################
radius <- sqrt( ds$Cases.Decided/ pi )
symbols(ds$Pendency..Days., ds$Avg.Days.to.Decide, circles=ds$Cases.Decided)
symbols(ds$Pendency..Days., ds$Avg.Days.to.Decide, circles=radius)
symbols(ds$Pendency..Days., ds$Avg.Days.to.Decide, circles=radius,
        inches=0.35, fg="white", bg="red", xlab="Pendency Days", ylab="Days to Decide")

text(ds$Pendency..Days., ds$Avg.Days.to.Decide, ds$Case.Type, cex=0.5)

radius1 <- sqrt( ds$Cases/ pi )
symbols(ds$Pendency..Days., ds$Avg.Days.to.Decide, circles=ds$Cases)
symbols(ds$Pendency..Days., ds$Avg.Days.to.Decide, circles=radius1)
symbols(ds$Pendency..Days., ds$Avg.Days.to.Decide, circles=radius1,
        inches=0.35, fg="white", bg="blue", xlab="Pendency Days", ylab="Days to Decide")

text(ds$Pendency..Days., ds$Avg.Days.to.Decide, ds$Case.Type, cex=0.5)

### 3
radius2 <- sqrt( ds$Avg.Days.to.Decide/ pi )
symbols(ds$Pendency..Days., ds$Cases, circles=ds$Avg.Days.to.Decide)
symbols(ds$Pendency..Days., ds$Avg.Days.to.Decide, circles=radius2)
symbols(ds$Pendency..Days., ds$Avg.Days.to.Decide, circles=radius2,
        inches=0.35, fg="white", bg="blue", xlab="Pendency Days", ylab="Days to Decide")

text(ds$Pendency..Days., ds$Avg.Days.to.Decide, ds$Case.Type, cex=0.5)

##############################################################################################
###
### Plots2
###
##############################################################################################

#install.packages("treemap")

library(treemap)
library(RColorBrewer)
display.brewer.all()

ds <- subset(d,State=="Karnataka")
ds <- subset(d,Court.Name=="ADDL. CITY CIVIL and SESSIONS JUDGES, MAYO HALL AND FTC, MAYOHALL")

treemap(subset(d,State=="Bihar"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Avg.Days.to.Decide",
        type="value",palette="YlGnBu")

treemap(subset(d,State=="Bihar"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="YlGnBu")

treemap(subset(d,State=="Bihar"),
        index="Case.Type",
        vSize="Avg.Days.to.Decide",
        vColor="Pendency..Days.",
        type="value",palette="YlGnBu")


treemap(subset(d,State=="Karnataka"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Avg.Days.to.Decide",
        type="value",palette="Blues")

treemap(subset(d,State=="Karnataka"),
        index="Case.Type",
        vSize="Pendency..Days.",
        vColor="Cases",
        type="value",palette="Blues")

#################################################
#################################################
### Interesting
#################################################
#################################################
treemap(subset(d,State=="Karnataka"),
        index="Case.Type",
        vSize="Cases",
        vColor="Avg.Days.to.Decide",
        type="manual",palette="Oranges")

treemap(subset(d,State=="Karnataka"),
        index="Case.Type",
        vSize="Cases",
        vColor="Pendency..Days.",
        type="manual",palette="Oranges")

treemap(subset(d,State=="Kerala"),
        index="Case.Type",
        vSize="Cases",
        vColor="Avg.Days.to.Decide",
        type="value",palette="Blues")

treemap(subset(d,State=="Kerala"),
        index="Case.Type",
        vSize="Cases",
        vColor="Pendency..Days.",
        type="value",palette="Blues")

treemap(subset(d,State=="Tamil Nadu"),
        index="Case.Type",
        vSize="Cases",
        vColor="Avg.Days.to.Decide",
        type="manual",palette="Blues")

treemap(subset(d,State=="Tamil Nadu"),
        index="Case.Type",
        vSize="Cases",
        vColor="Pendency..Days.",
        type="manual",palette="Blues")

##############################################################################################
###
### Plots3
###
##############################################################################################
dim(d)

# Case Type is not unique
d$Case.Type <- paste(d$Court.Name," [",d$Case.Type,"]",sep="")

# To scale, remove non numeric

nums <- sapply(d, is.numeric)
nums
dNum <- d[ , nums]
dNonNum <- as.data.frame(d[ ,!nums])
head(dNum)
head(dNonNum)

Scaledd <- as.data.frame(scale(dNum))

dScaled <- cbind(dNonNum,Scaledd)

library(ggplot2)
library("reshape2")

table(d$State)


# need to melt 
dS <- subset(dScaled,State=="Assam",select=Case.Type:Cases.Decided)
View(dS)

d.m <- melt(subset(dScaled,State=="Karnataka",select=Case.Type:Cases.Decided))
View(d.m)
View(dScaled)
## d.m <- melt(dScaled)
## need to sclae else cases becomes white as they are less 
## in number than population

names(d.m) <- c("Names","variable","value")

# Plot 1
p <- ggplot(d.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value),colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue")
p

p1 <- ggplot(d.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value),colour = "light green") + 
  scale_fill_gradient(low = "white", high = "orange")
p1
ggplot(d.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value), colour = "white") + # white
  scale_fill_gradient(low = "white", high = "green") + # white and steelblue
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))

# with nums
ggplot(d.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value), colour = "white") + # white
  scale_fill_gradient(low = "white", high = "green") + # white and steelblue
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  geom_text(aes(fill = value, label = round(value, 1))) +
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))

#######################################################################
### OK - Case Types alone are not unique....results amy vary
#######################################################################

library(ggplot2)
library("reshape2")

# show numbers, dont scale

# Case Type is not unique
d$Case.Type <- paste(d$Court.Name," [",d$Case.Type,"]",sep="")

dn <- subset(d,State=="Karnataka",select=Case.Type:Cases.Decided)
View(dn)

dm <- melt(dn)
## need to sclae else cases becomes white as they are less 
## in number than population

names(dm) <- c("Names","variable","value")

ggplot(dm, aes(variable, Names)) + 
  geom_tile(aes(fill = value), colour = "white") + # white
  scale_fill_gradient(low = "white", high = "green") + # white and steelblue
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  geom_text(aes(fill = value, label = round(value, 1))) +
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))


### have links..do not del....







####################
###
### ver 2 - may not be required............
###
####################
library(ggplot2)
library("reshape2")

### subset and then scale....

dim(d)
table(d$State)
d$Case.Type <- paste(d$Court.Name," [",d$Case.Type,"]",sep="")
View(d)
dS <- subset(d,State=="Karnataka",select=Case.Type:Cases.Decided)
View(dS)

# To scale, remove non numeric

nums <- sapply(dS, is.numeric)
nums
dSNum <- dS[ , nums]
dSNonNum <- as.data.frame(dS[ ,!nums])
head(dSNum)
head(dSNonNum)

ScaleddSNum <- as.data.frame(scale(dSNum))

dSScaled <- cbind(dSNonNum,ScaleddSNum)
View(dSScaled)

# need to melt 
d.m <- melt(dSScaled)
d.m <- melt(dS)
View(d.m)
View(dScaled)
## d.m <- melt(dScaled)
## need to sclae else cases becomes white as they are less 
## in number than population

names(d.m) <- c("Names","variable","value")

# Plot 1
p <- ggplot(d.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value),colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue")
p

p1 <- ggplot(d.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value),colour = "light green") + 
  scale_fill_gradient(low = "white", high = "orange")
p1
ggplot(d.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value), colour = "white") + # light green
  scale_fill_gradient(low = "pink", high = "red") + # white and steelblue
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))

##################
library(d3heatmap)

dS1 <- dS

rownames(dS1) <- dS1[,1]
dS1[,1] <- NULL
names(dS)
dim(dS)
length(unique(dS$Case.Type))
d3heatmap(dS1, scale = "column")

##############################################################################################
###
### Plots4
### http://bicorner.com/2015/03/17/graphics-r-tricky/
### http://mlbernauer.com/R/20150309_treemaps_with_ggplot2.html
### http://blog.revolutionanalytics.com/2015/11/emojis-in-ggplot-graphics.html
### http://www.r-bloggers.com/emojis-in-ggplot-graphics/
### http://www.r-bloggers.com/phylomoji-with-ggtree/
### http://mlbernauer.com/R/20150309_treemaps_with_ggplot2.html
### http://www.r-bloggers.com/how-to-publish-r-and-ggplot2-to-the-web/
### http://inundata.org/2013/04/10/a-quick-introduction-to-ggplot2/
### 
##############################################################################################

##############################################################################################
###
### Plots5
###
##############################################################################################

library(ggplot2)
g <- ggplot(ds, aes(Case.Type, y=Pendency..Days.)) 
g + geom_bar(stat="identity")


ggplot(subset(d,State=="Karnataka"), aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
  scale_x_continuous(name="Total population")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")



library(ggplot2)
table(d$State)

d$Pendency..Days. <-  as.numeric(gsub(",","",d$Pendency..Days.))
colSums(Filter(is.numeric, d))

