# http://zynata.com/base/src/index.html#/access/signin?portal=dakshlegal.in

### District Court cases - Counts by District, with Census Data

library(dplyr)
setwd("C:/project/Daksh/dataWrangling/countryLevel")

# no need - d1
d1 <- read.csv("../dataInput/District Courts Cases by Court, District, Case Type.csv",header=T,sep=";",as.is=T)

d <- read.csv("../dataInput/District Court cases - Counts by District, with Census Data.csv",header=T,sep=";")
dim(d)
write.csv(d, "districtAll.csv", row.names=FALSE)

View(d)
View(d1)
# remove duplicates
dim(d)
length(unique(d$Court.District))
d <- d[!duplicated(d$Court.District), ]
dim(d)

######
### remove NA - no need
######
d1 <- na.omit(d) 
dim(d1)
dim(d)
View(d)
View(d1)

d2 <- select(d,Court.District,Cases)
View(d2)
d3 <- read.csv("C:/data/list_of_district_of_india-1587j.csv",header=T,sep=",",as.is=T)
View(d3)
d4 <- select(d3,State,District)
View(d4)
d4$St <- gsub('.{4}$', '', d4$State)
d4$State <- NULL
names(d4) <- c("District","State")


View(d2)
names(d2) <- c("District","Cases")
d241 <- inner_join(d2, d4)
View(d241)

###
# Filter d241 for some states...
###
str(d241)
View(filter(d241,State == "Chhattisgarh"))
View(filter(d241,District == "Warangal" & State == "Telengana"))
table(d241$State)
###
# Define function
# returns string w/o leading or trailing whitespace
###
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
d241$State <- trim(d241$State)


d24 <- filter(d241,State %in% c("Chhattisgarh","Andhra Pradesh","Haryana","Jharkhand","Punjab","Himachal Pradesh"))
d24 <- filter(d241,State == "Chhattisgarh")

View(d24)
###
# format d24 for d3v4 cat treemap 
###

###
p1 <- as.data.frame(unique(d24$State))
names(p1) <- "id"
View(p1)
p1$id <- paste("India",p1$id,sep=".")
p1$value <- ""
View(p1)

d24$all <- paste("India",d24$State,d24$District,sep = ".")
View(d24)

p2 <- select(d24,all,Cases)
names(p2) <- c("id","value")
View(p2)

p12 <- rbind(p1,p2)
p12 <- rbind(c("India",""),p1,p2)
View(p12)

write.csv(p12,"../dataForD3/HighDensityStateDistrictCases.csv",row.names=FALSE)
#write.csv(p12,"../dataForD3/StateDistrictCases.csv",row.names=FALSE)

p2 <- as.data.frame(paste(d24$State,d24$District,sep = "."))
names(p2) <- "id"
p2$id <- paste("India",p2$id,sep=".")
p2$value <- ""
dim(p2)
View(p2)
p21 <- p2[duplicated(p2), ]
p21 <- unique(p2) 
dim(p2)
dim(p21)



#####################################################################################
#####################################################################################
#OLD - write.csv(d, "districtAll.csv", row.names=FALSE)
#write.csv(d2,"../dataForD3/districtCases.csv",row.names=FALSE)

library(dplyr)
q <- select(d,c(Court.District,Cases))
names(q) <- c("Court","TotCase")
dim(q)


q$js <- paste("{ name:", "\"",q$Court,"\"", "size :",q$TotCase," },")
View(q)
write.csv(q, "district.csv", row.names=FALSE)

x <- d
x <- x[order(x$Court.District),]
View(x)



########################################################
### Go to FINAL

# remove NA's
d1 <- na.omit(d)
dim(d1)


View(d)
names(d)
d <- d[order(-d$Cases,-d$Total.Population),]
head(d)
dim(d)
length(unique(d$Court.District))
summary(d)

dim(d)
View(d)
dim(d)
str(d1)
View(d1)
summary(d1$Total.Population)

##############################################################################################
###
### Bubble Plot
###
##############################################################################################
ggplot(d1, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
  scale_x_continuous(name="Murders per 1,000 population", limits=c(0,12))+
  scale_y_continuous(name="Burglaries per 1,000 population", limits=c(0,1250))+
  geom_text(size=4)+
  theme_bw()


ggplot(d1, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
  scale_x_continuous(name="Total population")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

######
### With Limit
######
ggplot(d1, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
  scale_x_continuous(name="Total population")+
  scale_y_continuous(name="Total cases",limits=c(0,1000))+
  geom_text(size=4)+
  theme_bw()

### With Limit - 2
ggplot(d1, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 200) +
  scale_x_continuous(name="Total population")+
  scale_y_continuous(name="Total cases",limits=c(0,1000))+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

### With Limit - 3
ggplot(d1, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
  scale_x_continuous(name="Total population",limits=c(1065000,5297001))+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() 

# v good
# http://docs.ggplot2.org/current/coord_cartesian.html
+ coord_cartesian(ylim = c(1065000, 5297001))


### try this
### http://stackoverflow.com/questions/11517411/ggplot2-how-to-manually-adjust-scale-area


##############################################################################################
###
### Heat Map 1
### https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
##############################################################################################
str(d1)
names(d1)
d1 <- na.omit(d)

## Get Top 20 ..otherwise...too many...
d1 <- head(d1,30)
# reorder based on no of cases
d1$Court.District <- with(d1, reorder(Court.District, Cases))

# To scale, remove non numeric
nums <- sapply(d1, is.numeric)
nums
d1Num <- d1[ , nums]
d1NonNum <- as.data.frame(d1[ , !nums])
head(d1Num)
head(d1NonNum)

Scaledd1 <- as.data.frame(scale(d1Num))

d1Scaled <- cbind(d1NonNum,Scaledd1)

library(ggplot2)
library(plyr)
library("reshape2")


# need to melt 
d1.m <- melt(d1Scaled)
## d1.m <- melt(d1)
## need to sclae else cases becomes white as they are less 
## in number than population

names(d1.m) <- c("Names","variable","value")

# Plot 1
p <- ggplot(d1.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value),colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue")
p

p1 <- ggplot(d1.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value),colour = "light green") + 
  scale_fill_gradient(low = "white", high = "orange")
p1

# get the nums..without scaled
p2 <- ggplot(d1.m, aes(variable, Names)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = value, label = value)) +
  scale_fill_gradient(low = "white", high = "orange") 

p2

# get the nums..with scale...+ nums from original( ?? try ...) 


ggplot(dat2, aes(as.factor(Var1), Var2, group=Var2)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = dat2$value, label = round(dat2$value, 1))) +
  scale_fill_gradient(low = "white", high = "red") 
# Plot 2
base_size <- 9
p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + theme(legend.position = "none",
                                             axis.ticks = element_blank(), axis.text.x = element_text(size = base_size *0.8, angle = 330, hjust = 0, colour = "grey50"))

### FINAL
### FINAL
### FINAL

###############################################################################
###
### heat map 2
### http://stackoverflow.com/questions/13016022/ggplot2-heatmaps-using-different-gradients-for-categories/13016912#13016912
###############################################################################
#install.packages("ggplot2")
#install.packages("reshape2")
library(ggplot2)
library("reshape2")

d <- d[order(-d$Cases,-d$Total.Population),]
d1 <- na.omit(d)
# reorder based on Court.Distrcits
#d1$Court.District <- with(d1, reorder(Court.District))

## Scale
# To scale, remove non numeric
nums <- sapply(d1, is.numeric)
nums
d1Num <- d1[ , nums]
d1NonNum <- as.data.frame(d1[ , !nums])
head(d1Num)
head(d1NonNum)

d1NumS <- as.data.frame(scale(d1Num))

d1S <- cbind(d1NonNum,d1NumS)
colnames(d1S)[1] <- 'Court.District'
names(d1S)
###

## Get Top 50 ..otherwise...too many...
d2 <- head(d1S,50)
d2 <- head(d1S,20)
d2 <- d1S
d2 <- head(d1,50) # no sclaing...show the numbers...
d3 <- head(d1,50) # no sclaing...show the numbers...
# then order by district
d2 <- d2[order(d2$Court.District),]
View(d2)
d2.m <- melt(d2)
d3.m <- melt(d3) # for showing nos...try to merge and then check...
colnames(d3.m)[3] <- 'NonScaledNo'

dNew <- merge(d2.m,d3.m,by=c("Court.District","variable"))
head(dNew)
## heat map
ggplot(d2.m, aes(variable, Court.District)) + 
  geom_tile(aes(fill = value), colour = "white") + # light green
  scale_fill_gradient(low = "white", high = "steelblue") + # white and orange
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))
        
#axis.text.y = element_text(angle = 330, vjust = 0,size = 10),
#axis.text.x = element_text(angle = 330, hjust = 0,size = 12))

## heat map, show numbers - ??? check jay....
ggplot(dNew, aes(variable, Court.District)) + 
  geom_tile(aes(fill = value), colour = "light green") + # light green
  scale_fill_gradient(low = "white", high = "orange") + # white and orange
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  geom_text(aes(fill = NonScaledNo, label = round(value, 1))) +
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))


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


## bubble
ggplot(d1, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
  scale_x_continuous(name="Total population")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

d2 <- d1[1:20,]
View(d2)
ggplot(d2, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total population")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

ggplot(d2, aes(x=Literates, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="blue", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total Literates")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

ggplot(d2, aes(x=Female.literates, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="pink", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total Female Literates")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

d3 <- d1[21:50,]
View(d3)
ggplot(d3, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total population")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

ggplot(d3, aes(x=Literates, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="blue", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total Literates")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

ggplot(d3, aes(x=Female.literates, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="pink", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total Female Literates")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

d4 <- d1[51:100,]
ggplot(d4, aes(x=Total.Population, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total population")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

ggplot(d4, aes(x=Literates, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="blue", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total Literates")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

ggplot(d4, aes(x=Female.literates, y=Cases, size=Cases, label=Court.District),guide=FALSE)+
  geom_point(colour="white", fill="pink", shape=21)+ scale_size_area(max_size = 25)+
  scale_x_continuous(name="Total Female Literates")+
  scale_y_continuous(name="Total cases")+
  geom_text(size=4)+
  theme_bw() + theme(legend.position = "none")

View(d4)
#############
d1 <- read.csv("DistrictCourt-YearstoDecide.csv",header=T,sep=";")
dim(d1)
head(d1)
View(d1)
names(d1)
names(d)

