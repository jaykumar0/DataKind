library(tidyr)

# http://stackoverflow.com/questions/24929954/is-it-possible-to-use-spread-on-multiple-columns-in-tidyr-similar-to-dcast
# http://stackoverflow.com/questions/29775461/tidyr-repeated-measures-multiple-variables-wide-format
# http://stackoverflow.com/questions/30592094/r-spreading-multiple-columns-with-tidyr

####################################################################
####################################################################

###
# Network Analysis
###
View(table(d$petitioner_advocate))
table(d$case_type)
###
# EP and OS - all status present..lets explore it first
###

d1 <- filter(d,case_type  == "EP")
#d1 <- filter(d,case_type %in% c("EP","OS"))
d1 <- filter(d,case_type %in% c("EP","OS","MC","SCC"))
dim(d1)

#d2 <- select(d1,c(before_honourable_judges,petitioner_advocate,case_type,NewStatus))
#dim(d2)
#View(d2)

### 
# on hearing data
###
h$petitioner_advocate <- ifelse(h$petitioner_advocate == "","No Petitioner Advocate",h$petitioner_advocate)
h$respondent_advocate <- ifelse(h$respondent_advocate == "","No Respondent Advocate",h$respondent_advocate)
h$before_honourable_judges <- ifelse(h$before_honourable_judges == "","No Judge",h$before_honourable_judges)

### 
# remove duplicates - multiple hearings
# no cate type in hearings data...can get from cases date
###
h2 <- filter(h, !duplicated(key))

####################################################################
####################################################################

# Petitioner Advocate is more interesting than Respondent Advocate....
# multiples in Petitioner Advocate, not so in Respondent Advocate

###
# Petitioner Advocate 
###

###
# Node data
### 
d2 <- select(d1,c(before_honourable_judges,petitioner_advocate,case_type,NewStatus))


#d2[!duplicated(d2$before_honourable_judges),]
UnqJud <- as.data.frame(unique(d2$before_honourable_judges))
colnames(UnqJud) <- "Col1"
UnqJud$Type <- "Judge"
UnqAdv <- as.data.frame(unique(d2$petitioner_advocate))
colnames(UnqAdv) <- "Col1"
UnqAdv$Type <- "Advocate"
UnqCase <- as.data.frame(unique(d2$case_type))
colnames(UnqCase) <- "Col1"
UnqCase$Type <- "CaseType"
UnqStatus <- as.data.frame(unique(d2$NewStatus))
colnames(UnqStatus) <- "Col1"
UnqStatus$Type <- "Status"

df <- rbind(UnqJud,UnqAdv)
df <- rbind(df,UnqCase)
df <- rbind(df,UnqStatus)
View(df)
rownames(df)
head(df)
head(df)
#df <- cbind(Row.Names = rownames(df), df)
names(df) <- c("Nodes","Type")
df$Label <- df$Nodes
df$ID <- df$Nodes
dim(df)

write.csv(df,"dataForD3/medakNodes4CaseTypes.csv")

z$color<-y[match(z$letter, y$letter),2]
JudgeID <- df[match(df$Nodes,d2$before_honourable_judges),1]
JudgeID

View(d2)
dim(d2)

###
# Edge data
###
p1 <- as.data.frame(table(d2$before_honourable_judges,d2$petitioner_advocate))
View(p1)
p1 <- filter(p1,Freq != 0)
View(p1 %>% arrange(desc(Freq)))

p2 <- as.data.frame(table(d2$petitioner_advocate,d2$case_type))
View(p2)
p2 <- filter(p2,Freq != 0)
View(p2 %>% arrange(desc(Freq)))

sum(p2$Freq)
dim(d2)

p3 <- as.data.frame(table(d2$case_type,d2$NewStatus))
View(p3)
p3 <- filter(p3,Freq != 0)
View(p3 %>% arrange(desc(Freq)))

p12 <- rbind(p1,p2)
p123 <- rbind(p12,p3)
names(p123) <- c("Source","Target","Weight")
p123$Type <- "Undirected"
View(p123)
write.csv(p123,"dataForD3/medakEdges4CaseTypes.csv")



###
# For Network Analysis
###
####################################################################
####################################################################
###
# Only Judge and Advocate and Status
###

### h2 - no Status and case tyep

UnqJud <- as.data.frame(unique(d$before_honourable_judges))
colnames(UnqJud) <- "Col1"
UnqJud$Type <- "Judge"
UnqAdv <- as.data.frame(unique(d$petitioner_advocate))
colnames(UnqAdv) <- "Col1"
UnqAdv$Type <- "Advocate"
# No case Type in hearing
#UnqCase <- as.data.frame(unique(h2$case_type))
#colnames(UnqCase) <- "Col1"
#UnqCase$Type <- "CaseType"
UnqStatus <- as.data.frame(unique(d$NewStatus))
colnames(UnqStatus) <- "Col1"
UnqStatus$Type <- "Status"

df <- rbind(UnqJud,UnqAdv)
#df <- rbind(df,UnqCase)
df <- rbind(df,UnqStatus)
View(df)
rownames(df)
head(df)
head(df)
#df <- cbind(Row.Names = rownames(df), df)
names(df) <- c("Nodes","Type")
df$Label <- df$Nodes
df$ID <- df$Nodes
dim(df)

write.csv(df,"dataForD3/medakNodesNoCase.csv")

z$color<-y[match(z$letter, y$letter),2]
JudgeID <- df[match(df$Nodes,d2$before_honourable_judges),1]
JudgeID

View(d2)
dim(d2)

###
# Edge data
###
p1 <- as.data.frame(table(d$before_honourable_judges,d$petitioner_advocate))
View(p1)
p1 <- filter(p1,Freq != 0)
View(p1 %>% arrange(desc(Freq)))

#p2 <- as.data.frame(table(d$petitioner_advocate,d$case_type))
#View(p2)
#p2 <- filter(p2,Freq != 0)
#View(p2 %>% arrange(desc(Freq)))

#sum(p2$Freq)
#dim(d)

p3 <- as.data.frame(table(d2$case_type,d2$NewStatus))
p3 <- as.data.frame(table(d$petitioner_advocate,d$NewStatus))
View(p3)
p3 <- filter(p3,Freq != 0)
View(p3 %>% arrange(desc(Freq)))

#p12 <- rbind(p1,p2)
p13 <- rbind(p1,p3)
names(p13) <- c("Source","Target","Weight")
p13$Type <- "Undirected"
View(p13)
table(p13$Weight)
View(as.data.frame(unique(p13$Weight)))

p13$Weight2 <- ifelse(p13$Weight == 1 ,1,
                      ifelse(p13$Weight > 1 & p13$Weight < 5,3,
                             ifelse(p13$Weight >= 5 & p13$Weight < 10 , 5 ,
                                    ifelse(p13$Weight >= 10 & p13$Weight < 50,7,
                                           ifelse(p13$Weight >= 50 & p13$Weight < 100,9,11))))) 

write.csv(p13,"dataForD3/medakEdgesNoCase.csv")

