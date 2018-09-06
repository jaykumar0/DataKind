library(tidyr)
####################################################################
####################################################################
# Petitioner Advocate is more interesting than Respondent Advocate....
# multiples in Petitioner Advocate, not so in Respondent Advocate

###
# Filter d into q - q is master
###

dim(d)

q <- filter(d,before_honourable_judges != "No Judge")
q <- filter(q,petitioner_advocate != "No Petitioner Advocate")
q <- filter(q,respondent_advocate != "No Respondent Advocate")
dim(q)

###
# Petitioner Advocate
###

View(table(d$petitioner_advocate))
p1 <- as.data.frame(table(d$before_honourable_judges,d$petitioner_advocate))
p1 <- as.data.frame(table(q$petitioner_advocate,q$before_honourable_judges))
View(p1  %>% arrange(desc(Freq)))

p2 <- as.data.frame(table(d$before_honourable_judges,d$petitioner_advocate,d$NewStatus))
p2 <- as.data.frame(table(q$before_honourable_judges,q$petitioner_advocate,q$NewStatus))
p2 <- as.data.frame(table(q$petitioner_advocate,q$before_honourable_judges,q$NewStatus))

View(p2  %>% arrange(desc(Freq)))

p21 <- p2 %>% spread(Var3,Freq)
View(p21  %>% arrange(desc(Disposed)))


####################################################################
####################################################################

###
# Respondent Advocate
###
#d3 <- select(d1,c(before_honourable_judges,respondent_advocate,case_type,NewStatus))
#dim(d3)

View(table(d$respondent_advocate))

r1 <- as.data.frame(table(d$before_honourable_judges,d$respondent_advocate))
r1 <- as.data.frame(table(q$before_honourable_judges,q$respondent_advocate))
r1 <- as.data.frame(table(q$respondent_advocate,q$before_honourable_judges))
View(r1  %>% arrange(desc(Freq)))

q2 <- as.data.frame(table(d$before_honourable_judges,d$respondent_advocate,d$NewStatus))
View(q2  %>% arrange(desc(Freq)))

q21 <- q2 %>% spread(Var3,Freq)
View(q21  %>% arrange(desc(Disposed)))


####################################################################
####################################################################
###
# Petitioner and Respondent Advocate
###

r1 <- as.data.frame(table(q$petitioner_advocate,q$respondent_advocate))
View(r1  %>% arrange(desc(Freq)))

### on hearing data
h$petitioner_advocate <- ifelse(h$petitioner_advocate == "","No Petitioner Advocate",h$petitioner_advocate)
h$respondent_advocate <- ifelse(h$respondent_advocate == "","No Respondent Advocate",h$respondent_advocate)
h$before_honourable_judges <- ifelse(h$before_honourable_judges == "","No Judge",h$before_honourable_judges)

h2 <- filter(h, !duplicated(key))
dim(h2)
r2 <- as.data.frame(table(h2$petitioner_advocate,h2$respondent_advocate))
View(r2  %>% arrange(desc(Freq)))

# Actual....
####################################################################
####################################################################
###
# Petitioner and Respondent Advocate, judge and Status
###
####################################################################
####################################################################

p1 <- as.data.frame(table(q$petitioner_advocate,q$respondent_advocate,q$before_honourable_judges,q$NewStatus))
p1 <- as.data.frame(table(q$before_honourable_judges,q$petitioner_advocate,q$respondent_advocate,q$NewStatus))
#p11 <- as.data.frame(table(q$before_honourable_judges,q$petitioner_advocate,q$respondent_advocate,q$NewStatus,q$key))
#p11 <- as.data.frame(table(q$NewStatus,q$key))
#View(p11)
View(p1)
p1 <- filter(p1,Freq != 0) %>% arrange(desc(Freq))
p2 <- head(p1,20) 
View(p2)
p2 <- filter(p1,Freq >= 4)
dim(p2)

names(p2) <- c("before_honourable_judges","petitioner_advocate","respondent_advocate","NewStatus")
UnqJud <- as.data.frame(unique(p2$before_honourable_judges))
UnqJud <- as.character(unique(p2$before_honourable_judges))
View(UnqJud)
UnqJud
t1 <- filter(q,before_honourable_judges %in% UnqJud)
dim(t1)
table(t1$before_honourable_judges)
table(t1$petitioner_advocate)

# check - should be same as p1
a1 <- as.data.frame(table(t1$before_honourable_judges,t1$petitioner_advocate,t1$respondent_advocate,t1$NewStatus))
a1 <- filter(a1,Freq != 0) %>% arrange(desc(Freq))
View(a1)

UnqPetAdv <- as.character(unique(p2$petitioner_advocate))
UnqResAdv <- as.character(unique(p2$respondent_advocate))
UnqStatus <- as.character(unique(p2$NewStatus))
dim(t1)
t1 <- filter(t1,petitioner_advocate %in% UnqPetAdv)
dim(t1)
t1 <- filter(t1,respondent_advocate %in% UnqResAdv)
dim(t1)
t1 <- filter(t1,NewStatus %in% UnqStatus)
dim(t1)
a1 <- as.data.frame(table(t1$before_honourable_judges,t1$petitioner_advocate,t1$respondent_advocate,t1$NewStatus))
a1 <- filter(a1,Freq != 0) %>% arrange(desc(Freq))
names(a1) <- c("before_honourable_judges","petitioner_advocate","respondent_advocate","NewStatus")
View(a1)

dim(t1)
273/1511
# The total case is around 18% of all and ....selection criteria ...more than 4
# Prepare data for Sankey - source, target, value
names(t1)
# link 1 - judge and petitioner advocate
l1 <- as.data.frame(table(t1$before_honourable_judges,t1$petitioner_advocate))
l1 <- filter(l1,Freq != 0)
names(l1) <- c("source","target","value")
View(l1)

# link 2 - petitioner advocate and responder advocate
l2 <- as.data.frame(table(t1$petitioner_advocate,t1$respondent_advocate))
l2 <- filter(l2,Freq != 0)
names(l2) <- c("source","target","value")
View(l2)

# link 3 - petitioner advocate and responder advocate
l3 <- as.data.frame(table(t1$respondent_advocate,t1$NewStatus))
l3 <- filter(l3,Freq != 0)
names(l3) <- c("source","target","value")
View(l3)

df <- rbind(l1,l2,l3)

View(df)
# B.Shubhakaran is both type of advocate..update the csv file....else get it in between...
write.csv(df,"../dataForD3/medakNWSankey.csv",row.names=FALSE)

#########################################################################
#########################################################################
### 
# on hearing data ...only cases with hearings...?? why do this ?? will  miss out pending ones.....
###
table(h$petitioner_advocate)
View(as.data.frame(table(d$petitioner_advocate)))

dim(filter(d,petitioner_advocate == "No Petitioner Advocate"))
dim(filter(d,respondent_advocate == "No Respondent Advocate"))


h$petitioner_advocate <- ifelse(h$petitioner_advocate == "","No Petitioner Advocate",h$petitioner_advocate)
h$respondent_advocate <- ifelse(h$respondent_advocate == "","No Respondent Advocate",h$respondent_advocate)
h$before_honourable_judges <- ifelse(h$before_honourable_judges == "","No Judge",h$before_honourable_judges)

### 
# remove duplicates - multiple hearings
# no cate type in hearings data...can get from cases date
###
h2 <- filter(h, !duplicated(key))
dim(filter(d,petitioner_advocate == "No Petitioner Advocate"))
dim(filter(d,respondent_advocate == "No Respondent Advocate"))
dim(filter(h2,petitioner_advocate == "No Petitioner Advocate"))
dim(filter(h2,respondent_advocate == "No Respondent Advocate"))
dim(h2)
dim(d)
