# https://gramener.com/nas/
# https://www.eventbrite.com/e/lifewithdata-data-art-gallery-bangalore-tickets-19737501398
# regex101.com/#javascript
# https://factly.in/
#https://gramener.com/nas/?subject=Maths+%1#technical-message

library(stringr)

View(as.data.frame(table(d$under_sections)))
View(as.data.frame(table(d$under_sections,d$NewStatus)))

s <-  strsplit(as.character(d$under_sections), ',')
#View(s)
s
unlist(s)
p1 <- data.frame(combined_case_number=rep(d$combined_case_number, sapply(s, FUN=length)),under_sections=unlist(s) )
View(p1)

s <-  strsplit(as.character(p1$under_sections), 'and')
p2 <- data.frame(combined_case_number=rep(p1$combined_case_number, sapply(s, FUN=length)),under_sections=unlist(s) )
View(p2)
dim(p2)

s <-  strsplit(as.character(p2$under_sections), 'AND')
p2 <- data.frame(combined_case_number=rep(p2$combined_case_number, sapply(s, FUN=length)),under_sections=unlist(s) )
View(p2)

###
# Define function
# returns string w/o leading or trailing whitespace
###
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

p2$under_sections <- trim(p2$under_sections)
p2$under_sections <- gsub("Cr.P.C","",p2$under_sections)
p2$under_sections <- gsub("SECTION","",p2$under_sections)
p2$under_sections <- gsub("OF I.P.ACT","",p2$under_sections)
p2$under_sections <- gsub("OF IP.ACT","",p2$under_sections)
p2$under_sections <- str_replace_all(p2$under_sections,"CPC","")
p2$under_sections <- str_replace_all(p2$under_sections,"r/w","")
p2$under_sections <- str_replace_all(p2$under_sections,"rw","")
p2$under_sections <- str_replace_all(p2$under_sections,"IPC","")

p2$under_sections <- trim(p2$under_sections)

View(p2)
View(as.data.frame(table(p2$under_sections)))
View(as.data.frame(table(d$under_sections)))
###
# Clean some moree
###
p21 <- p2
View(as.data.frame(table(p2$under_sections)))
View(as.data.frame(table(p21$under_sections)))

p2$under_sections <- ifelse(str_detect(p2$under_sections,"324"),"324",p2$under_sections)
View(filter(as.data.frame(table(p2$under_sections)),Var1 == "324"))
View(filter(p2,under_sections == "125"))

p2$under_sections <- ifelse(str_detect(p2$under_sections,"125"),"125",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"506"),"506",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"304"),"304",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"160"),"160",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"166"),"166",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"196"),"196",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"290"),"290",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"294"),"294",p2$under_sections)

p2$under_sections <- ifelse(str_detect(p2$under_sections,"Order 21"),"21",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"Order XXI"),"21",p2$under_sections)


# 13 of Provencial Insolvency Act.
# 13(1)
# 13(1) Ia
# 13(1)1a
# 13(1)i
# 13(1)ia
# 13(B)
# 13(i(ia)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"13 of Provencial Insolvency Act."),"13",p2$under_sections)
#p2$under_sections <- ifelse(str_detect(p2$under_sections,"13(1)"),"13",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"13\\("),"13",p2$under_sections)

### ONLY that string and not contains...check before doing..else 304 also gets converted to 30...
p2$under_sections <- ifelse(str_detect(p2$under_sections,"18"),"18",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"21"),"21",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"19"),"19",p2$under_sections)
p2$under_sections <- ifelse(str_detect(p2$under_sections,"30"),"30",p2$under_sections)

####################
## merge d2 with d
####################
# http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
f <- merge(x = d, y = p2, by = "combined_case_number", all = TRUE)
# move to next section.....

#View(f)
#final(final["IP-9-2010",])
#final[ which(final$combined_case_number=='IP-9-2010'), ]
#filter(f,final$combined_case_number=='IP-9-2010')

f1 <- (as.data.frame(table(f$under_sections.x,f$NewStatus))) # Original
f1 <- (as.data.frame(table(f$under_sections.y,f$NewStatus))) # Updated and cleaned
f1 <- filter(f1,Freq != 0)
View(f1)
f$under_sections.x <- NULL
names(f)
#write.csv(final, "medakFinal.csv", row.names=FALSE)

###
# GroupBy Under_sections
###

mSum <- f %>% group_by(under_sections.y,NewStatus) %>%
  summarise(AvgDaysToFirstHearing = round(mean(DaysToFirstHearing,na.rm=T),0), 
            AvgNumOfHrng = round(mean(NumHearing,na.rm=T),0), 
            AvgDaysBetweenHrng = round(mean(AvgDaysBetweenHearing,na.rm=T),0),
            AvgAgeinDays = round(mean(AgeinDays,na.rm=T),0),
            TotalCases = n())


mSum[is.na(mSum)] <- 0
View(mSum)
View(filter(mSum,under_sections.y == "125"))
View(filter(mSum,under_sections.y == "Under Order 21 rule 34"))
View(filter(mSum,under_sections.y == "U/o VII Rule 1 26"))

####################
###write json
####################

#jData <- final[,c("court_state","court_district","case_type","current_status","under_sections")]

jData <- final[,c("court_state","court_district","case_type","under_sections.y")]

# http://stackoverflow.com/questions/26986193/how-to-write-a-json-object-from-r-dataframe-with-grouping
# http://stackoverflow.com/questions/12818864/how-to-write-to-json-with-children-from-r

#install.packages("RJSONIO")
library(RJSONIO)

makeList<-function(x){
  if(ncol(x)>2){
    listSplit<-split(x[-1],x[1],drop=T)
    lapply(names(listSplit),function(y){list(name=y,children=makeList(listSplit[[y]]))})
  }else{
    lapply(seq(nrow(x[1])),function(y){list(CaseType=x[,1][y],IPC=x[,2][y])})
  }
}

jsonOut<-toJSON(list(name="jData",children=makeList(jData[-1])))
cat(jsonOut)

class(jsonOut)
write.csv(jsonOut, "1.json", row.names=FALSE)


ncol(jData)
listSplit<-split(jData[-1],jData[1],drop=T)
class(listSplit)

##################

jsonOut<-toJSON(list(name="MyData",children=makeList(MyData[-1])))
cat(jsonOut)


library(rjson)
fh <- file("[medak.json]", "w")
for (i in 1:nrow(d2)) {
  write.table(toJSON(d2[i,]), fh,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}
close(fh)