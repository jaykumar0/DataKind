##########################
## can use sankey output...
## change the column name as per noob 
## and add row for toplevel..will null as parent
## 

###
# not required ..keep as ex for creating and filling empry df 
###
dd <- data.frame(name= character(), parent= character(),stringsAsFactors = FALSE)
dd
str(dd)
dd[1, ] <- c("Medak","null")
dd

table(d$court_name)
unqCrtNames <- as.character(unique(d$court_name))
unqCrtNames

i <- 2
for (names in unqCrtNames)
{
  
  dd[i,]<-c(names,"Medak")
  i <- i + 1
}

dd
i

###
#
###
p1 <- filter(d,before_honourable_judges != "No Judge")

View(as.data.frame(table(d$before_honourable_judges)))
table(d$court_name)
table(d$case_type)





table(d$before_honourable_judges)


