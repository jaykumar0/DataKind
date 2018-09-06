############################################################################
############################################################################
# Explore
############################################################################
############################################################################
?DataExplorer
disp$NewStatus <- NULL
GenerateReport(disp)

library(ggplot2)
library(plotly)

###
# no of case types
###
q <- ggplot(data=disp, aes(x=case_type))
q + geom_bar(stat="count")
q + geom_bar(stat="count" , color="darkgreen", fill="lightgreen")
q + geom_bar(stat="count" , color="steelblue", fill="lightblue")


ggplotly()

###
# bar with values
###
# http://stackoverflow.com/questions/11653268/adding-labels-to-ggplot-bar-chart
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# https://plot.ly/ggplot2/geom_bar/
# http://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2

x <- as.data.frame(table(disp$case_type,disp$court_name))
names(x)

ggplot(x, aes(x=Var1, y=Freq,fill=Var2)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  theme_minimal()

# Outside bars
z<- as.data.frame(table(disp$case_type))
ggplot(z, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", color="steelblue", fill="lightblue")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  theme_minimal()


# Inside bars
ggplot(z, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

###
# case type and court
###
library(ggthemes)

dispSum <- disp %>%
  group_by(case_type, before_honourable_judges, court_name) %>%
  summarise(num_cases = n())

ggplot(dispSum, aes(x = case_type, y = num_cases, fill = court_name)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  #  facet_wrap(~before_honourable_judges) +
  coord_flip() +
  labs(y = 'Proportion of Cases', 
       x = 'Case Type',
       title = 'Medak Case Types - Courts Handing them') +
  theme_few()

###
# case type and Judge
###
ggplot(dispSum, aes(x = case_type, y = num_cases, fill = before_honourable_judges)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  #  facet_wrap(~before_honourable_judges) +
  coord_flip() +
  labs(y = 'Proportion of Cases', 
       x = 'Case Type',
       title = 'Medak Case Types - Courts Handing them') +
  theme_few()

###
#
###

q <- ggplot(data=disp, aes(x=AgeinDays))
q + geom_histogram(aes(fill = ..count..),binwidth = 100,color="darkblue") + scale_fill_gradient(name="Total Cases",low="white", high="pink") + theme(axis.title.y = element_blank(),axis.title.x = element_blank())
q + geom_histogram(aes(fill = ..count..),binwidth = 100) + scale_fill_gradient(name="Total Cases",low="pink", high="red")
q + geom_histogram(aes(fill = ..count..),binwidth = 100) + scale_fill_gradient(name="Total Cases",low="lightblue", high="steelblue")
q + geom_histogram(aes(fill = ..count..),binwidth = 100) + scale_fill_gradient(name="Total Cases",low="lightgreen", high="green")
ggplotly()


###
# Case Type and Judge and Judge and court
###

x <- as.data.frame(table(disp$case_type , disp$before_honourable_judges))
names(x)
g <- ggplot(x, aes(x=Var1, y = Freq, fill=Var2)) 
g + geom_bar(stat="identity")
g + geom_bar(stat="identity", position=position_dodge())

x <- as.data.frame(table(disp$before_honourable_judges , disp$court_name))
g <- ggplot(x, aes(x=Var1, y = Freq, fill=Var2)) 
g + geom_bar(stat="identity")
g + geom_bar(stat="identity", position=position_dodge())

###
# scatter plot
###
p <- ggplot(disp, aes(x=AgeinDays, y=DaysToFirstHearing,colour=court_name)) 
p + geom_point()
p + geom_point(shape=1)
p+ geom_smooth() 
p+geom_smooth(method=lm)

p1 <- ggplot(d, aes(x=AgeinDays, y=(DaysToFirstHearing),colour=court_name)) + geom_smooth()
p1

###
# Day Filed and court
###
g <- ggplot(disp, aes(Weekday, ..count..)) 
g + geom_bar(aes(fill = court_name), position = "dodge")

###
# boxplot - age and case type
###
# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/

ggplot(disp, aes(x=case_type, y=AgeinDays, fill=court_name)) + geom_boxplot()
ggplot(disp, aes(x=case_type, y=AgeinDays, fill=case_type)) + geom_boxplot() + 
  theme(legend.position="none")

ggplot(disp, aes(x=before_honourable_judges, y=AgeinDays, fill=before_honourable_judges)) + geom_boxplot() + 
  theme(legend.position="none")

ggplot(disp, aes(x=case_type, y=AgeinDays), colour="lightblue") + geom_boxplot()

###
# age by case type and judge
###
dispSum <- disp %>%
  group_by(case_type, before_honourable_judges) %>%
  summarise(AgeinDays = round(mean(AgeinDays),2),
            TotCase = n())

View(dispSum)

# http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
p <- ggplot(dispSum, aes(x=case_type, y=AgeinDays, fill=before_honourable_judges)) + geom_boxplot()
p <- ggplot(disp, aes(x=before_honourable_judges, y=AgeinDays, fill=before_honourable_judges)) + geom_boxplot() + 
  theme(legend.position="none")
p + facet_wrap( ~ case_type, ncol=2)

p <- ggplot(filter(disp,case_type == "L.A.O.P"), aes(x=before_honourable_judges, y=AgeinDays, fill=before_honourable_judges)) + geom_boxplot() + 
  theme(legend.position="none")
p

p <- ggplot(filter(disp,case_type == "MVOP"), aes(x=before_honourable_judges, y=AgeinDays, fill=before_honourable_judges)) + geom_boxplot() + 
  theme(legend.position="none")
p

p <- ggplot(filter(disp,case_type == "IP"), aes(x=before_honourable_judges, y=AgeinDays, fill=before_honourable_judges)) + geom_boxplot() + 
  theme(legend.position="none")
p

p <- ggplot(filter(disp,case_type == "SCC"), aes(x=before_honourable_judges, y=AgeinDays, fill=before_honourable_judges)) + geom_boxplot() + 
  theme(legend.position="none")
p

p <- ggplot(filter(disp,case_type == "LGOP"), aes(x=before_honourable_judges, y=AgeinDays, fill=before_honourable_judges)) + geom_boxplot() + 
  theme(legend.position="none")
p

###
# days to first hearing/no of hearing and casetype/judge
###

ggplot(disp, aes(x=case_type, y=DaysToFirstHearing, fill=case_type)) + geom_boxplot() + 
  theme(legend.position="none")

ggplot(disp, aes(x=case_type, y=DaysToFirstHearing, fill=court_name)) + geom_boxplot()

ggplot(disp, aes(x=case_type, y=NumHearing, fill=case_type)) + geom_boxplot() + 
  theme(legend.position="none")

ggplot(disp, aes(x=before_honourable_judges, y=NumHearing, fill=before_honourable_judges)) + geom_boxplot() + 
  theme(legend.position="none")


ggplot(disp, aes(x=case_type, y=NumHearing, fill=court_name)) + geom_boxplot()

ggplot(disp, aes(x=case_type, y=NumHearingImputed, fill=court_name)) + geom_boxplot()
ggplot(disp, aes(x=case_type, y=AgeinDays, fill=court_name)) + geom_boxplot()


###
#
###
ggplot(disp,
       aes(x = factor(""), fill = court_name) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")

ggplot(disp,
       aes(x = factor(""), fill = before_honourable_judges) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")
ggplotly()

###
#
###
ggplot(disp, aes(x=AgeinDays)) + 
  geom_density(color = "blue", adjust=1/10) +
  geom_vline(xintercept = 400, color="red")+
  geom_vline(xintercept = 1000, color="red")+
  geom_vline(xintercept = 4000, color="red") + 
  ggtitle("Density of Age in Days")

###
#
###

###
#
###
# http://r4stats.com/examples/graphics-ggplot2/
# http://stackoverflow.com/questions/15014595/how-to-use-black-and-white-fill-patterns-instead-of-color-coding-on-calendar-hea
# http://rgraphgallery.blogspot.in/2013/04/rg-heatmap-plot-of-calender.html
# http://stackoverflow.com/questions/15014595/how-to-use-black-and-white-fill-patterns-instead-of-color-coding-on-calendar-hea
# http://www.scoop.it/t/things-about-r/p/2108480383/2012/07/05/margintale-ggplot2-time-series-heatmaps
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# https://plot.ly/ggplot2/geom_bar/
# https://blog.clevertap.com/a-brief-primer-on-linear-regression-part-ii/
