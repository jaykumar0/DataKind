library(plotly)
library(ggplot2)
library(reshape2)

w <- read.csv("C:/project/Daksh/MedakViz/09f JudgeAndCaseDetails/medakJudgeAvgAgeinDays.csv")
w
str(w)
w.m <- melt(w, id.vars='before_honourable_judges')

ggplot(w.m, aes(before_honourable_judges, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

ggplot(w.m, aes(before_honourable_judges, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + coord_flip()

ggplotly()

