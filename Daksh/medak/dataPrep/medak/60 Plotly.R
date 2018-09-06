# http://moderndata.plot.ly/interactive-r-visualizations-with-d3-ggplot2-rstudio/
# https://plot.ly/ggplot2/


#install.packages("plotly")
library(plotly)
set.seed(100)
diam <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(diam, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)


t <- select(d,case_type,court_name,CivilOrCriminal,AgeinDays,DaysToFirstHearing,NewStatus,Weekday,Month)
t1 <- na.omit(t) 
dim(t1)

plot_ly(t1, x = AgeinDays, y = DaysToFirstHearing, text = paste("Case Type: ", case_type),
        mode = "markers", color = NewStatus, size = DaysToFirstHearing)

plot_ly(d, x = AgeinDays, y = DaysToFirstHearing, text = paste("Clarity: ", case_type),
        mode = "markers", color = NewStatus, size = NumHearing)

