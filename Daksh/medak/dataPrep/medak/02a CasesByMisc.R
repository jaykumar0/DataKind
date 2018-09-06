p1 <- as.data.frame(table(d$case_type))
View(p1)
names(p1) <- c("CaseType","Freq")
#p12 <- p1 %>% spread(Var2,Freq)
p1 <- p1 %>% arrange(desc(Freq))
View(p1)


###
#
###
# https://github.com/lionel-/ggstance
# http://juliasilge.com/blog/Return-of-NEISS/
library(ggplot2)
#install.packages("devtools")
#devtools::install_github("lionel-/ggstance")
#install.packages("ggstance")
library(ggstance)
library(scales)
ggplot(data = p1, aes(x = Freq, y = CaseType)) + 
  geom_barh(stat="identity", aes(fill = Freq)) +
  theme_minimal(base_family = "RobotoCondensed-Regular", base_size = 13) +
  theme(plot.title=element_text(family="Roboto-Bold")) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0), labels = scientific_format()) +
  scale_fill_gradient(low = "#86d746", high = "#5eb151") +
  labs(y = NULL, x = "Estimated number of injuries each year",
       title = "Emergency Room Injuries for White Women in Their 30s",
       subtitle = "NEISS reporting of injuries due to consumer products") +
  theme(axis.title.x=element_text(margin=margin(t=15)))
