###
#
###
cor(d$AgeinDays,d$DaysToFirstHearing,use="complete.obs")

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

library(ggplot2)
df <- data.frame(x = d$AgeinDays)
df$y <- d$DaysToFirstHearing

p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()
p

p1 <- p + geom_text(x = 100, y = 3300, label = lm_eqn(df), parse = TRUE)

p1

###
#
###
cor(d$AgeinDays,d$NumHearing,use="complete.obs")

###
#
###
cor(d$AgeinDays,d$AvgDaysBetweenHearing,use="complete.obs")

###
#
###
cor(d$NumHearing,d$AvgDaysBetweenHearing,use="complete.obs")


