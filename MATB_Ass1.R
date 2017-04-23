setwd("~/GitHub/statistics")
library(ggplot2)
library(car)
library(matlib)

men100 <- read.csv("Men100.csv", header = TRUE)
women100 <- read.csv("Women100.csv", header = TRUE)
forestmen <- read.csv("ForestMen.csv", header = TRUE)
plainsmen <- read.csv("PlainsMen.csv", header = TRUE)



# Plot with regression line women and men 100meter times
ggplot()+
	geom_point(data=men100, aes(x=Year, y=Sec), color="red", shape=1) +
	geom_smooth(data=men100, method="lm", aes(x=Year, y=Sec), se=FALSE) +
	geom_text(x = 25, y = 300, label = "Test", parse = TRUE) +
	geom_point(data=women100, aes(x=Year, y=Secs), color="blue", shape=2) +
	geom_smooth(data=women100, method="lm", aes(x=Year, y=Secs), se=FALSE) +
	xlab("Year") +
	ylab("Seconds")
#	geom_smooth(method=lm, se=FALSE)

#Linear Model of Men100
lm_men100=lm(Year ~ Sec, data=men100)
summary(lm_men100)

#Linear Model of Women100
lm_women100=lm(Year ~ Secs, data=women100)
summary(lm_women100)


# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: http://goo.gl/K4yh

lm_eqn <- function(df){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(coef(m)[1], digits = 2), 
              b = format(coef(m)[2], digits = 2), 
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
}

p +
geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)