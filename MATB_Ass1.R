setwd("~/GitHub/statistics")
library(ggplot2)
library(grid)
library(gridExtra)

# TASK 1
men100 <- read.csv("Men100.csv", header = TRUE)
women100 <- read.csv("Women100.csv", header = TRUE)

# Linear Models

lm_men100=lm(Sec ~ Year, data=men100)
lm_women100=lm(Secs ~ Year, data=women100)

# Equations are:
# y = -.028.x + 66 (women)
# y = -.0076.x + 25 (men)

# Therefore Intersect of these two lines is:
# -.028.x + 66 = -.0076.x + 25
# -.028x +.0076x = 25 - 66
# -.0204x = -41
# x = 2010 (Rounded up)

#Check Y's equal
# y = -.028 x 2010 +66 (women)
#	y = 9.72 sec

# y = -.0076 x 2010 + 25 (Men)
# y = 9.72sec 

# ANS: Men and Women predicted to be equal in 2010 at 9.72sec
# Somthing Wrong - Dosn't line up with graph.....??


# Plot with regression line women and men 100meter times
p1=ggplot() +
	geom_point(data=men100, aes(x=Year, y=Sec), color="red", shape=1) +
	geom_smooth(data=men100, method="lm", aes(x=Year, y=Sec), se=FALSE) +
	geom_text(aes(x = 1980, y = 10.1, label = lm_eqn(lm_men100)), parse = TRUE) +
	geom_point(data=women100, aes(x=Year, y=Secs), color="blue", shape=2) +
	geom_smooth(data=women100, method="lm", aes(x=Year, y=Secs), se=FALSE) +
	geom_text(aes(x = 1990, y = 11, label = lm_eqn(lm_women100)), parse = TRUE) +	
	xlab("Year") +
	ylab("Seconds")

#Summary of LM's
summary(lm_men100)
summary(lm_women100)


# Function to print Equations
lm_eqn <- function(m){
    eq <- substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2, 
         list(a = format(coef(m)[1], digits = 2), 
              b = format(coef(m)[2], digits = 2), 
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
}

# TASK 4
forestmen <- read.csv("ForestMen.csv", header = TRUE)
plainsmen <- read.csv("PlainsMen.csv", header = TRUE)

boxplot <- rbind(data.frame(group="forestmen", forestmen), data.frame(group="plainsmen", plainsmen))

# a - Full set of discriptive Stats
summary(forestmen)
summary(plainsmen)

p2 <- ggplot(boxplot, aes(x=group, y=X.Handle)) +
	geom_boxplot()
p3 <- ggplot(boxplot, aes(x=group, y=Length)) +
	geom_boxplot()
p4 <- ggplot(boxplot, aes(x=group, y=Notch)) +
	geom_boxplot()

grid.arrange(p2,p3,p4, ncol=2, top = "Boxplots")


