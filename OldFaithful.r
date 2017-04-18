setwd("~/GitHub/statistics")
library(ggplot2)
mydata <- read.csv("OldFaithful.csv", header = TRUE)

summary(mydata)

qplot(mydata$Interval..Min,
		geom="histogram",
		main="Histogram for OldFaithful",
		xlab="Interval Between Eruptions",
		fill=I("blue"),
		col =I("white"),
		xlim=c(42,95),
		binwidth=6
		)

qplot(mydata$Duration..Min,
		geom="histogram",
		main="Histogram for OldFaithful Duration Eruptions",
		xlab="Duration (Min)",
		fill=I("blue"),
		col=I("white"),
		xlim=c(1,5),
		binwidth=0.2
		)

ggplot(mydata, aes(x=Interval..Min., y=Duration..Min.)) +
	geom_point(shape=1) +
	geom_smooth(method=lm)