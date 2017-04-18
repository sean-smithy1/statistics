setwd("~/GitHub/statistics")
library(ggplot2)
mydata <- read.csv("OldFaithful.csv", header = TRUE)

summary(mydata)

qplot(mydata$Interval..Min,
		geom="histogram",
		main="Histogram for OldFaithful",
		xlab="Min Between Eruptions",
		fill=I("blue"),
		col =I("white"),
		xlim=c(40,100),
		binwidth=5
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