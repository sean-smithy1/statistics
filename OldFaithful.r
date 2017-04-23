setwd("~/GitHub/statistics")
library(ggplot2)
library(car)

mydata <- read.csv("OldFaithful.csv", header = TRUE)

summary(mydata)

ggplot(mydata, aes(mydata$Interval..Min)) +
	geom_histogram() +
	labs(title="Histogram for time beteen") +
  labs(x="Min", y="Count")

qplot(mydata$Duration..Min,
		main="Histogram for OldFaithful Duration Eruptions",
		xlab="Duration (Min)",
		fill=I("blue"),
		col=I("white"),
		xlim=c(1.7,5.1),
		binwidth=0.4
		)

ggplot(mydata, aes(x=Interval..Min., y=Duration..Min.)) +
	geom_point(shape=1) +
	geom_smooth(method=lm)

#Response ~ Predictor
of_lm1=lm(Duration..Min. ~ Day + Interval..Min., data=mydata)
summary(of_lm1)

# Odd - No Correlation
cor.test(mydata$Duration..Min.), mydata$Interval..Min.)

anova(of_lm1)

# Model Checking
par(mfrow=c(1, 2)) +
plot(of_lm1, which = c(1,2))

hist(of_lm1$residuals, breaks =10)


# density plot
ggplot() + geom_density(aes(residuals(of_lm1)))
# Yep it's normal-ish

#qq plot
qqPlot(of_lm1)

spreadLevelPlot(of_lm1, pch=20)

# Durbin-Watson-test for autocorrelation
durbinWatsonTest(of_lm1)

influenceIndexPlot(of_lm1)


# Useful
# sum(mydata$Interval..Min>=49 & mydata$Interval..Min <= 55)