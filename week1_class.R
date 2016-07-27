## Week 1 class
WHO <- read.csv("~/Downloads/mit/week1_WHO.csv")
str(WHO)

summary(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]

WHO$pct_60 <- WHO$Over60 / WHO$Population
which.min(WHO$pct_60)
WHO$Country[78]

which.max(WHO$LiteracyRate)
WHO$Country[44]
WHO[order(WHO$pct_60),]

#Use the tapply function to find the average child mortality rate of countries in each region.
tapply(WHO$ChildMortality, WHO$Region, mean)


