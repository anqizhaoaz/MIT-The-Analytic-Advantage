

federal <- read.csv("~/Downloads/mit/federalFundsRate.csv",stringsAsFactors=FALSE)
str(federal)

table(federal$RaisedFedFunds)
table(federal$Chairman)


federal$Chairman <- as.factor(federal$Chairman)
federal$DemocraticPres <- as.factor(federal$DemocraticPres)
federal$RaisedFedFunds <- as.factor(federal$RaisedFedFunds)

set.seed(201)
library(caTools)
spl = sample.split(federal$RaisedFedFunds, 0.7)

