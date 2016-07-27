
## week 2 class

#### wine ####
wine <- read.csv("~/Downloads/mit/wine.csv")
str(wine)

model1 = lm(Price~HarvestRain + WinterRain, data=wine)
summary(model1)

model2 = lm(Price~., data=wine)
summary(model2)

model3 = lm(Price~ Year + WinterRain + AGST + HarvestRain + Age, data=wine)
summary(model3)

cor(wine)

predictTest = predict(model3, newdata=wine)
predictTest

#### moneyball ####
baseball <- read.csv("~/Downloads/mit/baseball.csv")
str(baseball)

moneyball = subset(baseball, baseball$Year < 2002)
moneyball$SD = moneyball$RS - moneyball$RA
str(moneyball)

plot(moneyball$SD, moneyball$W)
winsReg = lm(W ~ SD, data=moneyball)
summary(winsReg)

runReg = lm(RA ~ OOBP + OSLG, data=, moneyball)
summary(runReg)
-837.38 + 2913.60*0.297 + 1514.29*0.370

runReg2 = lm(RS ~ OBP + SLG, data=, moneyball)
summary(runReg2)
-804.63 + 2737.77*0.311 +  1584.91 *0.405

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
cor(teamRank, wins2012)
wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)
cor(teamRank, wins2013)



