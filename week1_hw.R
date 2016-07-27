
## week 1 homework

#### AN ANALYTICAL DETECTIVE ####
#load data
mvtWeek1 <- read.csv("~/Downloads/mit/mvtWeek1.csv")
str(mvtWeek1)

summary(mvtWeek1$Beat)
table(mvtWeek1$Arrest)
str(subset(mvtWeek1, mvtWeek1$LocationDescription =="ALLEY"))

#convert date
DateConvert = as.Date(strptime(mvtWeek1$Date, "%m/%d/%y %H:%M"))

#extract month and weekday
mvtWeek1$Month = months(DateConvert)
mvtWeek1$Weekday = weekdays(DateConvert)
mvtWeek1$Date = DateConvert

#In which month did the fewest motor vehicle thefts occur?
table(mvtWeek1$Month)

#On which weekday did the most motor vehicle thefts occur?
table(mvtWeek1$Weekday)

#Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvtWeek1$Month, mvtWeek1$Arrest)

#visualize
hist(mvtWeek1$Date, breaks=100)

#Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period? (Note that the time period is from 2001 to 2012, 
#so the middle of the time period is the beginning of 2007.)
boxplot(mvtWeek1$Date~mvtWeek1$Arrest)

#For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvtWeek1$Year, mvtWeek1$Arrest)
2152/(2152+18517)

#For what proportion of motor vehicle thefts in 2007 was an arrest made?
1212/(1212+13068)

#For what proportion of motor vehicle thefts in 2012 was an arrest made?
550/(550+13542)

#Problem 4.1 - Popular Locations
sort(table(mvtWeek1$LocationDescription))

TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvtWeek1, LocationDescription %in% TopLocations)

#R will remember the other categories of the LocationDescription variable from the original dataset, 
#so running table(Top5$LocationDescription) will have a lot of unnecessary output. 
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription)

#One of the locations has a much higher arrest rate than the other locations. 
#Which is it? Please enter the text in exactly the same way as how it looks in the answer options for Problem 4.1.
table(Top5$LocationDescription, Top5$Weekday)

####  STOCK DYNAMICS ####
boeing <- read.csv("~/Downloads/mit/BoeingStock.csv")
coca <- read.csv("~/Downloads/mit/CocaColaStock.csv")
ge <- read.csv("~/Downloads/mit/GEStock.csv")
ibm <- read.csv("~/Downloads/mit/IBMStock.csv")
procter <- read.csv("~/Downloads/mit/ProcterGambleStock.csv")

#convert date
ibm$Date = as.Date(ibm$Date, "%m/%d/%y")
ge$Date = as.Date(ge$Date, "%m/%d/%y")
coca$Date = as.Date(coca$Date, "%m/%d/%y")
procter$Date = as.Date(procter$Date, "%m/%d/%y")
boeing$Date = as.Date(boeing$Date, "%m/%d/%y")

##Problem 1- Summary Statistics

#What is the earliest year in our datasets?
#What is the latest year in our datasets?
summary(ibm$Date)

#What is the minimum stock price of General Electric (GE) over this time period?
summary(ge$StockPrice)
#What is the maximum stock price of Coca-Cola over this time period?
summary(coca$StockPrice)
#What is the median stock price of Boeing over this time period?
summary(boeing$StockPrice)
#What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(procter$StockPrice)

## Problem 2 - Visualizing Stock Dynamics
plot(coca$Date, coca$StockPrice, type="l", col="red")
#Around what year did Coca-Cola has its highest stock price in this time period?
#Around what year did Coca-Cola has its lowest stock price in this time period?
lines(procter$Date, procter$StockPrice)
#In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more?
abline(v=as.Date(c("2000-03-01")), lwd=2, col="yellow")

#Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the other was going down. Which one was going up?
abline(v=as.Date(c("1983-03-01")), lwd=2, col="yellow")


## Problem 3 - Visualizing Stock Dynamics 1995-2005

#This will plot the CocaCola stock prices from 1995 through 2005, which are the observations numbered from 301 to 432. The additional argument, ylim=c(0,210), makes the y-axis range from 0 to 210. 
#This will allow us to see all of the stock values when we add in the other companies.
plot(coca$Date[301:432], coca$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(procter$Date[301:432], procter$StockPrice[301:432], lty=2, col="blue" )
lines(boeing$Date[301:432], boeing$StockPrice[301:432], lty=3, col="purple" )
lines(ibm$Date[301:432], ibm$StockPrice[301:432], lty=4, col="black" )
lines(ge$Date[301:432], ge$StockPrice[301:432], lty=5, col="orange" )

#Which stock fell the most right after the technology bubble burst in March 2000?
abline(v=as.Date(c("2000-03-01")), lwd=2, col="black")

#Which stock reaches the highest value in the time period 1995-2005?
#Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price? 
abline(v=as.Date(c("1997-09-01")), lwd=2, col="black")
abline(v=as.Date(c("1997-11-01")), lwd=2)

#In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price?
abline(v=as.Date(c("2004-01-01")), lwd=2, col="black")
abline(v=as.Date(c("2006-01-01")), lwd=2)

## Problem 4 - Monthly Trends
#For IBM, compare the monthly averages to the overall average stock price. 
#In which months has IBM historically had a higher stock price (on average)? Select all that apply.
tapply(ibm$StockPrice, months(ibm$Date), mean)
summary(ibm$StockPrice)

#General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(ge$StockPrice, months(ge$Date), mean)
tapply(coca$StockPrice, months(coca$Date), mean)
#For the months of December and January, every company's average stock is higher in one month and lower in the other. In which month are the stock prices lower?

#### DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES ####
## Problem 1 - Loading and Summarizing the Dataset
CPSData <- read.csv("~/Downloads/mit/CPSData.csv")
str(CPSData)

#Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it.
#Which state has the fewest interviewees?
#Which state has the largest number of interviewees?
sort(table(CPSData$Industry))
sort(table(CPSData$State)) 

#What proportion of interviewees are citizens of the United States?
table(CPSData$Citizenship)
(116639+7073)/131302

#A number of interviewees are of Hispanic ethnicity, as captured by the Hispanic variable. 
#For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPSData$Race, CPSData$Hispanic)

## Problem 2 - Evaluating Missing Values
#Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPSData)

## pattern of missiong values
table(CPSData$Region, is.na(CPSData$Married))
table(CPSData$Sex, is.na(CPSData$Married))
table(CPSData$Age, is.na(CPSData$Married))
table(CPSData$Married, is.na(CPSData$Married))
table(CPSData$Citizenship, is.na(CPSData$Married))

#How many states had all interviewees living in a non-metropolitan area 
#(aka they have a missing MetroAreaCode value)?
#How many states had all interviewees living in a metropolitan area? 
#Again, treat the District of Columbia as a state.
table(CPSData$State, is.na(CPSData$MetroAreaCode))

#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPSData$Region, is.na(CPSData$MetroAreaCode))
10674/(10674+20010) #[1] 0.3478686
5609/(5609+20330) #[1] 0.2162381
9871/(9871+31631) #[1] 0.237844
8084/(8084+25093) #[1] 0.2436628

#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(CPSData$MetroAreaCode), CPSData$State,  mean))

## Problem 3 - Integrating Metropolitan Area Data
MetroAreaCodes <- read.csv("~/Downloads/mit/MetroAreaCodes.csv")
str(MetroAreaCodes)

CountryCodes <- read.csv("~/Downloads/mit/CountryCodes.csv")
str(CountryCodes)

#merge data
CPS = merge(CPSData, MetroAreaCodes, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
str(CPS)

#How many interviewees have a missing value for the new metropolitan area variable? 
table(is.na(CPS$MetroArea))

#Which of the following metropolitan areas has the largest number of interviewees?
sort(table(CPS$MetroArea))

#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? Hint: Use tapply() with mean, as in the previous subproblem. Calling sort() on the output of tapply() could also be helpful here.
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

#which metropolitan area has the smallest proportion of interviewees who have received no high school diploma
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

#Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian, determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == "Asian" , CPS$MetroArea, mean,na.rm=TRUE))

## Problem 4 - Integrating Country of Birth Data
CPS = merge(CPS, CountryCodes, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
#How many interviewees have a missing value for the new country of birth variable?
summary(CPS$Country)

#Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))

#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States?
table(CPS$MetroArea, CPS$Country)
summary(CPS$Country)
CPS$borncat[CPS$Country=='United States'] <- 0
CPS$borncat[CPS$Country!='United States'] <- 1
tapply(CPS$borncat, CPS$MetroArea, mean, na.rm=TRUE)

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

#Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? 
#Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question.
table(CPS$MetroArea, CPS$Country=="India")
table(CPS$MetroArea, CPS$Country=="Brazil")
table(CPS$MetroArea, CPS$Country=="Somalia")

#### INTERNET PRIVACY POLL (OPTIONAL) ####
## Problem 1 - Loading and Summarizing the Dataset
poll <- read.csv("~/Downloads/mit/AnonymityPoll.csv")
#How many people participated in the poll?
str(poll)

#How many people participated in the poll?
summary(poll$Smartphone)
table(poll$Smartphone)
table(poll$State, poll$Region)

#How many interviewees reported not having used the Internet and not having used a smartphone?
table(poll$Smartphone, poll$Internet.Use)

## Problem 2 - Internet and Smartphone Users
#How many interviewees have a missing value for their Internet use?
#How many interviewees have a missing value for their smartphone use?
summary(poll$Smartphone)
summary(poll$Internet.Use)

#Use the subset function to obtain a data frame called "limited", 
#which is limited to interviewees who reported Internet use or who reported smartphone use.
limited <- subset(poll, poll$Smartphone ==1 | poll$Internet.Use==1)
str(limited)

## Problem 3 - Summarizing Opinions about Internet Privacy
#Which variables have missing values in the limited data frame? (Select all that apply.)
summary(limited)

#What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
summary(limited$Info.On.Internet)

#How many interviewees reported a value of 0 for Info.On.Internet?
table(limited$Info.On.Internet==0)
#How many interviewees reported the maximum value of 11 for Info.On.Internet?
table(limited$Info.On.Internet==11)

#What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet? 
table(limited$Worry.About.Info)
386/(386+475)
#What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet?
table(limited$Anonymity.Possible)
278/(278+475)
#What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet?
table(limited$Tried.Masking.Identity)
128/(128+656)
#What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
table(limited$Privacy.Laws.Effective)
186/(186+541)

## Problem 4 - Relating Demographics to Polling Results
#Build a histogram of the age of interviewees. What is the best represented age group in the population?
hist(limited$Age)

#What is the largest number of interviewees that have exactly the same value in their Age variable AND the same value in their Info.On.Internet variable? 
plot(limited$Age, limited$Info.On.Internet, pch=20)
max(table(limited$Age, limited$Info.On.Internet))

#By running the command jitter(c(1, 2, 3)) multiple times, we can see that the jitter function randomly adds or subtracts a small value from each number, 
#and two runs will yield different results.
#What relationship to you observe between Age and Info.On.Internet?
plot(jitter(limited$Age), jitter(limited$Info.On.Internet), pch=20)

#What is the average Info.On.Internet value for smartphone users?
tapply(limited$Info.On.Internet, limited$Smartphone, summary)


#What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
#What proportion of non-smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
table(limited$Tried.Masking.Identity, limited$Smartphone)
93/(93+390)
33/(33+248)















