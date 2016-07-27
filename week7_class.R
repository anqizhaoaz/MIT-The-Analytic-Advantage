
#### Week 7 In-class assignment

who <- read.csv("~/Downloads/mit/WHO.csv")
str(who)
plot(who$GNI, who$FertilityRate)

library(ggplot2)

## basic scatterplot
scatterplot = ggplot(who, aes(x=GNI, y=FertilityRate))
fertility_GNI = scatterplot + geom_point(col="blue", size=3, shape=17) +
  ggtitle("scatterplot of GNI and Fertility Rate")

## save plot
pdf("~/Downloads/mit/Myplot.pdf")
print(fertility_GNI)
dev.off()

ggplot(who, aes(x=GNI, y=FertilityRate, color=Region)) +
  geom_point( size=3, shape=15)

ggplot(who, aes(x=GNI, y=FertilityRate, color=LifeExpectancy)) +
  geom_point( size=3, shape=15)

ggplot(who, aes(x=FertilityRate,y=Under15)) +
  geom_point( size=3, shape=3) +
  stat_smooth(method="lm", level=0.95, color="orange")

ggplot(who, aes(x = FertilityRate, y = Under15, color=Region)) + geom_point() +
  scale_color_brewer(palette="Dark2")


######################  Crime data  ################
mvt <- read.csv("~/Downloads/mit/mvt.csv")
str(mvt)

## convert to date, weekday and hour
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

table(mvt$Weekday)
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

## Load the ggplot2 library:
library(ggplot2)

## Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))  

## Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))

## Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

## Change our x and y labels:
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1), linetype=2) + 
  xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1), alpha=0.3) + 
  xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")
## The linetype parameter makes the line dashed, and the alpha parameter makes the line lighter in color, or more transparent. 

#### create a heat map
# Create a counts table for the weekday and hour:
table(mvt$Weekday, mvt$Hour)

# Save this to a data frame:
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)

# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

# Create out plot:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))

# Change the colors
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=1)

# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")

# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=1) 

# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=1, alpha=0.5) 

# Fix the order of the days:
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# Change the label on the legend, and get rid of the y-label:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name="Total MV Thefts") + 
  theme(axis.title.y = element_blank())

# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + 
  theme(axis.title.y = element_blank())

#### Video 5: A Geographical Hot Spot Map
library(maps)
library(ggmap)

# Load a map of Chicago into R:
chicago = get_map(location = "chicago", zoom = 11)

# Look at the map
ggmap(chicago)

# Plot the first 100 motor vehicle thefts:
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))


# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts data frame for each area:
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
str(LatLonCounts)

LatLonCounts2 = subset(LatLonCounts, LatLonCounts$Freq>0)
str(LatLonCounts2)

# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq)) + scale_colour_gradient(low="yellow", high="red")

# We can also use the geom_tile geometry
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")

## We've created a geographical heat map, which in our case shows a visualization of the data, but it could also show the predictions of a model. Now that our heat map is loaded, let's take a look.



#### Video 6: A Heatmap on the United States
murders <- read.csv("~/Downloads/mit/murders.csv")
str(murders)

# Load the map of the US
statesMap = map_data("state")
str(statesMap)

# Plot the map:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)

# Join the statesMap data and the murders data into one dataframe:
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000

# Redo our plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "white", high = "red", guide = "legend")

# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "white", high = "red", guide = "legend", limits = c(0,10))

ggplot(murderMap, aes(x = long, y = lat, group=group, fill = GunOwnership)) + 
  geom_polygon(color="black") + scale_fill_gradient(low = "black", high = "red", guide="legend")


##################  Recitation ##############
intl <- read.csv("~/Downloads/mit/intl.csv")
str(intl)

library(ggplot2)
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat="identity") + #The height of the bar is the value of the y variable.
  geom_text(aes(label=PercentOfIntl))

intl$PercentOfIntl = intl$PercentOfIntl *100

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat="identity", fill="grey") + #The height of the bar is the value of the y variable.
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) +
  ylab("Percentage of International Student") +
  theme(axis.title.x = element_blank(), axis.text.x =element_text(angle=45, hjust=0.5))


##### Video 5: World Maps in R #####
library(ggmap)

intlall <- read.csv("~/Downloads/mit/intlall.csv")
head(intlall)
str(intlall)

intlall[is.na(intlall)] =0

## load map
world_map = map_data("world")
str(world_map)

## merge two dataset
world_map = merge(world_map, intlall, by.x="region", by.y = "Citizenship")

## plot world map
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black")

world_map = world_map[order(world_map$group, world_map$order),]

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black")

## china is missing due to different name in two dataset
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"
world_map = merge(world_map, intlall, by.x="region", by.y = "Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]


## replot
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator")






