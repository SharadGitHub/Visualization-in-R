
str(mvt)
mvt = read.csv("mvt.csv", header = T)
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

str(mvt)

WeekdayCounts= as.data.frame(table(mvt$Weekday))
View(WeekdayCounts)


library(ggplot2)
ggplot(WeekdayCounts, aes(Var1, Freq))+ geom_line(aes(group=1), linetype=2, alpha=0.3)

WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, levels = c("Sunday", "Monday", 
                                                           "Tuesday", "Wednesday", 
                                                           "Thursday", "Friday","Saturday"))

DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
View(DayHourCounts)
DayHourCounts$Hour= as.numeric(as.character(DayHourCounts$Var2))

ggplot(DayHourCounts, aes(Hour, Freq)) + geom_line(aes(group= Var1, color = Var1, size =1))


### HeatMap 

DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered = T,levels = c("Monday", 
                                                           "Tuesday", "Wednesday", 
                                                           "Thursday", "Friday",
                                                           "Saturday", "Sunday"))

ggplot(DayHourCounts, aes(Hour, Var1)) +
  geom_tile(aes(fill= Freq))+ scale_fill_gradient(name= "Total MV Thefts", low= "white", high = "red")+
  theme(axis.title.y= element_blank())

install.packages("ggmap")
install.packages("ggsn")
library(maps)
library(ggmap)

chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)+ geom_point(data = mvt[1:100, ], aes(Longitude, Latitude))

LatLongCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude,2)))
str(LatLongCounts)

### converting factor variable into numerical variable
LatLongCounts$Long = as.numeric(as.character(LatLongCounts$Var1))
LatLongCounts$Lat = as.numeric(as.character(LatLongCounts$Var2))

ggmap(chicago)+ geom_point(data=LatLongCounts, 
                  aes(Long, Lat, color=LatLongCounts$Freq,size=LatLongCounts$Freq))+
  scale_colour_gradient(low="yellow", high="red")


### using heatmap
ggmap(chicago)+ geom_tile(data=LatLongCounts, 
                           aes(Long, Lat, alpha=Freq), fill="red")



#### using dataset murder
murder = read.csv("murders.csv")
str(murder)

statesmap = map_data("state")
str(statesmap)

ggplot(statesmap, aes(long, lat, group = group))+ geom_polygon(fill= "white", color="black")

murder$region = tolower(murder$State)

murdermap = merge(statesmap, murder, by="region")
str(murdermap)

ggplot(murdermap, aes(long, lat, group= group, fill= Population))+ geom_polygon(color= "black")+
  scale_fill_gradient(low="black", high="red", guide="legend")


murdermap$murderRate = murdermap$Murders/ murdermap$Population *100000

ggplot(murdermap, aes(long, lat, group= group, fill= murderRate))+ geom_polygon(color= "black")+
  scale_fill_gradient(low="black", high="red", guide="legend", limit= c(0,10))



#### recitation 
intl= read.csv("intl.csv")
str(intl)
table(intl$Region)
ggplot(intl , aes(Region, PercentOfIntl))+ geom_bar(stat = "identity")

## to order Region acc to percentofintl
intl = transform(intl, Region= reorder(Region, - PercentOfIntl))
intl$PercentOfIntl= intl$PercentOfIntl*100

ggplot(intl , aes(Region, PercentOfIntl))+ geom_bar(stat = "identity", fill= "blue", alpha=0.2)+
  geom_text(aes(label = PercentOfIntl, vjust= -0.4))+ ylab("Percent of International Students")+
  theme(axis.title.x = element_blank(), axis.text.x= element_text(angle=45, hjust = 1))  




### data intlall
intlall = read.csv("intlall.csv", stringsAsFactors = F)
str(intlall)

intlall[is.na(intlall)]= 0

world_map = map_data("world")
str(world_map)

ggplot(world_map, aes(long, lat, group= group))+ geom_polygon(fill="white", color= "black")

## merging datasets
world_map = merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)
table(intlall$Citizenship)

world_map= world_map[order(world_map$group, world_map$order),]

ggplot(data= world_map, aes(long, lat, group= group))+ 
  geom_polygon(aes(fill=Total, color= "black"))+coord_map("mercator")


intlall$Citizenship[intlall$Citizenship== "China (People's Republic Of)"]= "China"
world_map = merge(map_data("world"), intlall, by.x = "region", by.y = "Citizenship")
world_map= world_map[order(world_map$group, world_map$order),]

#### 3D view

ggplot(data= world_map, aes(long, lat, group= group))+ 
  geom_polygon(aes(fill=Total, color= "black"))+coord_map("ortho", orientation = c(20,40,0))


#### household data
household = read.csv("households.csv")

str(household)

## in order to use the whole data we need to melt it first
library(reshape2)

melt(household, id="Year")
ggplot(melt(household, id="Year"), aes(Year, value, color= variable)) + geom_line(size=2)+
  geom_point(size=3)
