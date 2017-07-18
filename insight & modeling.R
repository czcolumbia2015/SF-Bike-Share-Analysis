station = read.csv("/Users/chenzheng/Downloads/station.csv")
status =  read.csv("/Users/chenzheng/Downloads/status.csv")
trip = read.csv("/Users/chenzheng/Downloads/trip.csv")
weather = read.csv("/Users/chenzheng/Downloads/weather.csv")

head(trip) 
head(status) 
head(station) 
head(weather) 


##preprocessing trip dataset:
##convert factor to datetime
install.packages("lubridate")  
library("lubridate")
trip$start_date = mdy_hm(as.character(trip$start_date))
trip$end_date = mdy_hm(as.character(trip$end_date))

install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
trip$date = as.Date(trip$start_date)
trip$end_date = as.Date(trip$end_date)
trip_count = trip %>% group_by(date) %>% summarise(count = n())

##feature engineer, create month, day of week, hour of day.
trip$start_month = month(trip$start_date)
trip$start_wday = wday(as.Date(trip$start_date), label = TRUE)
trip$start_hour = hour(trip$start_date)
trip$end_month = month(trip$end_date)
trip$end_wday = wday(as.Date(trip$end_date), label = TRUE)
trip$end_hour = hour(trip$end_date)
##convert seconds to minutes
trip$duration = trip$duration/60 
trip$is_weekend = ifelse(trip$start_wday %in% c("Sun", "Sat"), 1, 0)
trip$is_weekend = factor(trip$is_weekend, labels = c("weekday", "weekend"))
trip$is_weekend_v2 = ifelse(trip$end_wday %in% c("Sun", "Sat"), 1, 0)
trip$is_weekend_v2 = factor(trip$is_weekend_v2, labels = c("weekday", "weekend"))
##############insight getting##########
trip_date = trip %>% group_by(date) %>% summarise(trip_count = n())
ggplot(trip_date, aes(x = date, y = trip_count)) + geom_point() + geom_smooth(color = "#1A1A1A") + 
  labs(x = "Date", y = "# of trips", title = "Daily # of trips in San Franciso from 2013 - 2015") +
  theme(plot.title = element_text(hjust = 0.5))



isweekend_date = trip %>% group_by(date, is_weekend) %>% summarise(count = n())
isweekend_date$is_weekend = factor(isweekend_date$is_weekend, labels = c("weekday", "weekend"))

ggplot(isweekend_date, aes(x = date, y=count)) + geom_point(aes(color = is_weekend), size = 3, alpha = 0.65) +
  labs(x = "Date", y = "Total # of Trips Per Day", title = "Total # of Trips in San Francisco from 2013 - 2015")

ggplot(isweekend_date, aes(x = date, y=count)) + 
  geom_point() +
  facet_wrap(~ is_weekend) +
  geom_smooth(se = F) +
  labs(x = "Date", y = "Total # of Trips Per Day", title = "Total # of Trips in San Francisco from 2013 - 2015") +
  theme(plot.title = element_text(hjust = 0.5))


hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))

trip$start_wday <- factor(trip$start_wday)
trip$start_hour <- factor(trip$start_hour, level = 0:23, label = hour_format)

trip_hour_wday = trip %>% group_by(start_wday, start_hour) %>% summarise(count=n())

ggplot(trip_hour_wday, aes(x = start_hour, y = start_wday, fill = count)) + geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), legend.title = element_blank(), 
        legend.position = "top", legend.direction = "horizontal") +
  labs(x = "Hour of trips", y = "Day of Week of trips", title = "# of bicycle trips in San Francisco from 2013 - 2015") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(plot.title = element_text(hjust = 0.5))





isweekend_date = trip %>% group_by(date, is_weekend) %>% summarise(count = n())
isweekend_date$is_weekend = factor(isweekend_date$is_weekend, labels = c("weekday", "weekend"))

ggplot(isweekend_date, aes(x = date, y=count)) + 
  geom_point() +
  facet_wrap(~ is_weekend) +
  geom_smooth(se = F) +
  labs(x = "Date", y = "Total # of Trips Per Day", title = "Total # of Trips in San Francisco from 2013 - 2015") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(trip, aes(x = date)) + 
  geom_bar(aes(color=subscription_type), stat="count", position = "stack") +
  facet_grid(~is_weekend) +
  labs(x = "Day of Week", y = "# of trips", 
       title = "Customer Vs.Subscriber on Weekend and Weekdays") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())

#subscription_month = trip %>% group_by(subscription_type, start_month) %>% summarise(count = n())
#ggplot(subscription_month, aes(x = start_month, label=T, y = count)) + 
#  geom_bar(stat = "identity",  aes(fill=subscription_type)) +
#  labs(x = "Month of Year", y = "# of trips") +
#  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


ggplot(station_trip, aes(x = date)) + 
  geom_bar(aes(color=subscription_type), stat="count", position = "stack") +
  facet_wrap(~city, scales = "free_y") +
  labs(x = "Day of Week", y = "# of trips", 
       title = "Customer Vs.Subscriber by City") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())

hist(trip$duration, breaks = 100, xlab="Minutes" )

duration_mean = station_trip %>% group_by(start_wday,city) %>% mutate(sum_duration = sum(duration), count = n())
duration_day_city = duration_mean %>% group_by(start_wday,city) %>% summarise(duration_mean = mean(sum_duration/count))
ggplot(duration_day_city, aes(x = start_wday, y = duration_mean)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = city)) +
  labs(y = "Average Trip Duration")
  ##theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())



start_station = trip %>% group_by(start_station_name) %>%
  summarise(count = n()) %>% arrange(desc(count))

station_hour = trip %>% group_by(start_station_name, start_hour, is_weekend) %>% 
  filter(start_station_name %in% start_station$start_station_name[1:6]) %>% summarise(count = n())

ggplot(station_hour, aes(x = as.numeric(start_hour), y = count, colour = is_weekend)) +
  facet_wrap(~start_station_name, ncol = 2) + geom_line(aes(group = is_weekend)) +
  geom_point(aes(group = is_weekend)) + 
  labs(title = "Distribution of trips starting from each station across time by weekday/weekend", 
       x = "Time (hr)", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())





colnames(trip)[1] = "id_2"
colnames(trip)[5] = "id"
station_trip = merge(trip, station, by = "id")

library(plotly)
city_wday = station_trip %>% group_by(start_wday, city) %>% summarise(count = n())
g = ggplot(city_wday, aes(y = city, x = start_wday)) +
  geom_point(aes(size = count, col = count)) +
  scale_size(range = c(1,10)) + 
  
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())
ggplotly(g, tooltip = c("x", "y", "colour"))

install.packages("ggmap")
library("ggmap")

bbox = c(-122.4990, 37.31072, -121.7800, 37.88100)
sf = get_map(location = bbox, source = "stamen", maptype = "toner-lite")

map_trip = station_trip %>% group_by(long, lat, zip_code) %>% summarise(count = n())

ggmap(sf) + geom_point(data = map_trip, aes(x = long, y = lat, size = count, color = count)) +
  scale_size(name = "# Total Trips", range = c(3, 12)) 


end_station_trip = merge(trip, station, by.x = "end_station_id", by.y = "id")[, c("id_2","lat", "long", "city")]
names(end_station_trip)[2] = "end_lat"
names(end_station_trip)[3] = "end_long"

road_df = merge(station_trip, end_station_trip, by = "id_2") %>% 
  select ("id_2","lat", "start_long", "end_lat", "end_long", "city.y", "city.x") %>% 
  filter(city.y == "San Francisco" & city.x == "San Francisco")
  
road_map = road_df %>% group_by(lat, start_long, end_lat, end_long) %>% summarise(num_trips = n())
station_sf = station %>% filter(city=="San Francisco")

install.packages("ggrepel")
library("ggrepel")
ggplot(road_map) + geom_segment(aes(x=start_long, xend = end_long, y = lat, yend=end_lat, 
                                    size = num_trips, colour = num_trips, alpha = num_trips)) +
  geom_point(data = station_sf, aes(x=long, y=lat), size = 4) +
  geom_text_repel(data = station_sf, aes(x=long, y=lat, label=name), size = 4) +
  theme_light(base_size = 10) +
  scale_colour_gradient(low ="#132B43", high = "#56B1F7", limits=c(0, max(road_map$num_trips)), name="Number of Trips") +
  scale_alpha(limits=c(0, max(road_map$num_trips)), guide = F) +
  scale_size(limits=c(0, max(road_map$num_trips)), guide = F) +
  xlab("") + ylab("") + coord_fixed() +
  theme(axis.line = element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank())

##usage by isweekend/event/hour/
library(plotly)
######################## data file for shiny ###########################
trip_segment = data.frame(table(station_trip$bike_id, station_trip$start_hour, 
                                station_trip$start_wday,station_trip$city)) 
trip_segment$hour = as.factor(trip_segment$Var2)
trip_segment$Wday = as.factor(trip_segment$Var3)
trip_segment$City = as.factor(trip_segment$Var4)
trip_segment = trip_segment %>% select(Var1, hour, Wday, City, Freq)
write.csv(trip_segment, file = "/Users/chenzheng/Downloads/trip_segment.csv")
##############################################################################


len_bike = length(unique(trip$bike_id))
bike_rates = data.frame(table(bike_date$Var2, bike_date$Freq>0)/len_bike)
bike_rates$Date = as.Date(bike_rates$Var1)
bike_rates$Percent = bike_rates$Freq * 100
bike_rates$Day = as.numeric(bike_rates$Var1)

p1 = plot_ly(subset(bike_rates, Var2 == "TRUE"), 
             x = ~Date,
             y = ~Percent,
             type = "scatter",
             mode = "markers",
             color = I("orange"),
             alpha = 0.5,
             hoverinfo = "text",
             text = ~paste("Date: ", Date,
                           "Percent:", round(Percent, 2))) %>%
  add_lines(y = ~fitted(loess(Percent ~ Day)),
            color = I("blue")) %>%
  layout(xaxis = list(title = "San Francisco"),
         yaxis = list(title = "Percentage of Bikes Used"))
p1
##################################################################################################
##################################################################################################
##################################################################################################

##preprocessing weather data
weather = weather[weather$zip_code == 94107,]

weather$date = mdy(as.character(weather$date))
#set nan value as "Normal" in the event 
levels(weather$events) = c(levels(weather$events), "Normal")
weather$events[weather$events==''] = "Normal"
weather$events[weather$events=='rain'] = "Rain"
droplevels(weather, except = c("Normal", "Fog", "Rain", "Fog-Rain", "Rain-Thunderstorm"))

weather$precipitation_inches = as.numeric(as.matrix(weather$precipitation_inches))

weather = weather %>% group_by(date) %>% mutate(median = median(precipitation_inches, na.rm=T))
weather$precipitation_inches = ifelse(is.na(weather$precipitation_inches), weather$median, weather$precipitation_inches)
weather$precipitation_inches[is.na(weather$precipitation_inches)] = 0
summary(weather)

#max_wind_Speed_mph and max_gust_speed_mph are well correlated, so we use max_wind to help fill the null
#valuesof max_gust
numeric_cols = sapply(weather, is.numeric)
corr = cor(weather[,numeric_cols], use = "complete.obs")

weather = weather %>% group_by(max_wind_Speed_mph) %>% mutate(gust_median = median(max_gust_speed_mph, na.rm=T))
weather$max_gust_speed_mph = ifelse(is.na(weather$max_gust_speed_mph), weather$gust_median, weather$max_gust_speed_mph)


##feature engineer, create month, day of week, dummay for weekend days, seasonal variable

weather$month = month(weather$date)
weather$wday = wday(weather$date)
weather$weekend = ifelse(weather$wday %in% c(6,7), 1, 0)

weather$season = weather$month
weather$season[weather$season==12] = 2
weather$season[weather$season==1] = 2
weather$season[weather$season==3] = 5
weather$season[weather$season==4] = 5
weather$season[weather$season==6] = 8
weather$season[weather$season==7] = 8
weather$season[weather$season==9] = 11
weather$season[weather$season==10] = 11
weather$season = factor(weather$season, labels = c("Sum", "Aut", "Win", "Spr"))

install.packages("timeDate")
library("timeDate")

listHolidays()

Holiday = c(
  as.Date(USChristmasDay(2013)),
  as.Date(USColumbusDay(2013)),
  as.Date(USCPulaskisBirthday(2013)),
  as.Date(USDecorationMemorialDay(2013)),
  as.Date(USElectionDay(2013)),
  as.Date(USGoodFriday(2013)),
  as.Date(USInaugurationDay(2013)),
  as.Date(USIndependenceDay(2013)),
  as.Date(USLaborDay(2013)),
  as.Date(USLincolnsBirthday(2013)),
  as.Date(USMemorialDay(2013)),
  as.Date(USMLKingsBirthday(2013)),
  as.Date(USNewYearsDay(2013)),
  as.Date(USPresidentsDay(2013)),
  as.Date(USThanksgivingDay(2013)),
  as.Date(USVeteransDay(2013)),
  as.Date(USWashingtonsBirthday(2013)),
  as.Date(USChristmasDay(2014)),
  as.Date(USColumbusDay(2014)),
  as.Date(USCPulaskisBirthday(2014)),
  as.Date(USDecorationMemorialDay(2014)),
  as.Date(USElectionDay(2014)),
  as.Date(USGoodFriday(2014)),
  as.Date(USInaugurationDay(2014)),
  as.Date(USIndependenceDay(2014)),
  as.Date(USLaborDay(2014)),
  as.Date(USLincolnsBirthday(2014)),
  as.Date(USMemorialDay(2014)),
  as.Date(USMLKingsBirthday(2014)),
  as.Date(USNewYearsDay(2014)),
  as.Date(USPresidentsDay(2014)),
  as.Date(USThanksgivingDay(2014)),
  as.Date(USVeteransDay(2014)),
  as.Date(USWashingtonsBirthday(2014)),
  as.Date(USChristmasDay(2015)),
  as.Date(USColumbusDay(2015)),
  as.Date(USCPulaskisBirthday(2015)),
  as.Date(USDecorationMemorialDay(2015)),
  as.Date(USElectionDay(2015)),
  as.Date(USGoodFriday(2015)),
  as.Date(USInaugurationDay(2015)),
  as.Date(USIndependenceDay(2015)),
  as.Date(USLaborDay(2015)),
  as.Date(USLincolnsBirthday(2015)),
  as.Date(USMemorialDay(2015)),
  as.Date(USMLKingsBirthday(2015)),
  as.Date(USNewYearsDay(2015)),
  as.Date(USPresidentsDay(2015)),
  as.Date(USThanksgivingDay(2015)),
  as.Date(USVeteransDay(2015)),
  as.Date(USWashingtonsBirthday(2015))
)

weather$isholiday = ifelse(weather$date %in% Holiday, 1, 0)

#####add number of trips by day into weather data frome for further analysis.
df = merge(trip_count, weather, by = "date")
#events_dummy = model.matrix(~events, df)
#season_dummy = model.matrix(~season, df)

dat = df[, -c(27, 26, 25)]
## remove outliers
quantile(dat$count, c(seq(0, 1, by=0.05)))
dat = dat[dat$count<1360 & dat$count>300.0, ]
dat = dat[dat$date>"2013-09-15",]


install.packages("randomForest")
library("randomForest")
train_sample = sample(nrow(dat), nrow(dat)*0.6)
train_data = dat[train_sample, -c(1, 2)]
test_data = dat[-train_sample, -c(1, 2)]
train_data.y = dat$count[train_sample]
test_data.y = dat$count[-train_sample]



RF = function(train.dat.x, train.dat.y, test.dat.x, test.dat.y, ntree, mtry){
  model = randomForest(train.dat.x, train.dat.y, test.dat.x, test.dat.y, 
                       ntree = ntree, mtry = mtry, importance=T)
  return (model$test$mse)
}

##model = randomForest(train_data, train_data.y, test_data, test_data.y, ntree = 300, mtry=17, importance = T)
## generate cv indices
cv.index = sample(rep(1:10, nrow(train_data)/10))
## list of values for ntree
ntree.list = c(100, 200, 300, 400, 500)
## list of values for mtry
mtry.list = c(16, 17, 18, 19, 20, 21)

## 10 fold cross validation to tune the model parameters - ntree, mtry
mse.df = vector()
for (j in 1:5){
  mse.matrix = vector()
  for (k in 1:6){
    mtry.vec = vector()
    for (i in 1:10){
      cv.train.x = train_data[cv.index != i, ]
      cv.train.y = train_data.y[cv.index != i]
      cv.test.x = train_data[cv.index == i, ]
      cv.test.y = train_data.y[cv.index == i]
      rf.result = RF(cv.train.x, cv.train.y, cv.test.x, cv.test.y, ntree.list[j], mtry.list[k])
      mtry.vec = c(mtry.vec, rf.result)
    }
    mse.matrix = rbind(mse.matrix, mtry.vec)
  }
  mse_ntree = rowMeans(mse.matrix)
  mse.df = rbind(mse.df,mse_ntree)
}

error = data.frame(c(rep(100, 6), rep(200, 6), rep(300, 6), rep(400, 6), rep(500, 6)), 
                   c(rep(c(16,17,18,19,20,21), 5)),
                   c(
                     22844.07, 22590.95, 22368.30, 22040.11, 22391.56, 21587.57,
                     21061.92, 21902.98, 21856.27, 21567.72, 21308.19, 21412.00,
                     20877.43, 21053.98, 21370.86, 21420.36, 21131.19, 21317.35,
                     21504.48, 21154.12, 20914.82, 21398.15, 21308.60, 21139.50,
                     21741.88, 21186.32, 21006.03, 21159.47, 20877.40, 21325.12))

names(error) = c("ntree", "mtry", "value")
error$mtry= as.factor(error$mtry)
ggplot(error, aes(x=ntree, y=value, colour = mtry)) + geom_line() +geom_point() +
  labs(x="number of trees", y="Test MSE")

best_model = randomForest(count~., data = dat[train_sample, c(2, 5, 9, 11, 12, 22, 24, 27)], ntree=500, mtry=3)
Predicted_Value = predict(best_model, newdata = dat[-train_sample, -1])

mean((Predicted_Value-test_data.y)^2)

a = data.frame(dat[-train_sample, c(1,2)], Predicted_Value)
names(a) = c("date", "Actual", "Prediction")

install.packages("reshape2")
library("reshape2")
AP = melt(a, id = "date")
AP$variable= as.factor(AP$variabl)
ggplot(AP, aes(x = date, y = value, colour = variable)) + geom_line() +
  labs(x = "Date", y = "number of trips", title = "Predicted Values vs Actual Values") +
  theme(plot.title = element_text(hjust = 0.5))


VI_F = varImpPlot(best_model)
partialPlot(best_model, dat[train_sample, -1], wday)
partialPlot(best_model, dat[train_sample, -1], max_temperature_f)
partialPlot(best_model, dat[train_sample, -1], weekend)
partialPlot(best_model, dat[train_sample, -1], precipitation_inches)


barplot(t(VI_F)/sum(VI_F))

install.packages("rpart")
library("rpart")
tree = rpart(count~., data = dat[train_sample, -1], method= "anova")
plot(tree, uniform=T)
text(tree, use.n = T, all = T, cex = .8)

## xgboost algorithm
install.packages("gbm")
library("gbm")

boost.train_data = dat[train_sample, -1]
boost.test_data = dat[-train_sample, -1]
boost.train_y = dat$count[train_sample]
boost.test_y = dat$count[-train_sample]

Boost = function(train_data, train.dat.y, test_data, test_data.y, tree_num, depth){
  boost.model = gbm(train.dat.y~., data=train_data, distribution = "gaussian", 
                    n.trees = tree_num, interaction.depth = depth, shrinkage = 0.01)
  yhat.boost = predict(boost.model, newdata = test_data, n.trees = tree_num)
  return (mean((yhat.boost-test_data.y)^2))
}

## generate cv indices
cv.index = sample(rep(1:10, nrow(boost.train_data)/10))
## list of values for ntree
tree_num.list = c(100, 400, 800, 1200, 1600)
## list of values for mtry
depth.list = c(1,2,6)


boost.df = vector()
for (j in 1:5){
  boost.mse.matrix = vector()
  for (k in 1:3){
    depth.vec = vector()
    for (i in 1:10){
      cv.boost.train.x = boost.train_data[cv.index != i, ]
      cv.boost.train.y = boost.train_y[cv.index != i]
      cv.boost.test.x = boost.train_data[cv.index == i, ]
      cv.boost.test.y = boost.train_y[cv.index == i]
      boost.result = Boost(cv.boost.train.x, cv.boost.train.y, cv.boost.test.x, cv.boost.test.y, 
                           tree_num.list[j], depth.list[k])
      depth.vec = c(depth.vec, boost.result)
    }
    boost.mse.matrix = rbind(boost.mse.matrix, depth.vec)
  }
  mse_boost = rowMeans(boost.mse.matrix)
  boost.df = rbind(boost.df,mse_boost)
}

boost.error = data.frame(c(rep(100, 3), rep(400, 3), rep(800, 3), rep(1200, 3), rep(1600, 3)), 
                   c(rep(c(1,2,6), 5)),
                   c(
                     28682.97875, 21851.44236, 18661.27864, 1051.89417, 219.92538, 63.35954,
                     49.51766,  15.98051, 13.19308, 14.65639, 14.33493, 12.48332,
                     11.97299,  13.67351, 13.35441))

names(boost.error) = c("ntree", "depth", "value")
boost.error$depth= as.factor(boost.error$depth)
ggplot(boost.error, aes(x=ntree, y=value, colour = depth)) + geom_line() +geom_point() +
  labs(x="number of trees", y="Test MSE")


boost.model = gbm(boost.train_y~., data=boost.train_data, distribution = "gaussian", 
                  n.trees = 400, interaction.depth = 6, shrinkage = 0.01)
yhat.boost = predict(boost.model, newdata = boost.test_data, n.trees = 400)
mean((yhat.boost-boost.test_y)^2)

boost.a = data.frame(dat[-train_sample, c(1,2)], yhat.boost)
names(boost.a) = c("date", "Actual", "Prediction")

boost.AP = melt(boost.a, id = "date")
boost.AP$variable= as.factor(boost.AP$variable)
ggplot(boost.AP, aes(x = date, y = value, colour = variable)) + geom_line() +
  labs(x = "Date", y = "number of trips", title = "Predicted Values vs Actual Values") +
  theme(plot.title = element_text(hjust = 0.5))


