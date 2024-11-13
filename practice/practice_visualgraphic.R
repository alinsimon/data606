library(tidyverse)
library(openintro)
library(psych)
# adding description of code
carrier <- c('9E','AA','AS','B6','DL','EV','F9','FL','HA','MQ','OO','UA','US','VX','WN','YV')
carrier_name <- c('Endeavor Air Inc.','American Airlines Inc.','Alaska Airlines Inc.','JetBlue Airways',
                  'Delta Air Lines Inc.','ExpressJet Airlines Inc.','Frontier Airlines Inc.','AirTran Airways Corporation',
                  'Hawaiian Airlines Inc.','Envoy Air','SkyWest Airlines Inc.','United Air Lines Inc.',
                  'US Airways Inc.','Virgin America','Southwest Airlines Co.','Mesa Airlines Inc.')
carrierinfo <- data_frame(carrier,carrier_name)
nycflights <- merge(x = nycflights, y = carrierinfo, by = "carrier", all = TRUE)

#adding description of month
month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
month_name <- month.name
month_info <- data_frame(month,month_name)
nycflights <- merge(x = nycflights, y = month_info, by = "month", all = TRUE)


describe(nycflights)
psych::describeBy(nycflights,nycflights$month_name)

#Number Of flights per month
NumFlightnyc_month <- nycflights %>%
  group_by(month_name) %>%
  summarise( n_flights = n()) |>
  arrange(month)

ggplot(NumFlightnyc_month, aes( x = month_name, y = n_flights, color=month_name)) +
  geom_point() 

#Number Of flights
ggplot(nycflights, aes( x = month_name , color=month_name)) +
  geom_bar() +

# arr delay per month
ggplot(nycflights, aes( x = month_name ,  y = arr_delay, color=month_name)) +
  geom_boxplot() +
  coord_flip()+
  stat_summary(fun = "mean", geom = "point", shape = 1,size = 2, color = "red")+
  stat_summary(fun = "median", colour = "blue", size = 2, geom = "point")+
  theme(legend.position = "top")+
  xlab('Month')+
  ylab('Number Of Flights')

#professor
ggplot(nycflights, aes( x = month_name ,  y = arr_delay, color=month_name)) +
  geom_boxplot() +
  coord_flip()+
  stat_summary(fun = "mean", geom = "point", shape = 1,size = 2, color = "red")+
  stat_summary(fun = "median", colour = "blue", size = 2, geom = "point")+
  theme(legend.position = "top")+
  xlab('Month')+
  ylab('Number Of Flights') +
  scale_x_discrete(limits = rev(month_info$month_name))


ggplot(data = nycflights, aes(x = month_name, y = arr_delay), color=disp) +
  geom_point() +
  stat_summary(fun = "median", geom = "point", shape = 1,size = 2, color = "red")

ggplot(data = nycflights, aes(x = month_name, y = arr_delay), color=disp) +
  geom_line()

