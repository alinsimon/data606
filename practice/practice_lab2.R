

carrier <- c('9E','AA','AS','B6','DL','EV','F9','FL','HA','MQ','OO','UA','US','VX','WN','YV')
carrier_name <- c('Endeavor Air Inc.','American Airlines Inc.','Alaska Airlines Inc.','JetBlue Airways',
                  'Delta Air Lines Inc.','ExpressJet Airlines Inc.','Frontier Airlines Inc.','AirTran Airways Corporation',
                  'Hawaiian Airlines Inc.','Envoy Air','SkyWest Airlines Inc.','United Air Lines Inc.',
                  'US Airways Inc.','Virgin America','Southwest Airlines Co.','Mesa Airlines Inc.')
carrierinfo <- data_frame(carrier,carrier_name)

sfo_feb_flights <- merge(x = sfo_feb_flights, y = carrierinfo, by = "carrier", all = TRUE)

sfo_feb_flights

carrier <- c('9E','AA','AS','B6','DL','EV','F9','FL','HA','MQ','OO','UA','US','VX','WN','YV')
carrier_name <- c('Endeavor Air Inc.','American Airlines Inc.','Alaska Airlines Inc.','JetBlue Airways',
                  'Delta Air Lines Inc.','ExpressJet Airlines Inc.','Frontier Airlines Inc.','AirTran Airways Corporation',
                  'Hawaiian Airlines Inc.','Envoy Air','SkyWest Airlines Inc.','United Air Lines Inc.',
                  'US Airways Inc.','Virgin America','Southwest Airlines Co.','Mesa Airlines Inc.')
carrierinfo <- data_frame(carrier,carrier_name)
sfo_feb_flights <- merge(x = sfo_feb_flights, y = carrierinfo, by = "carrier", all = TRUE)

sfo_feb_flights %>%
  group_by(carrier_name) %>%
  summarise(median_dd = median(dep_delay, na.rm = TRUE), iqr_dd = IQR(dep_delay, na.rm = TRUE), n_flights = n())


#Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)

#Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)

carrier <- c('9E','AA','AS','B6','DL','EV','F9','FL','HA','MQ','OO','UA','US','VX','WN','YV')
#Description of carrier codes
carrier_name <- c('Endeavor Air Inc.','American Airlines Inc.','Alaska Airlines Inc.','JetBlue Airways',
                  'Delta Air Lines Inc.','ExpressJet Airlines Inc.','Frontier Airlines Inc.','AirTran Airways Corporation',
                  'Hawaiian Airlines Inc.','Envoy Air','SkyWest Airlines Inc.','United Air Lines Inc.',
                  'US Airways Inc.','Virgin America','Southwest Airlines Co.','Mesa Airlines Inc.')
#create a new df with previous information
carrierinfo <- data_frame(carrier,carrier_name)
#create a summary with median and Interquartile
Summary_sfo_feb_flights <-  sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_arr_delay = median(arr_delay),var_arr_delay = var(arr_delay),
            sd_arr_delay = sd(arr_delay), mean_arr_delay = mean(arr_delay),count(arr_delay),
            iqr_arr_delay = IQR(arr_delay), n_flights = n())
Summary_sfo_feb_flights <- merge(x = carrierinfo, y =Summary_sfo_feb_flights , by = "carrier" , all.y = TRUE)

Summary_sfo_feb_flights <- sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_arr_delay = median(arr_delay),var_arr_delay = var(arr_delay),
            sd_arr_delay = sd(arr_delay), mean_arr_delay = mean(arr_delay),n_flightsdisc = n_distinct(arr_delay),
            iqr_arr_delay = IQR(arr_delay), n_flights = n())

ggplot(sfo_feb_flights, aes(x=arr_delay)) + geom_density()

ggplot(data = sfo_feb_flights, aes(x = arr_delay, y = carrier))+
  geom_point()

#Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)

#Cross join: merge(x = df1, y = df2, by = NULL)


carrier <- c('9E','AA','AS','B6','DL','EV','F9','FL','HA','MQ','OO','UA','US','VX','WN','YV')

carrier_name <- c('Endeavor Air Inc.','American Airlines Inc.','Alaska Airlines Inc.','JetBlue Airways',
                  'Delta Air Lines Inc.','ExpressJet Airlines Inc.','Frontier Airlines Inc.','AirTran Airways Corporation',
                  'Hawaiian Airlines Inc.','Envoy Air','SkyWest Airlines Inc.','United Air Lines Inc.',
                  'US Airways Inc.','Virgin America','Southwest Airlines Co.','Mesa Airlines Inc.')

carrierinfo <- data_frame(carrier,carrier_name)

Summary_sfo_feb_flights <-  sfo_feb_flights %>%
                            group_by(carrier) %>%
                            summarise(median_dd = median(dep_delay), iqr_dd = IQR(dep_delay) ,mean_dd = mean(dep_delay), n_flights = n())
Summary_sfo_feb_flights <- merge(x = carrierinfo, y =Summary_sfo_feb_flights , by = "carrier" , all.y = TRUE )
Summary_sfo_feb_flights 

print("sort the data in decreasing order based on subjects ")
print(Summary_sfo_feb_flights[order(Summary_sfo_feb_flights$carrier_name, decreasing = FALSE), ]   )

Summary_sfo_feb_flights[order(Summary_sfo_feb_flights$carrier_name, decreasing = FALSE), ]

Summary_sfo_feb_flights |>
arrange(desc(carrier_name))

carrier <- c('9E','AA','AS','B6','DL','EV','F9','FL','HA','MQ','OO','UA','US','VX','WN','YV')
#Description of carrier codes
carrier_name <- c('Endeavor Air Inc.','American Airlines Inc.','Alaska Airlines Inc.','JetBlue Airways',
                  'Delta Air Lines Inc.','ExpressJet Airlines Inc.','Frontier Airlines Inc.','AirTran Airways Corporation',
                  'Hawaiian Airlines Inc.','Envoy Air','SkyWest Airlines Inc.','United Air Lines Inc.',
                  'US Airways Inc.','Virgin America','Southwest Airlines Co.','Mesa Airlines Inc.')
#create a new df with previous information
carrierinfo <- data_frame(carrier,carrier_name)
#create a summary with median and Interquartile
Summary_sfo_feb_flights <-  sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_arr_delay = median(arr_delay),
            iqr_arr_delay = IQR(arr_delay), n_flights = n(), var_arr_delay =var(arr_delay, na.rm=TRUE))
Summary_sfo_feb_flights <- merge(x = carrierinfo, y =Summary_sfo_feb_flights , by = "carrier" , all.y = TRUE)
Summary_sfo_feb_flights |>
  mutate(cv = ((sd_arr_delay/mean_arr_delay)*100))

Summary_sfo_feb_flights

str(nycflights)
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram()
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 150)

Summarynycflights <- nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay),mean_dd = mean(dep_delay)) 

ggplot(data = Summarynycflights, aes(x = month)) +
  geom_histogram()

#Describe the distribution of the **arrival** delays of these flights using a 
#histogram and appropriate summary statistics. **Hint:** The summary 
#statistics you use should depend on the shape of the distribution.


nycflights %>%
  summarise(mean_arr = mean(arr_delay), 
            median_arr = median(arr_delay), 
            numb_row = n())

ggplot(data = nycflights, aes(x = arr_delay)) +
  geom_histogram( binwidth = 10)

?rnorm
?IQR

remotes::install_github('jbryer/brickset')
#library(brickset)
data('legosets', package = 'brickset')
names(legosets)

#GGPLOT2
library(ggplot2)
str(mtcars)
cars_datacamp <- mtcars
ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()
# factor got added because it''s categorical
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point()


#DATA VISUALIZATION
ls('package:ggplot2')[grep('^geom_',ls('package:ggplot2'))] # all available plots
#https://learninginnovation.duke.edu/wp-content/uploads/2020/07/R_ggplot2_cheatsheet.pdf  


remotes::install_github('jbryer/brickset')
#library(brickset) colorbrewer2.org
data('legosets', package = 'brickset')
names(legosets)
legosets
#multicolor per catagory
ggplot(legosets, aes( x = pieces, y = US_retailPrice, color=availability)) +
  geom_point() +
  scale_color_brewer(type = 'qual', palette =3 )

#transparentcy
ggplot(legosets, aes( x = pieces, y = US_retailPrice, color=availability)) +
  geom_point( alpha = 0.1) 


#Minifiguras for all the categories
ggplot(legosets, aes( x = pieces, y = US_retailPrice, size= minifigs)) +
  geom_point() + facet_wrap(~ availability)


#BOXPLOT
ggplot(legosets, aes( x = 'Lego', y = US_retailPrice)) +
  geom_boxplot()

boxplot(legosets$US_retailPrice) # same as previus

# now if we use a categorical for X 
ggplot(legosets, aes( x = availability, y = US_retailPrice)) + 
  geom_boxplot()


# now if we use a categorical for X 
ggplot(legosets, aes( x = availability, y = US_retailPrice)) +
  geom_boxplot()+
  coord_flip()

tab <- psych::describeBy(legosets$US_retailPrice,
                         group = legosets$availability,
                         mat = TRUE)

ggplot(legosets, aes( x = availability, y = US_retailPrice)) +
 geom_boxplot()+
 geom_errorbar(data = tab, aes(x = group1, y = mean, ymin = mean - se , ymax = mean + se), color = 'green')+  
 geom_point(data = tab, aes(x = group1, y = mean), color = 'blue', size =5)+
 coord_flip()

# histogram
ggplot(legosets, aes( x = US_retailPrice)) +
  geom_histogram(binwidth = 25)


nycflight_summary <- nycflights %>%
  summarise(Mean_arr_des = mean(arr_delay, na.rm = TRUE), 
            Median_arr_des = median(arr_delay, na.rm = TRUE),
            Max_arr_des = max(arr_delay),
            Std_desviation_arr_des = sd(arr_delay, na.rm = TRUE),
            IRQ_arr_des = IQR(arr_delay),
            n = n()
  )
ggplot(data = nycflights, aes(x = arr_delay))+
  geom_histogram( binwidth = 90, color ='blue', fill = "lightblue")+
  geom_vline( data = nycflight_summary, aes(xintercept  = Median_arr_des) ,color ="red" , linetype = "dashed", size=1 ,show.legend = TRUE)+
  geom_vline( data = nycflight_summary, aes(xintercept  = Mean_arr_des) ,color ="darkgreen" , linetype = "longdash" )+
  geom_density()+
  xlab('Arrival Delay Flights')+
  ylab('Number Of Flights')+
  ggtitle("Arrival Delays for NYC Flights")
  #theme(panel.background = element_rect(fill = "white"))+
  #lines(density(nycflights$arr_delay,bw=1), col='red', lwd=3)

ggplot(data = nycflights, aes(x = arr_delay))+
  geom_density(col='red',lwd=1)+
  geom_histogram( data = nycflights,binwidth = 90, color ='blue', fill = "lightblue")+
  geom_vline( data = nycflight_summary, aes(xintercept  = Median_arr_des) ,color ="red" , linetype = "dashed", size=1 ,show.legend = TRUE)+
  geom_vline( data = nycflight_summary, aes(xintercept  = Mean_arr_des) ,color ="darkgreen" , linetype = "longdash" )
  
  
      
?theme
nycflight_summary

psych::describeBy(nycflights$arr_delay)

N <- 10000
x <- rnbinom(N, 10, .5)
x<- nycflights$arr_delay
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
     col='lightblue', xlab=' ', ylab=' ', axes=F,
     main='Positive Skewed')
lines(density(x,bw=1), col='red', lwd=3)


#Density plot

ggplot(legosets, aes( x = US_retailPrice, color = availability)) +
  geom_density() +   scale_x_log10()
##############################
#Mean Pro: This represents the overall average departure delay,
#taking into account the effect of each delay and giving an idea as to how the data is distributed. Con: This can be skewed by outliers.

#Median Pro: It takes the middle value of the entire data set, so outliers do not skew the median. Con: It fails to represent how the data is distributed.

#########################
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 0, "on time", "delayed"))

nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))

nycflights %>%
  mutate(dep_type = ifelse(dep_delay <= 0, "on time", "delayed"))

nycflights_dep_delay <- nycflights |>
  group_by(month) |>
  summarise(dep_delay_Na = sum(ifelse(dep_delay <= 0, 1, 0)), # getting flights with no delays
            num_flight = n ()) 
nycflights_dep_delay %>%
  mutate(average_no_delay = (dep_delay_Na / num_flight) )%>% 
  arrange(desc(average_no_delay))

nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))

ggplot(data = nycflights, aes(x = origin, fill = dep_type)) +
  geom_bar()

nycflights

#Mutate the data frame so that it includes a new variable that contains the 
#average speed, `avg_speed` traveled by the plane for each flight (in mph).
#**Hint:** Average speed can be calculated as distance divided by
#number of hours of travel, and note that `air_time` is given in minutes.

nycflights <- nycflights %>%
  mutate(avg_speed = distance / (air_time*60))

#8.  Make a scatterplot of `avg_speed` vs. `distance`. Describe the relationship
#between average speed and distance.
#**Hint:** Use `geom_point()`.
#*
ggplot(data = nycflights, aes(x = origin, y = avg_speed)) +
  geom_point( color = "blue")+
  geom_point( data = nycflights,aes(x = origin, y = distance), color = "red")


ggplot(data = nycflights, aes(x = distance, y =avg_speed ), color = disp) +
  geom_point( )

ggplot(data = nycflights, aes(x = avg_speed, y =distance ), color = disp) +
  geom_point( )

# determine (roughly) what the cutoff point is for departure
#delays where you can still expect to get to your destination on time

dl_aa_ua <- nycflights %>%
  filter(carrier == "AA" | carrier == "DL" | carrier == "UA")
summary(dl_aa_ua$dep_delay)
ggplot(data = dl_aa_ua, aes(x = dep_delay, y = arr_delay, color = carrier)) +
  geom_point()

#Based off the scatter plot below, the cutoff point for departure delays you can still REASONABLY expect
#to arrive at your destination on time is slightly ahead of schedule,
#roughly first five minutes before departure time. We see that with a 60 minute late departure time, it has still been possible to arrive at the destination on time, but this is extremely rare. It is also uncommon for flights departing between 0 and 60 minutes late. Even the majority of flights departing on time arrive late for these three carriers. Only by leaving early can you expect to arrive on time, and a reasonable cutoff for this seems to be around five minutes before scheduled takeoff.