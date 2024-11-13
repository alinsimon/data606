
library(ggplot2)

# Example data
data <- data.frame(x = c(1, 2, 3, 4, 5),
                   y = c(2, 4, 5, 7, 9))

# Create a scatter plot with a trend line
ggplot(data, aes(x, y)) +
  geom_point() +                        # Add points
  geom_smooth(method = "lm", se = FALSE) # Add trend line using linear regression

correlation <- cor(data$x, data$y)

# Create the plot with correlation coefficient annotation
ggplot(data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(x = 1, y = max(data$y), label = paste("Correlation:", round(cor(data$x, data$y), 2)), hjust = 0, vjust = 1)


hist(airbnb_nyc$price)


library(ggplot2)
ggplot(airbnb_nyc, aes(x=neighbourhood_group_cleansed,y=price)) +
  geom_point()+
  facet_wrap(vars(airbnb_nyc$neighbourhood_group_cleansed), scales='free')+
  labs(
    x = "NYC Neighbourhood",
    y = "Listing Price",
    title = "Relationship Between Listing Price and NYC Neighbourhood"
  )+
  theme(legend.position = "none",
        strip.background = element_rect(colour="#F6BE00",
                                        fill="#0a5796"),
        strip.text.x = element_text(colour = "white", face = "bold"))

means_neighbourhoodprice <- aggregate(price ~  neighbourhood_group_cleansed, airbnb_nyc, mean)


ggplot(airbnb_nyc, aes(x=neighbourhood_group_cleansed,y=price)) +
  geom_boxplot(fill = "white", colour = "#0a5796",
               outlier.colour = "#F6BE00", outlier.shape = 1)+
  coord_flip()+
  labs(
    x = "NYC Neighbourhood",
    y = "Listing Price",
    title = "Relationship Between NYC Neighbourhood and Listing Price"
  )+
  theme_minimal()+
  stat_summary(fun=mean, colour="#F6BE00", geom="point", 
               shape=18, size=3, show.legend=FALSE) +
  geom_text(data = means_neighbourhoodprice, aes(label = round(price,2), y = price+10 ,vjust=-0.7), colour="#0a5796")



Custom_br_labels <- c('0'='Studio',
                      '1'='1 Bedroom',
                      '2'='2 Bedrooms',
                      '3'='3 Bedrooms',
                      '4'='4 Bedrooms',
                      '5'='5 Bedrooms',
                      '6'='6 Bedrooms',
                      '7'='7 Bedrooms',
                      '8'='8 Bedrooms',
                      '9'='9 Bedrooms')

ggplot(airbnb_nyc, aes(x=factor(bedrooms),y=price)) +
  geom_point()+
  facet_wrap(vars(bedrooms), labeller = as_labeller(Custom_br_labels), scales='free')+
  labs(
    x = "Number of Bedrooms",
    y = "Listing Price",
    title = "Relationship Between Listing Price and Number of Bedrooms"
  )+
  theme(legend.position = "none",
        strip.background = element_rect(colour="#F6BE00",
                                        fill="#0a5796"),
        strip.text.x = element_text(colour = "white", face = "bold"))



means_bedroomsprice <- aggregate(price ~  bedrooms, airbnb_nyc, mean)

ggplot(airbnb_nyc, aes(x=factor(bedrooms),y=price)) +
  geom_boxplot(fill = "white", colour = "#0a5796",
               outlier.colour = "#F6BE00", outlier.shape = 1)+
  coord_flip()+
  labs(
    x = "Number of Bedrooms",
    y = "Listing Price",
    title = "Relationship Between the Number of Bedrooms and the Listing Price"
  )+
  theme_minimal()+
  stat_summary(fun=mean, colour="#F6BE00", geom="point", 
               shape=18, size=3, show.legend=FALSE) +
  geom_text(data = means_bedroomsprice, aes(label = round(price,2), y = price ,vjust=-0.6), colour="#0a5796")



ggplot(airbnb_nyc, aes(x=price)) + 
  geom_histogram(binwidth=39,fill = "#0a5796", colour = "#F6BE00")+
  labs(
    x = "Price",
    y = "Number of Listings",
    title = paste("Airbnb NYC Prices Histogram")
  )+
  theme_minimal()

#1 by one borough 
neighbourhood_listing <- sort(unique(airbnb_nyc$neighbourhood_group_cleansed), decreasing=FALSE)
neighbourhood_listing[1]
length(neighbourhood_listing)
for(i in 1:length(neighbourhood_listing) ){
  current_neighbourhood <- neighbourhood_listing[i]
  current_neighbourhood_airbnb <- airbnb_nyc|>
                                  filter(neighbourhood_group_cleansed == current_neighbourhood)
  
  ggplot(current_neighbourhood_airbnb, aes(x=price)) + 
    geom_histogram(binwidth=39,fill = "#0a5796", colour = "#F6BE00")+
    labs(
      x = "Price",
      y = "Number of Listings",
      title = paste("Airbnb NYC (",current_neighbourhood,") Prices Histogram")
    )+
    theme_minimal()
  
}


ggplot(airbnb_nyc, aes(x=neighbourhood_group_cleansed,fill=neighbourhood_group_cleansed)) + 
  geom_bar(alpha=0.9, colour="#F6BE00")+
  labs(
    x = "Neighbourhood",
    y = "Count",
    title = paste("Airbnb NYC Listing Location")
  )+
  theme_minimal()+
  scale_fill_manual('Neighbourhood', values=c('#094e87','#073c69','#06345a','#084578','#0a5796'))




Custom_br_labels <- c('0'='Studio',
                      '1'='1 Bedroom',
                      '2'='2 Bedrooms',
                      '3'='3 Bedrooms',
                      '4'='4 Bedrooms',
                      '5'='5 Bedrooms',
                      '6'='6 Bedrooms',
                      '7'='7 Bedrooms',
                      '8'='8 Bedrooms',
                      '9'='9 Bedrooms')

ggplot(airbnb_nyc, aes(x=factor(bedrooms))) + 
  geom_bar(fill = "#0a5796", colour = "#F6BE00")+
  labs(
    x = "Bedrooms",
    y = "Count",
    title = paste("Airbnb NYC Listing - Bedrooms")
  )+
  theme_minimal()

#labeller = as_labeller(Custom_br_labels)


