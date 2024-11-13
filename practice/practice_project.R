# Let's assume your dataframe is called 'df'
# and your dependent variable is 'y'


# Select independent variables
independent_vars <- subset(airbnb_nyc, select = -price)  # Select all columns except 'y'

# Alternatively, you can manually specify the independent variables
# independent_vars <- df[, c("x1", "x2", "x3")]

# Check the structure of independent_vars
str(independent_vars)



ggplot(airbnb_nyc , aes( x = beds,  y =price))+
  geom_point(alpha=0.2)+ 
  geom_smooth(method='lm', formula=y ~ x, size=1, se=FALSE, color ="#ff385c") +
  labs(
    x = "beds(x2)",
    y = "price(y)",
    title = "Scatterplot of price(y) vs beds(x2)"
  )+
  theme_minimal()

cor(airbnb_nyc$beds,airbnb_nyc$price, use='complete.obs')

#Correnlation between bedrooms and beds
airbnb_nyc_price_beds.lm <- lm(price ~ accommodates +property_type +neighbourhood_cleansed , data = airbnb_nyc)
#Summarize the model residuals
summary(airbnb_nyc_price_beds.lm)

# declare model here
airbnb_nyc_test.lm <- lm(price ~ accommodates +neighbourhood_cleansed , data = airbnb_nyc)
#Summarize the model residuals
summary(airbnb_nyc_test.lm)

plot1 <- ggplot(airbnb_nyc, aes(x=neighbourhood_cleansed_id, y=price,
                                color=neighbourhood_cleansed_id)) +
  geom_point(aes(color=neighbourhood_cleansed)) +
  geom_smooth(method = "lm") +
  ggtitle("price ~ accommodates") +
  theme_bw()
#?neighbourhood_cleansed_id ?neighbourhood_cleansed
airbnb_nyc_test.lm <- lm(price ~ accommodates + neighbourhood_cleansed , data = airbnb_nyc)
plot2 <-ggplot(airbnb_nyc, aes(x=accommodates, y=price, color=neighbourhood_cleansed)) +
  geom_point(aes(color=neighbourhood_cleansed)) +
  geom_smooth(method = "lm", mapping=aes(y=predict(airbnb_nyc_test.lm,airbnb_nyc))) +
  ggtitle("price ~ accommodates + neighbourhood_cleansed") +
  theme_bw()

#?neighbourhood_group_cleansed_id ?neighbourhood_group_cleansed
# declare model here
airbnb_nyc_test.lm <- lm(price ~ accommodates + neighbourhood_group_cleansed , data = airbnb_nyc)
plot3 <-ggplot(airbnb_nyc, aes(x=accommodates, y=price, color=neighbourhood_group_cleansed)) +
  geom_point(aes(color=neighbourhood_group_cleansed)) +
  geom_smooth(method = "lm", mapping=aes(y=predict(airbnb_nyc_test.lm,airbnb_nyc))) +
  ggtitle("price ~ accommodates + neighbourhood_group_cleansed") +
  theme_bw()

#Better
airbnb_nyc_test.lm <- lm(price ~ accommodates +property_type + neighbourhood_cleansed , data = airbnb_nyc)
summary(airbnb_nyc_test.lm)
 boxplot(airbnb_nyc_test.lm[['residuals']],main='Boxplot: Residuals',ylab='residual value')
#Residuals:Difference between what the model predicted and the actual value of y
summary(airbnb_nyc_test.lm$fitted.values)

glm(price ~ , data = airbnb_nyc)

polychor()
  
mean(c(2,4,6))
  ## bedrooms 9.07%
## accommodates 23%
## beds 14.49%
## property_type 23.8%
## neighbourhood_cleansed 17.8%
## neighbourhood_group_cleansed 7.5%
install.packages("rcompanion")
library(rcompanion)
## cramerV
cramerV(airbnb_nyc$neighbourhood_group_cleansed,airbnb_nyc$neighbourhood_cleansed)

library(psych)

tetrachoric(airbnb_nyc)

install.packages("ltm")
library(ltm)

# relationship between dummy variable and price
biserial.cor(airbnb_nyc_wdv$boroughManhattan,airbnb_nyc_wdv$price, use='complete.obs')

airbnb_nyc$property_type
model <- aov(price ~ neighbourhood_group_cleansed, data = airbnb_nyc)
summary(model)
TukeyHSD(model)

unique(airbnb_nyc$neighbourhood_group_cleansed)

TEST <- airbnb_nyc[,c(2,5,3,7,1) ]

### identify 
model <- lm(price ~ ., data = TEST)
step_model <- step(model)

# View the final selected model
summary(step_model)
