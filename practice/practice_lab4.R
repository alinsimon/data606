library(tidyverse)
library(openintro)
data("fastfood", package='openintro')
head(fastfood)
glimpse(fastfood)


mcdonalds <- fastfood %>%
  filter(restaurant == "Mcdonalds")
dairy_queen <- fastfood %>%
  filter(restaurant == "Dairy Queen")

mcdonalds
dairy_queen
# Question 1
ggplot(data = mcdonalds, aes( fill=restaurant, y = sum(cal_fat), x = restaurant))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_bar(data = dairy_queen ,
           aes( fill=restaurant, y = sum(cal_fat), x = restaurant),
           position = 'dodge', stat = 'identity')+
  ggtitle('Amount of calories from fat - MCDonals vs Dairy Queen') +
  ylab('Calories from Fat')+
  xlab('Restaurant')+
  scale_fill_manual('Restaurant', values=c('#DD0004','#FFE14D'))

#mcdonal vs Dairy Queen  
fastfood_comp <- fastfood |>
            filter(restaurant == "Mcdonalds" | restaurant == "Dairy Queen")
            

ggplot(data = fastfood_comp, aes(fill=restaurant,x = cal_fat))+
  geom_histogram(alpha = 0.9)+
  scale_fill_manual('Restaurant', values=c('#DD0004','#FFE14D'))+
  ylab('Count')+
  xlab('Calories from Fat')+
  ggtitle('Amount of calories from fat - MCDonals vs Dairy Queen') +
  theme_minimal()
  
ggplot(data = mcdonalds, aes(fill=restaurant,x = cal_fat))+
  geom_histogram()+
  geom_density()+
  geom_histogram(data = dairy_queen, aes(fill=restaurant,x = cal_fat),alpha = 0.5) +
  ggtitle('Amount of calories from Fat - MCDonals vs Dairy Queen') +
  xlab('Calories from Fat')+
  ylab('Count')+
  scale_fill_manual('Restaurant', values=c('#DD0004','#FFE14D'))+
  theme_minimal()


ggplot(data = mcdonalds, aes(x = cal_fat))+
  stat_function(fun = dnorm, 
                args = c(mean = mean(mcdonalds$cal_fat),
                         sd = sd(mcdonalds$cal_fat)), col = "#DD0004", size=1.1)+
  geom_vline(xintercept=mean(mcdonalds$cal_fat), linetype="dashed", color = "#DD0004", size=1)+
  geom_vline(xintercept=median(mcdonalds$cal_fat),  color = "#DD0004", size=1)+
  stat_function(fun = dnorm, 
                args = c(mean = mean(dairy_queen$cal_fat),
                         sd = sd(dairy_queen$cal_fat)), col = "#FFE14D", size=1.1)+
  geom_vline(xintercept=mean(dairy_queen$cal_fat), linetype="dashed", color = "#FFE14D", size=1)+
  geom_vline(xintercept=median(dairy_queen$cal_fat),  color = "#FFE14D", size=1)+
  ggtitle('MCDonals vs Dairy Queen Distribution') +
  xlab('Calories from Fat')+
  ylab('Count')+
  theme_minimal()


mcdonalds |>
  summarise( mean_cal_fat = mean(mcdonalds$cal_fat),
             median_cal_fat = median(mcdonalds$cal_fat),
             sd_cal_fat = sd(mcdonalds$cal_fat))

dairy_queen |>
  summarise( mean_cal_fat = mean(dairy_queen$cal_fat),
             median_cal_fat = median(dairy_queen$cal_fat),
             sd_cal_fat = sd(dairy_queen$cal_fat))
  
#Q1mcdonal
ggplot(data = mcdonalds, aes(fill=restaurant, x = cal_fat)) +
  geom_blank() +
  geom_histogram(aes(y = after_stat(density)),alpha = 0.8) +
  stat_function(fun = dnorm, 
                args = c(mean = mean(mcdonalds$cal_fat),
                sd = sd(mcdonalds$cal_fat)), col = "#ff8000", size=1.5)+
  geom_vline(xintercept=mean(mcdonalds$cal_fat), linetype="dashed", color = "#ff8000")+
  geom_vline(xintercept=median(mcdonalds$cal_fat),  color = "#ff8000")+
  scale_fill_manual('Restaurant', values=c('#FFE14D'))+
  theme_minimal()+
  ggtitle('Amount of calories from Fat - MCDonals') +
  xlab('Calories from fat')+
  ylab('Density')


#Q2Dairy Queen
ggplot(data = dairy_queen, aes(fill=restaurant, x = cal_fat)) +
  geom_blank() +
  geom_histogram(aes(y = after_stat(density)),alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = c(mean = mean(dairy_queen$cal_fat),
                         sd = sd(dairy_queen$cal_fat)), col = "#4e0000", size=1.5)+
  geom_vline(xintercept=mean(dairy_queen$cal_fat), linetype="dashed", color = "#4e0000")+
  geom_vline(xintercept=median(dairy_queen$cal_fat),  color = "#4e0000")+
  scale_fill_manual('Restaurant', values=c('#DD0004'))+
  theme_minimal()+
  ggtitle('Amount of calories from Fat - Dairy Queen') +
  xlab('Calories from fat')+
  ylab('Density')

?mfv

ggplot(data = dairy_queen, aes(x = cal_fat)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = c(mean = dqmean, sd = dqsd), col = "tomato")


ggplot(data = mcdonalds, aes(fill=restaurant, x = cal_fat)) +
  geom_bar()

qqnorm(sim_norm, cex=0.5,
       axes=F,
       main='Normal Q-Q Plot DairyQueen Calories',
       ylab='Calories from Fat', pch=19)
axis(1)
axis(2)
abline(mean(sim_norm), sd(sim_norm), col="#DD0004", lwd=1.5)



ggplot(data = mcdonalds, aes(sample = sim_normmc)) + 
  geom_line(stat = "qq",color = "red")+
  geom_qq(color = "red")+
  geom_qq_line() +
  theme_minimal()+
  ggtitle('Normal Probaility Plot - Dairy Queen')


bk <- fastfood %>%
  filter(restaurant == "Burger King")
qqnorm(bk$sodium, main = "Burger King")
abline(mean(bk$sodium), sd(bk$sodium), col="#DD0004", lwd=1.5)



restaurant_name <- unique(fastfood$restaurant)
restaurant_name
for(i in 1:length(restaurant_name)){
  temp_name <- restaurant_name[i]
  temp_filter <- fastfood |>
  filter(restaurant == temp_name)
  print(temp_name)
  qqnorm(temp_filter$sodium, main = temp_name)
  
}


Subway <- fastfood %>%
  filter(restaurant == "Subway")
qqnorm(Subway$total_carb, main = "Subway",cex=0.5,
       axes=F)
axis(1)
axis(2)
abline(mean(Subway$total_carb), sd(Subway$total_carb), col="darkgreen")


ggplot(data = Subway, aes(x = total_carb, fill="Subway")) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), col = "darkgreen") +
  stat_function(fun = dnorm,
                args = c(mean = mean(Subway$total_carb),
                sd = sd(Subway$total_carb)),
                col = "darkgreen")+
  theme_minimal()+
  ggtitle('Total Carbohydrates for Subway')+
  scale_fill_manual('Subway', values=c('#ffff9a'))



ggplot(data = Subway, aes(x = total_carb)) +
  geom_density()+
  geom_blank() +
  geom_histogram(aes(y = ..density..), col = "darkgreen") +
  stat_function(fun = dnorm,
                args = c(mean = mean(Subway$total_carb),
                         sd = sd(Subway$total_carb)),
                col = "darkgreen")


ggplot(data = Subway, aes(x = total_carb)) +
  geom_boxplot()


ggplot(data = mcdonalds, aes(x = cal_fat)) +
  geom_boxplot()+
  geom_boxplot(data = mcdonalds, aes(x = cal_fat))

ggplot(data = mcdonalds, aes(x = cal_fat)) +
  geom_histogram()


mcdonalds <- fastfood %>%
  filter(restaurant == "Mcdonalds")
dairy_queen <- fastfood %>%
  filter(restaurant == "Dairy Queen")

mcdonalds_summary <- summary(mcdonalds$cal_fat)
hist(mcdonalds$cal_fat,
     xlab = "Amount of calories from Fat - MCDonals",
     col = "#FFE14D", border = "#ff8000")

dairy_queen_summary <- summary(dairy_queen$cal_fat)
hist(dairy_queen$cal_fat,
     xlab = "Amount of calories from Fat - Dairy Queen",
     col = "#DD0004", border = "#4e0000")


h <- hist(mcdonalds$cal_fat, xlim=c(60, 80))
x <- seq(min(mcdonalds$cal_fat)-5, max(mcdonalds$cal_fat)+5, 0.01)
y <- dnorm(x, mean(mcdonalds$cal_fat), sd(mcdonalds$cal_fat))
y <- y * diff(h$mids[1:2]) * length(mcdonalds$cal_fat)
lines(x, y, lwd=1.5, col='blue')


#Q1

#mcdonal vs Dairy Queen  
fastfood_comp <- fastfood |>
  filter(restaurant == "Mcdonalds" | restaurant == "Dairy Queen")
ggplot(data = fastfood_comp, aes(fill=restaurant,x = cal_fat))+
  geom_histogram(alpha = 0.9)+
  scale_fill_manual('Restaurant', values=c('#DD0004','#FFE14D'))+
  ylab('Count')+
  xlab('Calories from Fat')+
  ggtitle('Amount of calories from fat - MCDonals vs Dairy Queen') +
  theme_minimal()

#mcdonal vs Dairy Queen Distribution
ggplot(data = mcdonalds, aes(x = cal_fat))+
  stat_function(fun = dnorm, 
                args = c(mean = mean(mcdonalds$cal_fat),
                         sd = sd(mcdonalds$cal_fat)), col = "#DD0004", size=1.1)+
  geom_vline(xintercept=mean(mcdonalds$cal_fat), linetype="dashed", color = "#DD0004", size=1)+
  geom_vline(xintercept=median(mcdonalds$cal_fat),  color = "#DD0004", size=1)+
  stat_function(fun = dnorm, 
                args = c(mean = mean(dairy_queen$cal_fat),
                         sd = sd(dairy_queen$cal_fat)), col = "#FFE14D", size=1.1)+
  geom_vline(xintercept=mean(dairy_queen$cal_fat), linetype="dashed", color = "#FFE14D", size=1)+
  geom_vline(xintercept=median(dairy_queen$cal_fat),  color = "#FFE14D", size=1)+
  ggtitle('MCDonals vs Dairy Queen Distribution') +
  xlab('Calories from Fat')+
  ylab('Count')+
  theme_minimal()

#Q1mcdonal
ggplot(data = mcdonalds, aes(fill=restaurant, x = cal_fat)) +
  geom_blank() +
  geom_histogram(aes(y = after_stat(density)),alpha = 0.8) +
  stat_function(fun = dnorm, 
                args = c(mean = mean(mcdonalds$cal_fat),
                         sd = sd(mcdonalds$cal_fat)), col = "#ff8000", size=1)+
  geom_vline(xintercept=mean(mcdonalds$cal_fat), linetype="dashed", color = "#ff8000")+
  geom_vline(xintercept=median(mcdonalds$cal_fat),  color = "#ff8000")+
  scale_fill_manual('Restaurant', values=c('#FFE14D'))+
  theme_minimal()+
  ggtitle('Amount of calories from Fat - MCDonals') +
  xlab('Calories from fat')+
  ylab('Density')


#Q2Dairy Queen
ggplot(data = dairy_queen, aes(fill=restaurant, x = cal_fat)) +
  geom_blank() +
  geom_histogram(aes(y = after_stat(density)),alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = c(mean = mean(dairy_queen$cal_fat),
                         sd = sd(dairy_queen$cal_fat)), col = "#4e0000", size=1)+
  geom_vline(xintercept=mean(dairy_queen$cal_fat), linetype="dashed", color = "#4e0000")+
  geom_vline(xintercept=median(dairy_queen$cal_fat),  color = "#4e0000")+
  scale_fill_manual('Restaurant', values=c('#DD0004'))+
  theme_minimal()+
  ggtitle('Amount of calories from Fat - Dairy Queen') +
  xlab('Calories from fat')+
  ylab('Density')

###################
ggplot(data = mcdonalds, aes(x = cal_fat))+
  stat_function(fun = dnorm, 
                args = c(mean = mean(mcdonalds$cal_fat),
                         sd = sd(mcdonalds$cal_fat)), col = "#FFE14D", size=1.1)+
  geom_vline(xintercept=mean(mcdonalds$cal_fat), linetype="dashed", color = "#FFE14D", size=1)+
  geom_vline(xintercept=median(mcdonalds$cal_fat),  color = "#FFE14D", size=1)+
  stat_function(fun = dnorm, 
                args = c(mean = mean(dairy_queen$cal_fat),
                         sd = sd(dairy_queen$cal_fat)), col = "#DD0004", size=1.1)+
  geom_vline(xintercept=mean(dairy_queen$cal_fat), linetype="dashed", color = "#DD0004", size=1)+
  geom_vline(xintercept=median(dairy_queen$cal_fat),  color = "#DD0004", size=1)+
  ggtitle('MCDonals vs Dairy Queen Distribution') +
  xlab('Calories from Fat')+
  ylab('Count')+
  theme_minimal()