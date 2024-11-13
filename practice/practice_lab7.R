#Make a side-by-side boxplot of `physical_3plus` and `weight`.
#Is there a relationship between these two variables? What did you expect and why?

library(psych)
yrbss_c <- yrbss|>
  drop_na()

tab <- describeBy(yrbss_c$weight, 
                  group=yrbss_c$physical_3plus, #same value as X aes
                  mat=TRUE, skew=FALSE)


ggplot(yrbss_c, aes(x = factor(physical_3plus), y = weight )) + 
  geom_boxplot() +
  geom_point(data = tab, aes(x=group1, y=mean), 
             color='blue', size=2)+
  labs(
    x = "Physically Active per Day",
    y = "Weight",
    title = "Relationship Between Physically Active and weight"
  )+
  theme_minimal()



ggplot(yrbss_c, aes(x=weight, color = physical_3plus)) + geom_density()


visualize(null_dist) + 
  shade_p_value(obs_stat = obs_diff, direction = "two_sided")

?shade_p_value

#task7
#There is a probability that difference between the weights of those who exercise at least three times a
#week and those who don’t lies on (0.72, 2.33)


t.test(factor(physical_3plus) ~ weight, data = yrbss)

min(22-1,22-1)



yrbss2 <- filter( yrbss, physical_3plus == "no" & weight != 0) 
physical_no <- yrbss2$weight 

physical_yes <- (filter( yrbss, physical_3plus == "yes" & weight != 0))$weight 
glimpse (physical_no)
##  num [1:2656] 55.8 46.7 67.1 69.8 66.7 ...
n_yes <- nrow(physical_yes) 
n_no <- nrow(physical_no)
df <- n_yes - 1
mean_no <- mean(physical_no)
mean_yes <- mean(physical_yes)
sd_no <- sd(physical_no)
sd_yes <- sd(physical_yes)
SE <- sqrt( (sd_yes^2)/n_yes + (sd_no^2)/n_no)
t_value <- qt(0.05/2, df, lower.tail = FALSE)
point_estimate <-  mean_yes - mean_no
lower_CI <- point_estimate - t_value * SE
upper_CI <- point_estimate + t_value * SE
lower_CI 

#Task8 
n_yes <- 8351
#n_no <- nrow(physical_no)
df <- n_yes - 1
mean_h <- mean(yrbss1$height)
#mean_yes <- mean(physical_yes)
sd_h <- sd(yrbss1$height)
#sd_yes <- sd(physical_yes)
SE <- sd_h/sqrt(n_yes)

t_value <- qt(0.05/2, df, lower.tail = FALSE)
point_estimate <-  mean_h
lower_CI <- point_estimate - t_value * SE
upper_CI <- point_estimate + t_value * SE
lower_CI 



#Tare8
mean_height <- mean(yrbss$height, na.rm = TRUE)
sd_height <- sd(yrbss$height, na.rm = TRUE)
sample_height <- yrbss %>% 
  summarise(freq = table(height)) %>%
  summarise(n = sum(freq, na.rm = TRUE))

height_upper <- mean_height + z*(sd_height/sqrt(sample_height))
height_lower <- mean_height - z*(sd_height/sqrt(sample_height))
c(height_lower,height_upper)

#Tarea10
height_exercise <- yrbss %>% 
  filter(physical_3plus == "yes") %>% 
  select(height) %>% 
  na.omit()
height_noexercise <- yrbss %>% 
  filter(physical_3plus == "no") %>% 
  select(height) %>% 
  na.omit()
boxplot(height_exercise$height, height_noexercise$height,
        names = c("exercise", "no_exercise"))

#no exercise
mean_height_no <- mean(height_noexercise$height)
print(mean_height_no)
## [1] 1.665587
sd_height_no<- sd(height_noexercise$height)
print(sd_height_no)
## [1] 0.1028581
max_height_no <- max(height_noexercise$height)
print(max_height_no)
## [1] 2.11
#exercise
mean_height_yes <- mean(height_exercise$height)
print(mean_height_yes)
## [1] 1.703213
sd_height_yes <- sd(height_exercise$height)
print(sd_height_yes)
## [1] 0.1032956
max_height_yes <- max(height_exercise$height)
print(max_height_yes)
## [1] 2.11
meandiff <- mean_height_yes - mean_height_no
stderror <-sqrt(((mean_height_yes^2) / nrow(height_exercise)) + ((mean_height_no^2) / nrow(height_noexercise)))

degfreedom_height <- 4022-1
tvalue_height <- qt(.05/2, degfreedom_height, lower.tail = FALSE)
rightintervalht <- meandiff + tvalue_height * stderror
leftintervalht <- meandiff - tvalue_height * stderror
pvalue_height <- 2*pt(tvalue_height,degfreedom_height, lower.tail = FALSE) #p-value

print(round(leftintervalht,2))
print(round(rightintervalht,2))


#Tarea 12
yrbss1 <- yrbss %>% 
  mutate(sleep_8hrs = ifelse(yrbss$school_night_hours_sleep > 7, "yes", "no"))


yrbss4 <- filter( yrbss1, sleep_8hrs == "no" & weight != 0) 
weight_no <- yrbss4$weight 

weight_yes <- (filter( yrbss1, sleep_8hrs == "yes" & weight != 0))$weight 
#glimpse (weight_no) 

n_yes <- 5695 
n_no <- 2656  
df <- n_yes - 1
mean_no <- mean(weight_no)
mean_yes <- mean(weight_yes)
sd_no <- sd(weight_no)
sd_yes <- sd(weight_yes)
SE <- sqrt( (sd_yes^2)/n_yes + (sd_no^2)/n_no)
t_value <- qt(0.05/2, df, lower.tail = FALSE)
point_estimate <-  mean_yes - mean_no
lower_CI <- point_estimate - t_value * SE
upper_CI <- point_estimate + t_value * SE
lower_CI 
upper_CI