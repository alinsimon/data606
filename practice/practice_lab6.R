
data('yrbss', package='openintro')
no_helmet <- yrbss %>%
  filter(helmet_12m == "never")

# Tarea1

table(yrbss$text_while_driving_30d)

summaryt1 <- yrbss %>%
  count(text_while_driving_30d) %>%
  mutate(p = n /sum(n))
summaryt1

sortindex <- c(1,2,5,6,3,7,4,8,9)
summarytf <- tibble(summaryt1,sortindex)
summarytf <- summarytf |>
  arrange(desc(sortindex))
#summarytf

ggplot(yrbss,aes(x=text_while_driving_30d, fill =text_while_driving_30d ))+
  geom_bar()+
  scale_x_discrete(limits = rev(summarytf$text_while_driving_30d))+
  ggtitle('Days students have texted while driving within the past 30 days') +
  ylab('Count')+
  xlab('Text While Driving 30 Days')+
  theme_minimal()+
  scale_fill_manual('Categories', values=c('#160042','#423065','#574776','#6C5E87','#817598','#968CA9','#ABA3Ba','#968CA9','#ABA3Ba'))



# Tarea2 What is the proportion of people who have texted while driving every day in the past 30 days and never wear helmets

q2 <- table(yrbss$text_while_driving_30d,yrbss$helmet_12m)|>
  prop.table()
#Proportion per Category of people who have texted while driving every day in the past 30 days
#from the total of observations 
q2[,4]

#Proportion of people who have texted while driving every day in the past 30 days and never wear helmets 
#from the total of observations 
sum(q2[2:7,4])

yrbss|>
  filter(yrbss$helmet_12m == "never")|>
  count(text_while_driving_30d) |>
  mutate(p_hat = n /sum(n))

#Tarea3
# from the previous calculation
lower_ci <- 0.06458558
upper_ci <- 0.07750269

# Knowing that lower_ci = p - moe & upper_ci = p + moe

moe <- (upper_ci - lower_ci) / 2  # margin of error

moe

#


data('yrbss', package='openintro')
no_helmet <- yrbss %>%
  filter(helmet_12m == "never")

no_helmet <- no_helmet %>%
  mutate(text_ind = ifelse(text_while_driving_30d == "30", "yes", "no"))
no_helmet

no_helmet %>%
  drop_na(text_ind) %>% # Drop missing values
  specify(response = text_ind, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci(level = 0.95)

no_helmet_sa1 <- no_helmet |>
  filter(text_ind == "yes") #Last 30 days only
  no_helmet_sa1
n_yrbss <- nrow(yrbss)
p_no_helmet_sa1 <- nrow(no_helmet_sa1) / n_yrbss

p_no_helmet_sa1 # p
n_yrbss # n

1.96*sqrt(p_no_helmet_sa1*(1-p_no_helmet_sa1)/n_yrbss)

#90% - 1.64, 95% - 1.96, 99% - 2.58

#Tarea 4
yrbss$physically_active_7d 
yrbss$gender

yrbss %>%
  count(physically_active_7d) %>%
  mutate(p = n /sum(n))

ggplot(yrbss,aes(x=physically_active_7d, fill =gender ))+
  geom_bar()+
  ggtitle('Physically Active 7 Days by Gender') +
  ylab('Count')+
  xlab('Physically Active 7 Days')+
  theme_minimal()

data('yrbss', package='openintro')
nphysically_active <- yrbss %>%
  filter(physically_active_7d == "0") #no activity


nphysically_active <- nphysically_active %>%
  mutate(text_ind = ifelse(gender == "female", "yes", "no"))


nphysically_active %>%
  drop_na(text_ind) %>% # Drop missing values
  specify(response = text_ind, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci(level = 0.95)


#Tarea5
#It shows that the margin of error is maximized at 0.5 population proportion. 
#The margin of error also vanishes if the population proportion is near to 0 or 1. 
#The margin of error is inversely proportional to the square root of the sample size.


#Tarea 6 The sampling distribution at n=300 and p=0.1 appears to look uni modal and symmetric with the center at p=0.1
set.seed(12)
n <- 300 
p <- 0.1
reps = 1000
samples <- replicate(reps, rbinom(1,n,p))
hist(samples,
     #ylim = c(0, 1.4),
     col = "steelblue",
     freq = F,
     breaks = 25)
curve(dnorm(x, mean=n*p, sd=sqrt(p*(1-p)*n)),
      col = "red",
      lwd = "2",
      add = T)

sample.proportions <- samples / n
hist(sample.proportions,
     #ylim = c(0, 1.4),
     col = "steelblue",
     freq = F,
     breaks = 25)
curve(dnorm(x, mean=p, sd=sqrt(p*(1-p)/n)),
      col = "red",
      lwd = "2",
      add = T)

#task8 smootherAs n increases the distribution of p^ seems to be symmetic and the single peak seems more promiment
#with a greater spread compared to when I decreased n it seems less symmetric and there wasnt that
#much of a spread 

#task9# Insert code for Exercise 9 here
yrbss %>%filter(school_night_hours_sleep == '10+' & strength_training_7d > 6) %>%nrow()/nrow(yrbss)
## [1] 0.1389973
#HO: Those who sleep 10+ hours per day are more likely to strength train everyday of the week. 
#HA: Those who sleep 10+ hours per day are not more likely to strength train everyday of the week.
#Null hypothesis: There is no convincing evidence that hose who sleep 10+ hours every day are more likely to strength train every day of the week Alt hypothesis. 
#There is convincing evidence that hose who sleep 10+ hours every day are more likely to strength train everyday
#Based on the p-value of 0.1389973 we would not reject the null hypothesis as it is greater than 0.05. 
#Therefore, we can conclude that people who sleep 10+ hours per day are not more likely to strength 
#train everyday of the week.


#Chi-Square test would be a good option for testing relationship.

#H0: There is no relation between to sleep 10+ hours per day and to strength train every day of the week

#H1: There is a relation between to sleep 10+ hours per day and to strength train every day of the week

# New data frame with the needed conditions

sleep_more <- yrbss %>%
  mutate(sleep_ind = ifelse(school_night_hours_sleep == "10+", "yes", "no"),
         train_ind = ifelse(strength_training_7d == "7", "yes", "no") )
# Calculate the proportion for those sleeping 10+ hours

sleep_more %>%
  filter(sleep_ind == "yes" | sleep_ind == "no") %>%
  count(sleep_ind) %>%
  mutate(p_sleep = n / sum(n))
## # A tibble: 2 x 3
##   sleep_ind     n p_sleep
##   <chr>     <int>   <dbl>
## 1 no        12019  0.974 
## 2 yes         316  0.0256
# Calculate the proportion for those who strength to train 7d

sleep_more %>%
  filter(train_ind == "yes" | train_ind == "no") %>%
  count(train_ind) %>%
  mutate(p_train = n / sum(n))
## # A tibble: 2 x 3
##   train_ind     n p_train
##   <chr>     <int>   <dbl>
## 1 no        10322   0.832
## 2 yes        2085   0.168

# calculate the difference in proportions for the two categories

pd <- 0.0256 - 0.1681

pd
## [1] -0.1425
#The difference is significant, let do independence test with infer

sleep_more %>%
  specify(sleep_ind ~ train_ind , success = "yes") %>%
  hypothesize( null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate ("diff in props") %>%
  get_ci(level = 0.95)


school_night_hours_sleep_active %>%
  drop_na(text_ind) %>% # Drop missing values
  specify(response = text_ind, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci(level = 0.95)


## Warning: Removed 1364 rows containing missing values.
## Warning: The statistic is based on a difference or ratio; by default, for
## difference-based statistics, the explanatory variable is subtracted in the
## order "no" - "yes", or divided in the order "no" / "yes" for ratio-based
## statistics. To specify this order yourself, supply `order = c("no", "yes")` to
## the calculate() function.
## # A tibble: 1 x 2
##   lower_ci upper_ci
##      <dbl>    <dbl>
## 1 -0.00756  0.00773
#I would say that there is not convincing evidence that those who sleep 10+ hours per day are more likely to strength train every day of the week



#task10
#A type I error is a false positive where we reject a true null hypothesis. 5% for alpha=0.05.
#The definition of a type 1 error is when you reject the null hypothesis even though you didn’t need to.

#task11
P <-0.5
Z.alpha <-1.96
ME <-0.01
N<- Z.alpha^2*P*(1-P)/ME^2
N
## [1] 9604 A sample size of at least 9604.

# Refer to that problem, p = 0.50
# From the formua me = z * sqrt (p * (1 -p) / n)

me11 <- 0.01
z11 <- abs(qnorm(0.025))
p11 <- 0.5

n11 <- (p11 / (me11 / z11))^2

n11
## [1] 9603.647
#I would need 9604 people to sample to ensure that you are within the guidelines

#example difference of two proportions


