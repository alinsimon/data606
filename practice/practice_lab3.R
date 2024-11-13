
library(tidyverse)
library(openintro)

glimpse(kobe_basket)
kobe_basket
#H : hit
#M : Miss
kobe_basket |>
  filter(game == 1, quarter == 1)

kobe_streak <- calc_streak(kobe_basket$shot)

# get points
kobe_basket_test <- kobe_basket |>
  mutate(shot_reference = ifelse(shot == "H", 1, -1))

streak_id_counter <-1
kobe_basket_test|>
  mutate(streak_id = ifelse(kobe_basket_test$shot_reference[row_number()]==-1,
                            kobe_basket_test$streak_id[row_number() -1 ] + 1,
                            1))


kobe_basket_test
ggplot(data = kobe_streak, aes(x = length)) +
  geom_bar()

kobe_streak |>
  filter(length >= 1)

kobe_basket

#Exerci2
summarykobe_streak <- kobe_streak |>
  group_by(length) |>
  summarise( count_streak = n() )
summarykobe_streak

#cutoffS <- data.frame(yintercept=39, Lines='Cutoff Streaks')

ggplot(data = summarykobe_streak, aes(x = length, y = count_streak)) +
  geom_line(color="darkgreen")+
  geom_point(size =2)+
  #geom_hline(aes(yintercept=yintercept, linetype=Lines), cutoffS, color = "red")+
  geom_hline(yintercept=39, linetype="dashed", color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  geom_hline(yintercept=1, linetype="dashed", color = "blue")+
  geom_vline(xintercept=4, linetype="dashed", color = "blue")+
  xlab('Length')+
  ylab('Number Of Streaks')+
  ggtitle("Streaks and Lengths - KobeBasket") +
  theme_minimal()

# 2 possible outcomes H or M
#Probability to Hit Shot, favourable outcomes vs total number of equally likely outcomes
 kobe_basket |>
  summarise( probability_H_shot = sum(ifelse(shot == "H", 1, 0)) / n())
 
#Probability to Miss Shot 
 kobe_basket |>
   summarise( probability_M_shot = sum(ifelse(shot == "M", 1, 0)) / n())


ggplot(data = summarykobe_streak, aes(x = length, y = count_h_shots )) +
  geom_line()+
  stat_summary(colour = "blue", size = 2, geom = "point")+
  theme(legend.position = "top")+
  xlab('Length')+
  ylab('Number Of Hits')

#ShotReference where Hit will be replace by 1 and M will be -1
kobe_basket <- kobe_basket |>
  mutate(shot_reference = ifelse(shot == "H", 1, -1))

plot(1:length(kobe_basket$shot_reference), cumsum(kobe_basket$shot_reference), type='l')
abline(h=0)
abline(v=133)

#cumsum, calculate cumulative sum
cumsum(c(1,-1,-1,1,1))

cards <- data.frame(Event = c('Heart (not ace)','Ace','King of Spades','All else'),
	X = c(1, 5, 10, 0),	pX = c(12/52, 5/52, 1/52, 32/52) )
cards$XpX <- cards$X * cards$pX
cards2 <- rep(0, 11)
cards2[cards$X + 1] <- cards$pX
names(cards2) <- 0:10
barplot(cards2, main='Probability of Winning Game')
cards2

summarykobe_streak <- kobe_streak |>
  group_by(length) |>
  summarise( count_streak = n() )
summarykobe_streak

cutoffS <- data.frame(yintercept=39, Lines='CutOff Streaks')

ggplot(data = summarykobe_streak, aes(x = length, y = count_streak)) +
  geom_line(color="darkgreen")+
  geom_point(size =2)+
  geom_hline(aes(yintercept=yintercept, linetype=Lines), cutoffS, color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  geom_hline(yintercept=1, linetype="dashed", color = "blue")+
  geom_vline(xintercept=4, linetype="dashed", color = "blue")+
  xlab('Length')+
  ylab('Number Of Streaks')+
  ggtitle("Streaks and Lengths - KobeBasket") +
  theme_minimal()

############ similations#########
coin_outcomes <- c("heads", "tails")
sim_fair_coin  <- sample(coin_outcomes, size = 100, replace = TRUE)
sim_fair_coin

table(sim_fair_coin)

sim_unfair_coin <- sample(coin_outcomes, size = 100, replace = TRUE, 
                          prob = c(0.2, 0.8))

table(sim_unfair_coin)

# Example legend
# Specify colour and shape
lp1 <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) + geom_line() + geom_point()
lp1

# Here's what happens if you just specify colour
lp1 + scale_colour_discrete(name  ="Payer",
                            breaks=c("Female", "Male"),
                            labels=c("Woman", "Man"))

# Specify both colour and shape
lp1 + scale_colour_discrete(name  ="Payer",
                            breaks=c("Female", "Male"),
                            labels=c("Woman", "Man")) +
  scale_shape_discrete(name  ="Payer",
                       breaks=c("Female", "Male"),
                       labels=c("Woman", "Man"))

