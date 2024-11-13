#250 millions american,88% support and 12 dont support pannel solar, 
#we want to validate if 88% supports (p=0.88)
p=0.88
main_source <- tibble( support_pannel_sollar = c(rep("Yes", 220000000), rep("No", 30000000)))

#getting sample of 1000
samp1 <- main_source |>
  sample_n(1000)
samp1

#compute to get the fraction of sample that says "support"
samp1 %>%
  count(support_pannel_sollar) %>%
  mutate(p_hat = n /sum(n))

#after running the simulation we get the sample point of estimate of p1=0.893 (p_hat)
p_hat=0.893

#calculate error:
sample_error <- p_hat - p
sample_error


## Simulation sample data ##
#When we simulate the sample data we create the sampling distribution
#we create sample with 1000 observations, in total 10000 sample
samp1_rep_10k <- main_source %>%
  rep_sample_n(size = 1000, reps = 10000, replace = TRUE) %>%
  count(support_pannel_sollar) %>%
  mutate(p_hat = n /sum(n))%>%
  filter(support_pannel_sollar == "Yes")

#Shape is nearly normal, symetric
ggplot(data = samp1_rep_10k, aes(x = p_hat)) +
  geom_histogram( binwidth = 0.001 )+
  labs(
    x = "p_hat (support pannel solar)",
    title = "Sampling Distribution of p_hat",
    subtitle = paste("(10000 samples , n = 1000, mean = ",mean(samp1_rep_10k$p_hat),
                     ' mediam = ',median(samp1_rep_10k$p_hat),
                     ' sd = ',sd(samp1_rep_10k$p_hat),")")
  )


hist(samp1_rep_10k$p_hat, 
     main = "Sampling distribution of p_hat (support pannel solar)",
     sub=paste("(10000 samples , n = 1000, mean = ",
               round(mean(samp1_rep_10k$p_hat),3),
               ", median = ",median(samp1_rep_10k$p_hat),
               ", SD = ",round(sd(samp1_rep_10k$p_hat),3),
               ")"), xlab = "Sample p_hat (support pannel solar)", ylab = "", col = "steelblue")


###Normal distribution
#dnorm gives the density, 
#pnorm gives the distribution function, 
#qnorm gives the quantile function, and 
#rnorm generates random deviates


#standard_error <- function(x) {sd(x) / sqrt(length(x))}

standard_error <- function(p,n) {sqrt(p*(1-p)/n)}




means <- mean(samp1_rep_10k$p_hat)
std <-sd(samp1_rep_10k$p_hat)
se <- round(standard_error(0.88,1000),2)


ggplot(data = samp1_rep_10k, aes(x = p_hat)) +
  geom_blank() +
  geom_histogram(aes(y = after_stat(density)),binwidth = 0.001) +
  stat_function(fun = dnorm, args = c(mean = means, sd = std), col = "steelblue",size=1.5)


# Plot the histogram of sample means
hist(samp1_rep_10k$p_hat, breaks = 30, prob = TRUE, main = "Distribution of Sample Means",
     xlab = "Sample Mean", col = "white")
# Overlay density curves
curve(dnorm(x, mean = means, sd = std), col = "steelblue", lwd = 2, add = TRUE)
# Add labels and legends
legend("topright", legend = c("Distribution Curve"),col = c("steelblue"), lwd = 2)

#p_hat in 0.02 of p(true population) considering mean and std (or standard error) in this case standard error
std <-se
tail_left <- pnorm(0.86, mean = means, sd = std)
tail_right <- 1 - pnorm(0.90, mean = means, sd = std)
center <- pnorm(0.90, mean = means, sd = std) - pnorm(0.86, mean = means, sd = std)
tail_left 
tail_right
center

#Result , this is about 95.4% of the sampling distribution for p_hat
#with +-0.02 of the population proportion , p=0.88

 



