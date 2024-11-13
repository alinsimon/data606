
f <- function(x, mean = 0, sigma = 1) {
  1 / (sigma * sqrt(2 * pi)) * exp(1)^(-1/2 * ( (x - mean) / sigma )^2)
}
  min <- 0; max <- 2
  ggplot() + stat_function(fun = f) + xlim(c(-4, 4)) + 
    geom_vline(xintercept = c(min, max), color = 'blue', linetype = 2) + xlab('x')
  
  
  
  ggplot(data = sample_props50, aes(x = p_hat)) +
    geom_histogram(binwidth = 0.02) +
    labs(
      x = "p_hat (Doesn't benefit)",
      title = "Sampling distribution of p_hat",
      subtitle = "(50 samples , n = 15000)"
    )

#Tarea 4  
  f <- function(x, mean = 0, sigma = 1) {
    1 / (sigma * sqrt(2 * pi)) * exp(1)^(-1/2 * ( (x - mean) / sigma )^2)
  }
  
min_phat <- min(sample_props50$p_hat, na.rm = TRUE)
max_phat <- max(sample_props50$p_hat, na.rm = TRUE)
min_phat 
max_phat

ggplot() + stat_function(fun = f) + xlim(c(-4, 4)) + 
  geom_vline(xintercept = c(min_phat, max_phat), color = 'blue', linetype = 2) + xlab('x')

#Tarea 5

sample_props_small <- global_monitor %>%
  rep_sample_n(size = 10, reps = 25, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")

n_observations <- nrow(sample_props_small)
n_observations

#Tarea 5
library(shiny)


#Tarea 7

samp5 <- global_monitor %>%
  sample_n(15)

ggplot(samp5, aes(x = scientist_work)) +
  geom_bar() +
  labs(
    x = "", y = "",
    title = "Sample Statistics - Do you believe that the work scientists\ndo benefit people like you?"
  ) +
  coord_flip() 


samp5 %>%
  count(scientist_work) %>%
  mutate(sample_statistic = n /sum(n))

#Tarea 8
sample_props15 <- global_monitor %>%
  rep_sample_n(size = 15, reps = 2000, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Benefits")

hist(sample_props15$p_hat, 
main = "Sampling distribution of p_hat",
sub=paste("(2000 samples , n = 15, mean = ",
mean(sample_props15$p_hat),
"), SE=(",standard_error(sample_props15$p_hat),
")"), xlab = "Sample p_hat (Benefits)", ylab = "", col = "steelblue")


?get_ci



