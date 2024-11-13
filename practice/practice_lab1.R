install.packages('tinytex') 
tinytex::install_tinytex()

sample_df <- data.frame(
  group = factor(rep(letters[1:3], each = 5)),
  value = rnorm(30)
)
sample_df

group_means_df <- setNames(
  aggregate(value ~ group, sample_df, mean),
  c("group", "group_mean")
)

sample_df
group_means_df

ggplot(data = sample_df, mapping = aes(x = group, y = value)) +
  geom_point() +
  geom_point(
    mapping = aes(y = group_mean), data = group_means_df,
   colour = 'red', size = 3
  )

arbuthnot

arbuthnotgroup_means_df <- setNames(
  aggregate(girls ~ year, arbuthnot, mean),
  c("year", "year_mean")
)
arbuthnotgroup_means_df

ggplot(data = arbuthnot, mapping = aes(x = year, y = girls)) +
  geom_point() +
  geom_point(
    mapping = aes(y = year_mean), data = arbuthnotgroup_means_df,
    colour = 'red', size = 3
  )

ggplot(data = arbuthnot, mapping = aes(x = year, y = girls)) +
  geom_point() +
  geom_smooth( aes(y = girls) , method = "lm" ,se = FALSE ,colour = 'red') +
  labs(x = "Years", y = "Girls Baptized")
?geom_smooth


summarize_arbuthnot <- arbuthnot %>%
  summarize(min = min(boys), max = max(boys), total_boys=sum(boys), count = n())

summarize_present <- present %>%
  summarize(min = min(boys), max = max(boys), total_boys=sum(boys), count = n())

summarize_present > summarize_arbuthnot



#Calculate total
present <- present %>%
  mutate(total = boys + girls)

#calculate boy_ratio
present <- present %>%
  mutate(boy_ratio = boys / total)

#calculate girls_ratio
present <- present %>%
  mutate(girl_ratio = girls / total)

#plot df
ggplot( data = present , aes(x = year, y = boy_ratio))+  
  geom_point(color = "blue")+
  geom_smooth(method = "lm" ,se = FALSE, color = "darkblue")+
  labs(x = "Year", y = "Proportion of Newborn Boys")

#plot df girls vs boys
ggplot( data = present , aes(x = year))+
  geom_line( aes(y = girls), color = "red")+
  geom_line( aes(y = boys), color = "blue")+
  labs(x = "Year", y = "Proportion of Newborn")

#Export Pdf 
install.packages("tinytex")
library("tinytex")
install.packages('tinytex')
tinytex::install_tinytex()
rmarkdown::render("labs/Lab1_intro_to_r.Rmd", output_format = "pdf_document")
