#### Environment ####
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
names(movielens)

options(digits = 7)

#####

#### Ex Recommendation System ####

# 1
movielens %>%
  group_by(movieId) %>%
  mutate(count = n(), year = as.character(first(year))) %>%   
  ggplot(aes(x = year, y = sqrt(count), group = year)) +
  geom_boxplot(outlier.colour = "darkblue", # outlier colour
               notchwidth = 1, # box line width
               colour = "darkred" # box colour
              ) + 
  theme(axis.text.x = element_text(angle = 90, hjust=1))

# alternative (baseR)
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 两个画出来结果不太一样



# 2
t<- movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  mutate(per = n()/(2018-year), avg_rating = sum(rating)/n()) %>%
  dplyr::select(movieId, per, avg_rating, title) %>%
  arrange(desc(per)) %>%
  distinct() %>%
  head(25) 

# 3 
movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  mutate(per = n()/(2018-year), avg_rating = sum(rating)/n()) %>%
  dplyr::select(movieId, per, avg_rating, title) %>%
  arrange(desc(per)) %>%
  distinct() %>%
  qplot(y = avg_rating, x = per, data =.)

# 5
movielens <- mutate(movielens, date = as_datetime(timestamp))

# 6 
movielens %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarise(per = mean(rating)) %>%
  qplot(y = per, x = date, data = .)
# check avarage rating each week and time 
# some evidence for a time effect

# 8 error bar plot with mean and standard error 
movielens %>%
  group_by(genres) %>%
  filter(n() >= 1000) %>%
  mutate(avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  ggplot(aes(x = genres, y = avg, 
             ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar(width=0.4, colour="orange", alpha=0.9) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

#####

#### Ex Regularisation ####

# Background
# many good schools are small schools (observation)
# connection between size and quality?
# we simulate data, where true quality of each school is independent of size

# data simulation 
{
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1)) # number of students in each school

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu)) # true quality of school (mu)

schools %>% top_n(10, quality) %>% arrange(desc(quality))
# we can see the top 10 schools 

# have students in the school take a test 
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

}

# 1. top by mean score 
schools %>%
  arrange(desc(score)) %>%
  head(10)
  
# 2. median school size 
median(schools$size)
schools %>%
  arrange(desc(score)) %>%
  head(10) %>%
  summarise(median(size))

# 3. median school size, worst
schools %>%
  arrange(score) %>%
  head(10) %>%
  summarise(median(size))

# 4.
schools %>%
  ggplot(aes(x = size, y = score)) + 
  geom_point(color = "blue") + 
  geom_point(data = filter(schools, rank <= 10), 
             color = "violet") # highlight top 10 schools (by rank)



# 5 regularising scores 
# (average score is not a good statistic)
overall <- mean(sapply(scores, mean)) # overall avarage of all schools 
alpha <- 25
score_reg <- sapply(scores, function(x){
  overall + sum(x-overall)/(length(x)+alpha)
})
schools %>% mutate(score_reg = score_reg) %>%
  arrange(desc(score_reg)) %>%
  head(10)



# 6 improve regularisation 
alpha <- seq(10, 250, 1)
RMSE <- function(quality, estimate) {
  sqrt(sum((quality-estimate)^2)/length(quality))
}

rmses <- sapply(alpha, function(a){
  score_reg <- sapply(scores, function(x){
    overall + sum(x-overall)/(length(x)+a)
  })
  schools %>%
    mutate(score_reg = score_reg) %>%
    summarise(rmse = RMSE(quality = quality,
                          estimate = score_reg)) %>%
    pull(rmse)
})

alpha[which.min(rmses)] # alpha = 135


# 7 
overall <- mean(sapply(scores, mean)) # overall avarage of all schools 
alpha <- 135
score_reg <- sapply(scores, function(x){
  overall + sum(x-overall)/(length(x)+alpha)
})
schools %>% mutate(score_reg = score_reg) %>%
  arrange(desc(score_reg)) %>%
  head(10)

# 8 a common mistake:
# shrinking values toward zero when they are not centred around 0 
alpha <- seq(10, 250, 1)
RMSE <- function(quality, estimate) {
  sqrt(sum((quality-estimate)^2)/length(quality))
}

rmses <- sapply(alpha, function(a){
  score_reg <- sapply(scores, function(x){
    sum(x)/(length(x)+a)
  })
  schools %>%
    mutate(score_reg = score_reg) %>%
    summarise(rmse = RMSE(quality = quality,
                          estimate = score_reg)) %>%
    pull(rmse)
})

alpha[which.min(rmses)] # alpha = 10
# we see the result is drastically different 

#####

  