#### Environment ####
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

#####

#### ksmooth ####
# Bin smoothing 
span <- 7
# 即用前后多少个点求平均
fit <- with(polls_2008,
            ksmooth(day, margin, 
                    kernel = "box", bandwidth = span))
# box: equal weight for all points 
polls_2008 %>% 
  mutate(smooth = fit$y) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")



# Kernel 
span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin, 
                    kernel = "normal", bandwidth = span))
# we use normal distribution to assign weights 
# (less weights to boundary, more weights to centre)
# Hence the curve is smoother
polls_2008 %>% 
  mutate(smooth = fit$y) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#####

#### Local Weighted Regression (loess) ####

# Theory base: Taylor Expansion 
# Can use poly to approximate curves locally 
# this "local" should be larger than taking curve as constant 
# (while mathematically speaking this need not to be true)



# Local Linear 
total_days <- diff(range(polls_2008$day)) 
span <- 21/total_days
# we think 3 weeks is a good interval for linearlity 
fit <- loess(margin ~ day, degree=1, 
             span = span, data=polls_2008)
# Remark:
# 1. Span asks for a proportion. 
# If N is the number of data points and span = 0.5, for given x
# 0.5N closest points to x would be used for the fit 
# 2. For local fit, a method similar to weighted version of MSE is used 
# But the actual method gives higher weights to closer points 
# 3. family = "symmetric"
# Can fit local model robustly, by detecting and down-weighting outliers
polls_2008 %>% 
  mutate(smooth = fit$fitted) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")



# Local Quatratic 
total_days <- diff(range(polls_2008$day))
span <- 28/total_days
# we take even larger span for quatratic fit

fit_1 <- loess(margin ~ day, degree=1, 
               span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, 
               span = span, data=polls_2008)
# default fit for loess is actually quatratic 
polls_2008 %>% 
  mutate(smooth_1 = fit_1$fitted, 
         smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth_1), color="red", lty = 2) + 
  geom_line(aes(day, smooth_2), color="orange", lty = 1)
# degree = 2 (orange) gives us more wiggly results
# local linear fit is less impacted by noise

#####

#### Caveat of default ggplot smoothing ####
polls_2008 %>% 
  ggplot(aes(day, margin)) + 
  geom_point() +
  geom_smooth()
# these default smoothing parameters are rarely optimal 

polls_2008 %>% 
  ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(method = loess,
              method.args = list(degree = 1, span = 0.15))
# should handset them instead

#####

#### Ex 3 ####
library(caret)
data("mnist_27")

fit <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>% #factor as numeric dummy
  loess(y ~ x_2, degree = 1,
             data = .)
# default span is 0.75

p_hat <- predict(fit, newdata = mnist_27$test) 
# estimate conditional probability in test data 
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
# decision based on Bayes' rule 选概率大的那个情况作为prediction
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

