library(tidyverse)
library(caret)

set.seed(2) 
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

make_fit <- function(mu_1){
  dat <- make_data(mu_1)
  fit <- glm(y ~ x, data = dat$train, family = "binomial")
  y_hat <- predict(fit, newdata = dat$test)
  res <-sum(dat$train[ ,2]-y_hat)  
}

make_fit(mu_1 <- seq(0, 3, len=25))

set.seed(1)
mu_1 <- seq(0, 3, len=25)
res <- replicate(25, make_fit(mu_1))         
res

















