library(tidyverse)
library(caret)

set.seed(1) # if using R 3.5 or earlier
#set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
test_index <- createDataPartition(dat$y, times = 1, p=0.5, list = FALSE)

train_data <- dat[-test_index, ]
test_data <- dat[test_index, ]


y_hat <- predict(lm(train_data), test_data)
mean((y_hat - test_data$y)^2)

RMSE(test_data$y, y_hat)