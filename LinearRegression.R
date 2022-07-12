library(tidyverse)
library(caret)

#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

find_RMSE <- function(ds){
  test_index <- createDataPartition(ds$y, times = 1, p=0.5, list = FALSE)
  train_data <- dat %>% slice(-test_index)
  test_data <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_data)
  y_hat <- predict(fit, newdata = test_data)
  sqrt(mean((y_hat-test_data$y)^2))
}
 

find_RMSE_of_n <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n , c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
    find_RMSE(dat)
  
}



rep_find_RMSE <- function(x){
  data.frame(mean_RMSE =  replicate(100, find_RMSE_of_n(x))) %>% summarise(., mean(.$mean_RMSE), sd(.$mean_RMSE))
}




n <- c(100, 500, 1000, 5000, 10000)
print(n)
set.seed(1, sample.kind="Rounding")
rep_find_RMSE(100)


#rlang::last_error()


