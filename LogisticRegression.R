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

mu_1_seq <- seq(0, 3, len=25)
res <- NULL
for (i in mu_1_seq) {
  set.seed(1)
dat <- make_data(mu_1 = i)
glm_fit <- glm(y ~ x ,data = dat$train, family = "binomial")
y_hat <- predict(glm_fit, newdata = dat$test, type = "response")
y_hat_logit <- ifelse(y_hat > 0.5, 1, 0) %>% factor
acc <- confusionMatrix(y_hat_logit, dat$test$y)$overall[["Accuracy"]]
res <- append(res, acc)

}
print(mu_1_seq)
print(res)
plot(mu_1_seq, res)










