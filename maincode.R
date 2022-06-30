library(caret)
library(dslabs)
library(tidyverse)
data(heights)


y <- heights$sex #define predictors
x <- heights$height #define outcomes

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
#times defines how many random samples of indexes to return
# p defines what proportion we want to split indexes of the data from
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

#y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% factor(levels = levels(test_set$sex)) #tests accuracy
#mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>% summarise(mean(height), sd(height)) #this shows that men are on average taller than females

y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex)) #Predict male if height is within two standard deviations from the average male.
mean(y_hat == test_set$sex)

#usually we would not do this as it would overtrain a dataset
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
}) # map function applies every value in cutoff to the function
accuracy[which.max(accuracy)]

#now do this with our test set

y_hat <- ifelse(x > cutoff[which.max(accuracy)], "Male", "Female") %>% factor(levels = levels(test_set$sex)) #Predict male if height is within two standard deviations from the average male.
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
}) # map function applies every value in cutoff to the function
F_1[which.max(F_1)]
cutoff[which.max(F_1)]
