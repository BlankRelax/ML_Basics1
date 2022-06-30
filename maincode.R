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

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% factor(levels = levels(test_set$sex)) #tests accuracy
mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>% summarise(mean(height), sd(height)) #this shows that men are on average taller than females

y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)