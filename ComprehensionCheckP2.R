library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2) # if using R 3.5 or earlier
#set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

cutoff_SL <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), by = 0.1)
accuracy_SL <- map_dbl(cutoff_SL, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_SL)

cutoff_SW <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), by = 0.1)
accuracy_SW <- map_dbl(cutoff_SW, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_SW)

cutoff_PL <- seq(min(iris$Petal.Length), max(iris$Petal.Length), by = 0.1)
accuracy_PL <- map_dbl(cutoff_PL, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_PL)

cutoff_PW <- seq(min(iris$Petal.Width), max(iris$Petal.Width), by = 0.1)
accuracy_PW <- map_dbl(cutoff_PW, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_PW)

y_hat_PL_test <- ifelse(test$Petal.Length > 4.7, "virginica", "versicolor" ) %>% factor(levels = levels(test$Species))
mean(y_hat_PL_test == test$Species)






cutoff_SL <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), by = 0.1)
accuracy_SL <- map_dbl(cutoff_SL, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor" ) %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy_SL)

cutoff_SW <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), by = 0.1)
accuracy_SW <- map_dbl(cutoff_SW, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor" ) %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy_SW)

cutoff_PL <- seq(min(iris$Petal.Length), max(iris$Petal.Length), by = 0.1)
accuracy_PL <- map_dbl(cutoff_PL, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor" ) %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy_PL)


cutoff_PW <- seq(min(iris$Petal.Width), max(iris$Petal.Width), by = 0.1)
accuracy_PW <- map_dbl(cutoff_PW, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor" ) %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy_PW)

#plot(iris,pch=21,bg=iris$Species)

cutoff_PL <- seq(min(iris$Petal.Length), max(iris$Petal.Length), by = 0.1)
accuracy_PL <- map_dbl(cutoff_PL, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_PL)
cutoff_PL[which.max(accuracy_PL)]

cutoff_PW <- seq(min(iris$Petal.Width), max(iris$Petal.Width), by = 0.1)
accuracy_PW <- map_dbl(cutoff_PW, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_PW)
cutoff_PW[which.max(accuracy_PW)]

y_hat_PL_test <- ifelse(test$Petal.Length > 4.7|test$Petal.Width > 1.5, "virginica", "versicolor" ) %>% factor(levels = levels(test$Species))
mean(y_hat_PL_test == test$Species)

