library(caret)

set.seed(1) # if using R 3.5 or earlier
#set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))# our priors
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#disease # probabilty you have the desease(1)=0.02, probability 0 = 0.98
test[disease==0] 


Prob_test_positive <- sum(test[disease==1])/1e6
prob_disease <- sum(disease==1)/1e6
prob_healthy <- sum(disease==0)/1e6
Prob_test_positive

0.85*0.02/(Prob_test_positive)
