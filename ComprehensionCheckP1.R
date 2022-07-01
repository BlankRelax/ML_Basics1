library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

inclass_female <- dat %>% filter(dat$sex == "Female" & dat$type == "inclass")
inclass <- dat %>% filter(dat$type == "inclass")
online_female <- dat %>% filter(dat$sex == "Female" & dat$type == "online")
online <- dat %>% filter(dat$type == "online")

factor(dat$type)
#inclass are female
#online are male

y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>%  factor(levels = c("Female", "Male"))
accuracy = mean(y_hat == dat$sex)
accuracy
class(dat$sex)
class(y_hat)

table(y_hat, y)
confusionMatrix(y_hat, y)