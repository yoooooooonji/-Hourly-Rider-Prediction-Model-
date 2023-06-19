# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS") # nolint
ipak(pkg)

##########################################################################################################################################################
# data load
data <- read_excel("/Users/yj.noh/Desktop/infra.xlsx") 
head(data)
str(data)

data$day <- as.factor(data$day)
data$weather <- as.factor(data$weather)

# 배차소요시간
model1 <- lm(배차소요시간 ~ ord_cnt + weather + rider_cnt + day, data = data)
summary(model1)

model2 <- lm(배차소요시간 ~ ord_cnt + rider_cnt + day, data = data)
summary(model2)
coefficients(model2)

y_pred <- predict(model1, data = data)

library(Metrics)
mae(data$배차소요시간, y_pred) #0.54 
rmse(data$배차소요시간, y_pred) #0.67
mape(data$배차소요시간, y_pred) #

# 배달시간
model3 <- lm(배달시간 ~ ord_cnt + weather + rider_cnt + day, data = data)
summary(model3)

model4 <- lm (배달시간 ~ ord_cnt + rider_cnt + day, data = data)
summary(model4)
coefficients(model4)
