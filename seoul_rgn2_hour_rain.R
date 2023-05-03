# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg<-c("readr","dplyr","tidytext","tidyverse","lubridate","reshape2","psych","gtsummary", "readxl", "MASS")
ipak(pkg)

#####################################################################################################
# 서울 전체 기상 
getwd()
data <- read.csv("rider_hour_rgn2_seoul_filled_data.csv", fileEncoding = "cp949")

data <- data %>% 
  filter(is_rain == 1)

dim(data) # 16,810

#modeling
var <-  c('pick_rgn2_nm', 'hour_reg','day_of_reg','is_rain','month','week','is_holiday')
data[,var]<- lapply(data[,var], factor)

# correlation
cor(data[c("rider_cnt_2", "order_cnt_w_1", "order_cnt_w_2","order_cnt_w_3","order_cnt_w_4",
           "rider_cnt_w_1", "rider_cnt_w_2", "rider_cnt_w_3", "rider_cnt_w_4", "rain_c", "snow_c")])

# train_test 
train <- data %>% 
  filter(reg_date <= '2022-12-31')

dim(train) # 15,142

test <- data %>% 
  filter(reg_date >= '2023-01-01')

dim(test) # 1,668

train_set <- subset(train, select = -c(datetime, reg_date, order_cnt, rider_cnt, order_cnt_2, is_rain))
test_set <- subset(test, select = -c(datetime, reg_date,  order_cnt, rider_cnt, order_cnt_2, is_rain))

str(train_set)

# numeric variable mix-max scale

# x 변수의 최소값과 최대값 저장
# train_min <- min(train$rider_cnt)
# train_max <- max(train$rider_cnt)
# 
# test_min <- min(test$rider_cnt)
# test_max <- max(test$rider_cnt)
# 
# normalize <- function(x, na.rm = TRUE) {
#   return((x- min(x)) /(max(x)-min(x)))
# }
# 
# train_set[c('rider_cnt','temp_c', 'rain_c','order_cnt_w_1',
#             'order_cnt_w_2','order_cnt_w_3','order_cnt_w_4','rider_cnt_w_1',
#             'rider_cnt_w_2','rider_cnt_w_3','rider_cnt_w_4')] = normalize(train_set[c('rider_cnt','temp_c', 'rain_c','order_cnt_w_1',
#                                                                                   'order_cnt_w_2','order_cnt_w_3','order_cnt_w_4','rider_cnt_w_1', 
#                                                                                   'rider_cnt_w_2','rider_cnt_w_3','rider_cnt_w_4')])
# 
# test_set[c('rider_cnt','temp_c', 'rain_c','order_cnt_w_1',
#             'order_cnt_w_2','order_cnt_w_3','order_cnt_w_4','rider_cnt_w_1',
#             'rider_cnt_w_2','rider_cnt_w_3','rider_cnt_w_4')] = normalize(test_set[c('rider_cnt','temp_c', 'rain_c','order_cnt_w_1',
#                                                                                       'order_cnt_w_2','order_cnt_w_3','order_cnt_w_4','rider_cnt_w_1', 
#                                                                                       'rider_cnt_w_2','rider_cnt_w_3','rider_cnt_w_4')])
# 

library(forecast)

# all
model <- lm(rider_cnt_2 ~., data = train_set)
summary(model) 
accuracy(model) 

# rider_cnt_w_1, order_cnt_w_1
model1 <- lm(rider_cnt_2 ~ rider_cnt_w_1 + order_cnt_w_1, data=train_set)
summary(model1) 
accuracy(model1) 

# rider_cnt_w_2, order_cnt_w_2
model2 <- lm(rider_cnt_2 ~ rider_cnt_w_2 + order_cnt_w_2, data=train_set)
summary(model2) 
accuracy(model2) 

# rider_cnt_w_3, order_cnt_w_3
model3 <- lm(rider_cnt_2 ~ rider_cnt_w_3 + order_cnt_w_3, data=train_set)
summary(model3) 
accuracy(model3) 

# rider_cnt_w_4, order_cnt_w_4
model4 <- lm(rider_cnt_2 ~ rider_cnt_w_4 + order_cnt_w_4, data=train_set)
summary(model4) 
accuracy(model4) 

# 1~2
model5 <- lm(formula = rider_cnt_2 ~ pick_rgn2_nm + day_of_reg + temp_c + 
               rain_c + snow_c + month + week + order_cnt_w_1 + 
               order_cnt_w_2 +  rider_cnt_w_1 + rider_cnt_w_2 + is_holiday, 
             data = train_set)
summary(model5) 
accuracy(model5) 

# 1~3
model6 <- lm(formula = rider_cnt_2 ~ pick_rgn2_nm + day_of_reg + temp_c + 
               rain_c + snow_c + month + week + order_cnt_w_1 + 
               order_cnt_w_2 + order_cnt_w_3 + rider_cnt_w_1 + 
               rider_cnt_w_2 + rider_cnt_w_3 +  is_holiday, 
             data = train_set)

summary(model6) 
accuracy(model6) 

# stepAIC
stepAIC(model, direction = "both")
model_AIC <- lm(formula = rider_cnt_2 ~ pick_rgn2_nm + hour_reg + day_of_reg + 
                  temp_c + rain_c + snow_c + month + week + order_cnt_w_4 + 
                  rider_cnt_w_1 + rider_cnt_w_2 + rider_cnt_w_3 + rider_cnt_w_4 + 
                  is_holiday, data = train_set)


summary(model_AIC) 
accuracy(model_AIC)  


model_rain <- model_AIC

# p-value < 0.05 
summary(model)
model_fin <- lm(rider_cnt_2 ~ pick_rgn2_nm + hour_reg + day_of_reg +rain_c + snow_c + month + week + 
                  rider_cnt_w_1 + rider_cnt_w_2 + rider_cnt_w_3 + rider_cnt_w_4 + is_holiday, data = train_set) 
summary(model_fin) 
accuracy(model_fin) 

#다중공선성
library(car)

vif(model) 
vif(model_AIC)

  

# plot 
plot(test$rider_cnt)
lines(model$fitted.values, col = "red")
lines(model1$fitted.values, col = "blue")
lines(model2$fitted.values, col = "green")
lines(model3$fitted.values, col = "yellow")
lines(model4$fitted.values, col = "navy")
lines(model5$fitted.values, col = "pink")
lines(model6$fitted.values, col = "orange")


# test set 
#install.packages("Metrics")
library(Metrics)
# accuracy 
# # 예측값을 normalize() 함수에서 사용한 최소값과 최대값으로 되돌리기
y_pred <- predict(model_AIC, newdata = test_set) # 모델의 예측값
#y_pred_rescaled <- y_pred * (test_max - test_min) + test_min


r_square = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

actual <- test$rider_cnt_2

rmse <- rmse(actual, y_pred)
r_square <- r_square(actual, y_pred)
mae <- mae(actual, y_pred)
error = y_pred - actual

rmse 
r_square 
mae 

result <- data.frame(y_pred, actual,test$reg_date, test$hour_reg,test$day_of_reg, test$pick_rgn2_nm, test$month, test$is_holiday, error)
summary(result$error)

#############################################################################################################################################
# 지역별 시간대별, 기상 유무 mae 
# 시간대별
result <- aggregate(abs(y_pred - actual), 
                    by = list(test_set$hour_reg), 
                    FUN = mean)

result <- data.frame(result)


#지역별
result2 <-aggregate(abs(y_pred - actual), 
                    by = list(test_set$pick_rgn2_nm), 
                    FUN = mean) 

result2<- data.frame(result2)


#시간,지역별
result4 <- aggregate(abs(y_pred - actual), 
                     by = list(test_set$hour_reg, test_set$pick_rgn2_nm), 
                     FUN = mean)

result4<- data.frame(result4)
write.csv(result4, "result4_3.csv", row.names = FALSE, fileEncoding = "cp949")

#############################################################################################################################################
# 잔차 
# 잔차 등분산성
par(mfrow = c(2,2))
plot(model_AIC)

#잔차 독립성
library(car)
hap_1_res <- residuals(model_AIC)
durbinWatsonTest(hap_1_res) #0.7424

#잔차 정규성
shapiro.test(hap_1_res)




