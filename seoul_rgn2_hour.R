# 0. install packages 
options(scipen=999)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg<-c("readr","dplyr","tidytext","tidyverse","lubridate","reshape2","psych","gtsummary", "readxl", "MASS")
ipak(pkg)

##########################################################################################################################################################
# 1. data load
data1 <- read_excel("/Users/yj.noh/Desktop/rider_pick2_hour_2022.xlsx") 
data2 <- read_excel("/Users/yj.noh/Desktop/rider_pick2_hour_2023.xlsx")

data <- rbind(data1,data2) 
data <- data %>% 
  dplyr::rename(rider_cnt = 라이더수,
                order_cnt = 주문수)
# seoul
data <- data %>% 
  filter(pick_rgn1_nm == '서울특별시')

dim(data) # 197804

# weather
weather1 <- read.csv("/Users/yj.noh/Desktop/weather_2022.csv", fileEncoding = "cp949") 
weather2 <- read.csv("/Users/yj.noh/Desktop/weather_2023.csv", fileEncoding = "cp949")

weather <- rbind(weather1, weather2)
weather <- weather %>% 
  dplyr::rename(temp_c = 기온..C.,
         rain_c = 강수량.mm.,
         snow_c = 적설.cm.,
         date = 일시,
         wind = 풍속.m.s.,
         humidity = 습도...)

weather <- weather %>% 
  mutate(date_2 = as.Date(date),
         hour = hour(date))

weather <- weather %>% 
  mutate(date_3 = case_when (hour %in% c(0,1,2) ~ date_2-1, 
                             TRUE ~ date_2)) 

# 2. join 
data$reg_date <- as.Date(data$reg_date)
data <- left_join(data, weather[c("date_3","hour","temp_c","rain_c", "snow_c", "wind", "humidity")], by = c("reg_date" = "date_3", "hour_reg" = "hour"))

# 3. na 
data$rain_c[is.na(data$rain_c)] <- 0
data$snow_c[is.na(data$snow_c)] <- 0
data$rain_c[is.na(data$wind)] <- 0
data$snow_c[is.na(data$humidity)] <- 0

colSums(is.na(data))

# 4. 변수 생성
# month, week, hour, day, location -> category 
data <- data %>% 
  mutate(hour_reg2 = case_when(hour_reg == 0 ~ 24,
                              hour_reg == 1 ~ 25,
                              hour_reg == 2 ~ 26,
                              TRUE ~ hour_reg))

data <- data %>% 
  mutate(month = month(reg_date),
         week = ceiling(day(reg_date) /7))

data <- data %>% 
  mutate(is_rain = ifelse((rain_c > 0 | snow_c > 0),1,0))

table(data$is_rain)  # 0 : 175784 , 1: 18852         

# 전일 동시간대 주문수
# str(data)
# 
# data <- data %>% 
#   group_by(pick_rgn2_nm, hour_reg) %>% 
#   arrange(reg_date) %>% 
#   mutate(order_cnt_lag = lag(order_cnt))

# 전시간 주문수 
# data <- data %>% 
#   group_by(pick_rgn2_nm) %>% 
#   arrange(reg_date, hour_reg2) %>% 
#   mutate(order_cnt_last = lag(order_cnt))

# w-1 동일 요일 동시간대 주문수
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(order_cnt_w_1= lag(order_cnt, n=1))

# w-2 동일 요일 동시간대 주문수
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(order_cnt_w_2 = lag(order_cnt, n=2))

# w-3 동일 요일 동시간대 주문수
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(order_cnt_w_3 = lag(order_cnt, n=3))

# w-4 동일 요일 동시간대 주문수
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(order_cnt_w_4 = lag(order_cnt, n=4))

# w-1 동일 요일 동시간대 라이더수 
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(rider_cnt_w_1 = lag(rider_cnt, n=1))

# w-2 동일 요일 동시간대 라이더수 
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(rider_cnt_w_2 = lag(rider_cnt, n=2))

# w-3 동일 요일 동시간대 라이더수 
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(rider_cnt_w_3 = lag(rider_cnt, n=3))

# w-4 동일 요일 동시간대 라이더수 
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(rider_cnt_w_4 = lag(rider_cnt, n=4))

# 주문수/라이더수 
data <- data %>% 
  mutate(infra_w_1 = order_cnt_w_1/rider_cnt_w_1,
         infra_w_2 = order_cnt_w_2/rider_cnt_w_2,
         infra_w_3 = order_cnt_w_3/rider_cnt_w_3,
         infra_w_4 = order_cnt_w_4/rider_cnt_w_4)
  
# is_holiday
holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09", 
                     "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15",
                     "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", "2022-10-03", 
                     "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21",
                     "2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-05",
                     "2023-05-27", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
                     "2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))

data$reg_date <- as.Date(data$reg_date)
data <- data %>% 
  mutate(is_holiday = ifelse((reg_date %in% holiday_list) | (day_of_reg %in% c("FRI", "SAT", "SUN")),1,0))

# is_peak
data <- data %>% 
  mutate(is_peak1 = ifelse(hour_reg2 %in% c(11,12,13),1,0),
         is_peak2 = ifelse(hour_reg2 %in% c(17,18,19,20),1,0))

# check NA
colSums(is.na(data)) 

data <- data %>% 
  filter(!is.na(order_cnt_w_1)&!is.na(order_cnt_w_2) & !is.na(order_cnt_w_3) & !is.na(order_cnt_w_4)) 

dim(data) # 185,188

# data[c("rider_cnt", "order_cnt", "pick_rgn2_nm")] %>% tbl_summary( by = "pick_rgn2_nm") %>% add_p()
# data[c("rider_cnt","order_cnt","pick_rgn2_nm", "is_rain")] %>% tbl_summary(by="is_rain")  %>% add_p()
# data[c("rider_cnt","order_cnt","pick_rgn2_nm", "is_peak1")] %>% tbl_summary(by="is_peak1")  %>% add_p()
# data[c("rider_cnt","order_cnt","pick_rgn2_nm", "is_peak2")] %>% tbl_summary(by="is_peak2") %>% add_p()

#write.csv(data, "rider_hour_rgn2_seoul_data.csv", row.names = FALSE, fileEncoding = "cp949")
#####################################################################################################
#modeling
str(data)

var <-  c('hour_reg', 'day_of_reg', 'pick_rgn1_nm','hour_reg2', 'month', 'week', 'is_rain',
          'is_holiday','is_peak1' ,'is_peak2')
data[,var]<- lapply(data[,var], factor)

# train_test 
train <- data %>% 
  filter(reg_date <= '2022-12-31')

dim(train) # 146085

test <- data %>% 
  filter(reg_date >= '2023-01-01')

dim(test) # 39,103

train_set <- subset(train, select = -c(reg_date, hour_reg, pick_rgn1_nm, order_cnt, wind, humidity))
test_set <- subset(test, select = -c(reg_date, hour_reg, pick_rgn1_nm, order_cnt, wind, humidity))

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

summary(train_set)

# correlation
cor(data$rider_cnt, data$order_cnt_w_1) # 0.95
cor(data$rider_cnt, data$order_cnt_w_2) # 0.95
cor(data$rider_cnt, data$order_cnt_w_3) # 0.94
cor(data$rider_cnt, data$order_cnt_w_4) # 0.94

cor(data$rider_cnt, data$rider_cnt_w_1) #0.97
cor(data$rider_cnt, data$rider_cnt_w_2) #0.97
cor(data$rider_cnt, data$rider_cnt_w_3) # 0.96
cor(data$rider_cnt, data$rider_cnt_w_4) #0.96

cor(data$rider_cnt, data$rain_c) # -0.005
cor(data$rider_cnt, data$temp_c) # 0.064
cor(data$rider_cnt, data$snow_c) # -0.0331


library(forecast)

# all
model <- lm(rider_cnt ~., data = train_set)
summary(model) # 0.968
accuracy(model) #mae 0.004

# order_cnt_w_1
model1 <- lm(rider_cnt ~ order_cnt_w_1, data =train_set)
summary(model1) # 0.9102
accuracy(model1) #mae 0.008

# order_cnt_w_2
model2 <- lm(rider_cnt ~ order_cnt_w_2, data =train_set)
summary(model2) # 0.905
accuracy(model2) #mae 0.008

# order_cnt_w_3
model3 <- lm(rider_cnt ~ order_cnt_w_3, data =train_set)
summary(model3) # 0.9006
accuracy(model3) #mae 0.008

# order_cnt_w_4
model4 <- lm(rider_cnt ~ order_cnt_w_4, data =train_set)
summary(model4) # 0.8993
accuracy(model4) #mae 0.008

# rider_cnt_w_1
model5 <- lm(rider_cnt ~ rider_cnt_w_1, data = train_set)
summary(model5) # 0.9496
accuracy(model5) #0.005

# rider_cnt_w_2
model6 <- lm(rider_cnt ~ rider_cnt_w_2, data = train_set)
summary(model6) # 0.945
accuracy(model6) #0.005

# rider_cnt_w_3
model7 <- lm(rider_cnt ~ rider_cnt_w_3, data = train_set)
summary(model7) # 0.9404
accuracy(model7) #0.005

# rider_cnt_w_4
model8 <- lm(rider_cnt ~ rider_cnt_w_4, data = train_set)
summary(model8) # 0.9389
accuracy(model8) #0.005

# stepAIC
stepAIC(model, direction = "both")
model_AIC <- lm(formula = rider_cnt ~ day_of_reg + pick_rgn2_nm + rain_c + 
                  snow_c + hour_reg2 + month + week + is_rain + order_cnt_w_1 + 
                  order_cnt_w_2 + order_cnt_w_4 + rider_cnt_w_1 + rider_cnt_w_2 + 
                  rider_cnt_w_3 + rider_cnt_w_4 + infra_w_1 + infra_w_3 + infra_w_4 + 
                  is_holiday, data = train_set)

summary(model_AIC) #0.9687
accuracy(model_AIC) 
  

# p-value < 0.05 
model_fin <- lm (rider_cnt ~ day_of_reg + pick_rgn2_nm + rain_c + 
                   snow_c + hour_reg2 + month + week + is_rain + order_cnt_w_1+ 
                   order_cnt_w_2 + order_cnt_w_4 + rider_cnt_w_1 + rider_cnt_w_2 + 
                   rider_cnt_w_3 + rider_cnt_w_4 + infra_w_1 + infra_w_3 + infra_w_4 + 
                   is_holiday, data = train_set)

summary(model_fin) # 0.9687
accuracy(model_fin) # 0.004

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
install.packages("Metrics")
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

actual <- test$rider_cnt

rmse <- rmse(actual, y_pred)
r_square <- r_square(actual, y_pred)
mae <- mae(actual, y_pred)
error = y_pred - actual

rmse # 32
r_square # 0.95
mae # 22.96

result <- data.frame(y_pred, actual,test$reg_date, test$hour_reg2,test$day_of_reg, test$pick_rgn2_nm, test$month, test$rain_c, test$is_holiday, test$is_rain, error)
summary(result$error)

#############################################################################################################################################
# 지역별 시간대별, 기상 유무 mae 
result <- aggregate(abs(y_pred - actual), 
                    by = list(test_set$hour_reg2), 
                    FUN = mean)
result

result2 <-aggregate(abs(y_pred - actual), 
                    by = list(test_set$pick_rgn2_nm), 
                    FUN = mean) 
result2

result3 <- aggregate(abs(y_pred - actual), 
                     by = list(test_set$is_rain), 
                     FUN = mean)
result3

result4 <- aggregate(abs(y_pred - actual), 
                     by = list(test_set$hour_reg2, test_set$pick_rgn2_nm), 
                     FUN = mean)
result4
#############################################################################################################################################
# 잔차 
# 잔차 등분산성
par(mfrow = c(2,2))
plot(model_AIC)

#잔차 독립성
library(car)
hap_1_res <- residuals(model_AIC)
durbinWatsonTest(hap_1_res) #1.06

#잔차 정규성
shapiro.test(hap_1_res)




