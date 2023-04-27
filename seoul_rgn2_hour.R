# 0. install packages 

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
table(data$pick_rgn2_nm)

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

var <-  c('month','week','hour_reg2', 'is_rain')
data[,var]<- lapply(data[,var], factor)

# 전일 동시간대 주문수
str(data)

data <- data %>% 
  group_by(pick_rgn2_nm, hour_reg) %>% 
  arrange(reg_date) %>% 
  mutate(order_cnt_lag = lag(order_cnt))

# 전시간 주문수 
data <- data %>% 
  group_by(pick_rgn2_nm) %>% 
  arrange(reg_date, hour_reg2) %>% 
  mutate(order_cnt_last = lag(order_cnt))

# 전주 동일 요일 동시간대 주문수
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(order_cnt_last_week = lag(order_cnt))

# 전주 동일 요일 동시간대 라이더수 
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn2_nm) %>% 
  arrange(reg_date) %>% 
  mutate(rider_cnt_last_week = lag(rider_cnt))

# holiday
# 공휴일 여부 판단 함수 정의
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

# check NA
colSums(is.na(data)) 

data <- data %>% 
  filter(!is.na(order_cnt_lag)&!is.na(order_cnt_last_week) & !is.na(rider_cnt_last_week) & !is.na(order_cnt_last)) 

dim(data) # 194,636

data[c("rider_cnt", "order_cnt", "pick_rgn2_nm")] %>% tbl_summary( by = "pick_rgn2_nm")

#write.csv(data, "rider_hour_rgn2_seoul_data.csv", row.names = FALSE, fileEncoding = "cp949")
#####################################################################################################
#modeling
str(data)

var <-  c('day_of_reg', 'pick_rgn1_nm', 'pick_rgn2_nm','is_holiday')
data[,var]<- lapply(data[,var], factor)

# train_test 
train <- data[c("reg_date","hour_reg2", "day_of_reg","pick_rgn2_nm", "rider_cnt",  "temp_c",
                "rain_c", "snow_c", "wind","humidity", "month", "week", "order_cnt_lag", "order_cnt_last_week", 
                "rider_cnt_last_week", "is_holiday", "is_rain", "order_cnt_last")] %>% 
  filter(reg_date <= '2022-12-31')

dim(train) # 155,530

test <- data[c("reg_date","hour_reg2", "day_of_reg","pick_rgn2_nm", "rider_cnt",  "temp_c",
               "rain_c", "snow_c", "wind","humidity", "month", "week", "order_cnt_lag", "order_cnt_last_week", 
               "rider_cnt_last_week", "is_holiday", "is_rain", "order_cnt_last")] %>% 
  filter(reg_date >= '2023-01-01')

dim(test) # 39,106

train <- subset(train, select = -reg_date)
test <- subset(test, select = -reg_date)

# correlation
cor(data$rider_cnt, data$order_cnt_lag) # 0.92
cor(data$rider_cnt, data$order_cnt_last_week) # 0.95
cor(data$rider_cnt, data$rain_c) # -0.004
cor(data$rider_cnt, data$temp_c) # 0.065
cor(data$rider_cnt, data$snow_c) # -0.033 
cor(data$rider_cnt, data$humidity) # -0.14
cor(data$rider_cnt, data$wind) # 0.12
cor(data$rider_cnt, data$rider_cnt_last_week) # 0.97
cor(data$rider_cnt, data$order_cnt_last) # 0.84
cor(data$rider_cnt, data$order_cnt) # 0.97

library(forecast)

# all
model <- lm(rider_cnt ~., data = train)
summary(model) # 0.967
accuracy(model)

# order_cnt_lag, order_cnt_last_week
model1 <- lm(rider_cnt ~ order_cnt_lag + order_cnt_last_week, data =train)
summary(model1) # 0.9177
accuracy(model1)

# order_cnt_lag
model2 <- lm(rider_cnt ~ order_cnt_lag, data =train)
summary(model2) # 0.8596
accuracy(model2)

# order_cnt_last_week
model3 <- lm(rider_cnt ~ order_cnt_last_week, data = train)
summary(model3) # 0.9106 
accuracy(model3)

# rider_cnt_last_week
model4 <- lm(rider_cnt ~ rider_cnt_last_week, data = train)
summary(model4) # 0.9497
accuracy(model4)

# rider_cnt_last_week, order_cnt_last_week
model5 <- lm(rider_cnt ~ rider_cnt_last_week + order_cnt_last_week, data = train)
summary(model5) # 0.9498
accuracy(model5)

# p-value < 0.05
model6 <- lm(formula = rider_cnt ~ hour_reg2 + day_of_reg + pick_rgn2_nm + 
               rain_c + snow_c + wind + humidity + month + week + order_cnt_lag + 
               order_cnt_last_week + rider_cnt_last_week + is_holiday + 
               is_rain + order_cnt_last, data = train)

summary(model6) # 0.967
accuracy(model6)

# stepAIC
#stepAIC(model, direction = "both")
model7 <- lm(formula = rider_cnt ~ hour_reg2 + day_of_reg + pick_rgn2_nm + 
               rain_c + snow_c + wind + humidity + month + week + order_cnt_lag + 
               order_cnt_last_week + rider_cnt_last_week + is_holiday + 
               is_rain + order_cnt_last, data = train)

summary(model7) # 0.967
accuracy(model7)

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
# accuracy 
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

actual <- test$rider_cnt
p1 <- predict(model, newdata = test)
p2 <- predict(model1, newdata = test)
p3 <- predict(model2, newdata = test)
p4 <- predict(model3, newdata = test)
p5 <- predict(model4, newdata = test)
p6 <- predict(model5, newdata = test)
p7 <- predict(model7, newdata = test)


rmse1 <- rmse(actual, p1)
rmse2 <- rmse(actual, p2)
rmse3 <- rmse(actual, p3)
rmse4 <- rmse(actual, p4)
rmse5 <- rmse(actual, p5)
rmse6 <- rmse(actual, p6)
rmse7 <- rmse(actual, p7)

rmse1 
rmse2 
rmse3 
rmse4 
rmse5 
rmse6 
rmse7 

#mae
mae1 <- mean(abs(actual-p1))
mae2 <- mean(abs(actual-p2))
mae3 <- mean(abs(actual-p3))
mae4 <- mean(abs(actual-p4))
mae5 <- mean(abs(actual-p5))
mae6 <- mean(abs(actual-p6))
mae7 <- mean(abs(actual-p7))

mae1 #29.68
mae2 # 38.69
mae3 # 30.26
mae4 # 19.31
mae5 # 19.33
mae6 # 19.02
mae7 # 19.02

error = p7 - actual
result <- data.frame(p7, actual,test$hour_reg2,test$day_of_reg, test$pick_rgn2_nm, test$month, test$rain_c, test$is_holiday, test$is_rain, error)
summary(result$error)


good <- result %>% 
  filter(error >= -20 & error <= 20) #26110

bad <- result %>% 
  filter(error <=-100 | error >= 100) # 440

table(good$test.pick_rgn2_nm)
table(bad$test.pick_rgn2_nm)

#12556
medium <- result %>% 
  filter(-100 < error & error < -20 | 20 < error & error < 100)

dim(test) # 39106 
summary(bad) #피크시간, 일요일, 강남구 
summary(good) #야간시간, 중구,종로구,성동구,중랑구,도봉구,용산구 / 
summary(medium) #피크시간, 강남구,금토일

# 잔차 등분산성
par(mfrow = c(2,2))
plot(model7)

#잔차 독립성
library(car)
hap_1_res <- residuals(model7)
durbinWatsonTest(hap_1_res) #1.06

#잔차 정규성
shapiro.test(hap_1_res)

#다중공선성
vif(model7) #last_week 2개



