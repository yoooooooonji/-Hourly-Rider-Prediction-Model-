# 1. 패키지 불러오기
library(forecast)
library(prophet)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# 2. 데이터 불러오기 및 전처리
getwd()
data <- read.csv("final_data.csv", fileEncoding = "cp949")
data<- subset(data, select = -c(rider_cnt, order_cnt, temp_c, rain_c, snow_c, q1, q3,IQR1.5, outlier))
head(data)
str(data)

data$datetime <- as.POSIXct(data$datetime, format="%Y-%m-%d %H:%M:%S") # datetime 컬럼을 POSIXct 형식으로 변환
data <- data %>% arrange(datetime) # datetime을 기준으로 데이터 정렬

var <-  c('pick_rgn2_nm', 'hour_reg','day_of_reg','is_rain','month','week','is_holiday')
data[,var]<- lapply(data[,var], factor)

# 지역별로 데이터를 분리
locations <- unique(data$pick_rgn2_nm)

# 데이터 분할 (8:2)
set.seed(123)
#train_idx <- sample(nrow(data), round(nrow(data) * 0.8))
# train_data <- data[train_idx, ]
# test_data <- data[-train_idx, ]
train_data <- data  %>% filter(reg_date <= '2022-12-31')
test_data <- data  %>% filter(reg_date >= '2023-01-01')

dim(train_data) #218,775
dim(test_data) # 72,000

# 각 지역별 모델 및 예측 결과를 저장할 빈 리스트
arima_results <- list()
prophet_results <- list()


# 각 지역별로 ARIMA 및 Prophet 모델 훈련 및 예측 수행
for (location in locations) {
  data_location_train <- train_data %>% filter(pick_rgn2_nm == !!location)
  data_location_test <- test_data %>% filter(pick_rgn2_nm == !!location)

  # ARIMA 모델
  ts_data_location_train <- ts(data_location_train[, c("rider_cnt_2", "is_rain")], frequency = 24) 
  ts_data_location_test <- ts(data_location_test[, c("rider_cnt_2", "is_rain")], frequency = 24) 

  arima_model_location <- auto.arima(ts_data_location_train[,"rider_cnt_2"], xreg = ts_data_location_train[,"is_rain"])
  arima_forecast_location <- forecast(arima_model_location, xreg = ts_data_location_test[, "is_rain"])
  result_arima <- data.frame(data_location_test$datetime, arima_forecast_location$mean, arima_forecast_location$lower[,2], arima_forecast_location$upper[,2])
  colnames(result_arima) <- c("datetime", "arima_mean", "arima_lower", "arima_upper")
  #arima_results[[as.character(location)]] <- data.frame(test_data$datetime, arima_forecast_location$mean, arima_forecast_location$lower[,2], arima_forecast_location$upper[,2])
  
# Prophet 모델
# 이벤트 여부를 고려한 PROPHET 데이터 전처리
prophet_data_location_train <- data_location_train[c("datetime", "rider_cnt_2", "is_rain")] %>% 
  rename(ds = datetime, y = rider_cnt_2)

holidays <- prophet_data_location_train %>% filter(is_rain == 1) %>% 
  mutate(holiday = "rain")
holidays <- holidays[c("holiday","ds")]

# PROPHET 모델 생성 (이벤트 여부 포함)
prophet_model_location <- prophet(holidays = holidays, yearly.seasonality = TRUE)
#prophet_model_location <- add_regressor(prophet_model_location, "is_rain")
#prophet_model_location <- fit(prophet_model_location, prophet_data_location_train)

#PROPHET 모델 예측 (이벤트 여부 포함)
future_location <- make_future_dataframe(prophet_model_location, periods = nrow(data_location_test), freq = "hour")
prophet_forecast_location <- predict(prophet_model_location, future_location)
prophet_results[[as.character(location)]] <- data.frame(pick_rgn2_nm = location, forecast = prophet_forecast_location$yhat, lower = prophet_forecast_location$yhat_lower, upper = prophet_forecast_location$yhat_upper)

}


#결과 데이터프레임 만들기
arima_results <- do.call(rbind, arima_results)
prophet_results <- do.call(rbind, prophet_results)

#결과 데이터프레임에 실제값 넣기
arima_results$actual <- test_data$rider_cnt_2
prophet_results$actual <- test_data$rider_cnt_2


# 5. 성능 비교 (MAE, RMSE)
# MAE, RMSE, MAPE 계산
actual_data <- test_data$rider_cnt_2

arima_mae <- mae(actual = actual_data, predicted = unlist(lapply(arima_results, function(x) x$mean)))
arima_rmse <- rmse(actual = actual_data, predicted = unlist(lapply(arima_results, function(x) x$mean)))
arima_mape <- mape(actual = actual_data, predicted = unlist(lapply(arima_results, function(x) x$mean)))

prophet_mae <- mae(actual = actual_data, predicted = unlist(lapply(prophet_results, function(x) x$yhat)))
prophet_rmse <- rmse(actual = actual_data, predicted = unlist(lapply(prophet_results, function(x) x$yhat)))
prophet_mape <- mape(actual = actual_data, predicted = unlist(lapply(prophet_results, function(x) x$yhat))))

# 성능 비교 결과 출력
cat("ARIMA - MAE:", arima_mae, " RMSE:", arima_rmse, "MAPE:", arima_mape, "\n")  
cat("prophet - MAE:", lm_mae, " RMSE:", lm_rmse, "MAPE:", lm_mape, "\n")  


# 강남구
# gn_data <- data  %>% filter(pick_rgn2_nm == '강남구')
# dim(gn_data) #11631
# min(gn_data$datetime)
# max(gn_data$datetime)

# # 3. ARIMA 모델 훈련 및 예측
# # 이벤트 여부를 고려한 시계열 데이터로 변환
# ts_data_event <- ts(gn_data[, c("rider_cnt_2", "is_rain")], frequency = 24) # 시간대별 방문자 수와 이벤트 여부를 시계열 데이터로 변환 (24시간 단위)

# # ARIMA 모델 훈련 (이벤트 여부 포함)
# arima_model_event <- auto.arima(ts_data_event[, "rider_cnt_2"], xreg = ts_data_event[, "is_rain"])

# # ARIMA 모델 예측 (이벤트 여부 포함)
# arima_forecast_event <- forecast(arima_model_event, h=24, xreg = tail(ts_data_event[, "is_rain"], 24*7*30)) # 다음 24시간 예측, 마지막 24시간의 이벤트 여부를 외생 변수로 사용


# # 4. PROPHET 모델 훈련 및 예측

# # 이벤트 여부를 고려한 PROPHET 데이터 전처리
# prophet_data_event <- gn_data[c("datetime", "rider_cnt_2", "is_rain")] %>% 
# rename(ds = datetime, y = rider_cnt_2)

# holidays <- prophet_data_event %>% filter(is_rain == 1) %>% mutate(holiday = "rain")
# holidays <- holidays[c("holiday","ds")]

# # PROPHET 모델 훈련 (이벤트 여부 포함)
# prophet_model_event <- prophet(prophet_data_event, holidays = holidays,yearly.seasonality = TRUE)

# # PROPHET 모델 예측 (이벤트 여부 포함)
# future_event <- make_future_dataframe(prophet_model_event, periods = 24*7*30, freq = "hour") # 다음 24시간 예측을 위한 데이터 프레임 생성
# prophet_forecast_event <- predict(prophet_model_event,future_event) 

# # 5. 성능 비교 (MAE, RMSE)
# # 실제 값 추출
# actual <- tail(gn_data$rider_cnt_2, 24*7*30) # 실제 값 추출 (마지막 1주일)

# # ARIMA 예측값 추출
# arima_predicted <- arima_forecast_event$mean

# # PROPHET 예측값 추출
# prophet_predicted <- tail(prophet_forecast_event$yhat, 24*7*30)

# # MAE 계산
# mae_arima <- mean(abs(actual - arima_predicted))
# mae_prophet <- mean(abs(actual - prophet_predicted))

# # RMSE 계산
# rmse_arima <- sqrt(mean((actual - arima_predicted)^2))
# rmse_prophet <- sqrt(mean((actual - prophet_predicted)^2))

# # 성능 비교 결과 출력
# cat("ARIMA - MAE:", mae_arima, " RMSE:", rmse_arima, "\n")  # mae 38.32, rmse 63.32
# cat("PROPHET - MAE:", mae_prophet, "RMSE:", rmse_prophet, "\n") # 52.81, rmse 70.01
