# 1. 패키지 불러오기
library(forecast)
install.packages("prophet")
library(prophet)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# 2. 데이터 불러오기 및 전처리
getwd()
data <- read_excel("final_data.xls") 

data$datetime <- as.POSIXct(data$datetime, format="%Y-%m-%d %H:%M:%S") # datetime 컬럼을 POSIXct 형식으로 변환
data <- data %>% arrange(datetime) # datetime을 기준으로 데이터 정렬

# 지역별로 데이터를 분리
locations <- unique(data$pick_rgn2_nm)

# 각 지역별 모델 및 예측 결과를 저장할 빈 리스트
arima_results <- list()
prophet_results <- list()

# 각 지역별로 ARIMA 및 Prophet 모델 훈련 및 예측 수행
for (location in locations) {
  data_location <- data %>% filter(location == !!location)
  
  # ARIMA 모델
  ts_data_location <- ts(data_location[, c("rider_cnt_2", "is_rain")], frequency = 24) # 시간대별 방문자 수를 시계열 데이터로 변환 (24시간 단위)
  arima_model_location <- auto.arima(ts_data_location[,"rider_cnt_2"], xreg = ts_data_location[,"is_rain"])
  arima_forecast_location <- forecast(arima_model_location, xreg = taio(ts_data_location[,"is_rain"], h=24*7)
  arima_results[[as.character(location)]] <- arima_forecast_location$mean
  
  # Prophet 모델
  prophet_data_location <- data_location %>% select(datetime, rider_cnt_2) %>% rename(ds=datetime, y=rider_cnt_2)
  prophet_model_location <- prophet(prophet_data_location)
  future_location <- make_future_dataframe(prophet_model_location, periods = 24*7, freq = "hour")
  prophet_forecast_location <- predict(prophet_model_location, future_location)
  prophet_results[[as.character(location)]] <- tail(prophet_forecast_location$yhat, 24*7)
}

arima_results <- data.frame(arima_results)
prophet_results <- data.frame(prophet_results)

write.csv(arima_results, "arima_results.csv", row.names = FALSE, fileEncoding = "cp949")
write.csv(prophet_results, "prophet_results.csv", row.names = FALSE, fileEncoding = "cp949")

################################################################################
# 강남구
gn_data <- data  %>% filter(pick_rgn2_nm == '강남구')
dim(gn_data) #11631
min(gn_data$datetime)
max(gn_data$datetime)

# 3. ARIMA 모델 훈련 및 예측
# 이벤트 여부를 고려한 시계열 데이터로 변환
ts_data_event <- ts(gn_data[, c("rider_cnt_2", "is_rain")], frequency = 24) # 시간대별 방문자 수와 이벤트 여부를 시계열 데이터로 변환 (24시간 단위)

# ARIMA 모델 훈련 (이벤트 여부 포함)
arima_model_event <- auto.arima(ts_data_event[, "rider_cnt_2"], xreg = ts_data_event[, "is_rain"])

# ARIMA 모델 예측 (이벤트 여부 포함)
arima_forecast_event <- forecast(arima_model_event, h=24, xreg = tail(ts_data_event[, "is_rain"], 24*7*30)) # 다음 24시간 예측, 마지막 24시간의 이벤트 여부를 외생 변수로 사용


# 4. PROPHET 모델 훈련 및 예측

# 이벤트 여부를 고려한 PROPHET 데이터 전처리
prophet_data_event <- gn_data[c("datetime", "rider_cnt_2", "is_rain")] %>% 
rename(ds = datetime, y = rider_cnt_2)

holidays <- prophet_data_event %>% filter(is_rain == 1) %>% mutate(holiday = "rain")
holidays <- holidays[c("holiday","ds")]

# PROPHET 모델 훈련 (이벤트 여부 포함)
prophet_model_event <- prophet(prophet_data_event, holidays = holidays,yearly.seasonality = TRUE)

# PROPHET 모델 예측 (이벤트 여부 포함)
future_event <- make_future_dataframe(prophet_model_event, periods = 24*7*30, freq = "hour") # 다음 24시간 예측을 위한 데이터 프레임 생성
prophet_forecast_event <- predict(prophet_model_event,future_event) 

# 5. 성능 비교 (MAE, RMSE)
# 실제 값 추출
actual <- tail(gn_data$rider_cnt_2, 24*7*30) # 실제 값 추출 (마지막 1주일)

# ARIMA 예측값 추출
arima_predicted <- arima_forecast_event$mean

# PROPHET 예측값 추출
prophet_predicted <- tail(prophet_forecast_event$yhat, 24*7*30)

# MAE 계산
mae_arima <- mean(abs(actual - arima_predicted))
mae_prophet <- mean(abs(actual - prophet_predicted))

# RMSE 계산
rmse_arima <- sqrt(mean((actual - arima_predicted)^2))
rmse_prophet <- sqrt(mean((actual - prophet_predicted)^2))

# 성능 비교 결과 출력
cat("ARIMA - MAE:", mae_arima, " RMSE:", rmse_arima, "\n")  # mae 38.32, rmse 63.32
cat("PROPHET - MAE:", mae_prophet, "RMSE:", rmse_prophet, "\n") # 52.81, rmse 70.01

# 피크시간대 비교하기 / 기상  <- 이 시간대 잘 맞추는 모델 선정


