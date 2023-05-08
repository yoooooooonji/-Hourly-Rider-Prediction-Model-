# 1. 패키지 불러오기
library(forecast)
library(prophet)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# 2. 데이터 불러오기 및 전처리
data <- read_csv("your_data_file.csv") # 데이터 파일 불러오기
data$datetime <- as.POSIXct(data$datetime, format="%Y-%m-%d %H:%M:%S") # datetime 컬럼을 POSIXct 형식으로 변환
data <- data %>% arrange(datetime) # datetime을 기준으로 데이터 정렬

# 3. ARIMA 모델 훈련 및 예측
# 시계열 데이터로 변환
ts_data <- ts(data$visitors, frequency = 24) # 시간대별 방문자 수를 시계열 데이터로 변환 (24시간 단위)
# 이벤트 여부를 고려한 시계열 데이터로 변환
ts_data_event <- ts(data[, c("visitors", "event")], frequency = 24) # 시간대별 방문자 수와 이벤트 여부를 시계열 데이터로 변환 (24시간 단위)

# ARIMA 모델 훈련
arima_model <- auto.arima(ts_data)

# ARIMA 모델 훈련 (이벤트 여부 포함)
arima_model_event <- auto.arima(ts_data_event[, "visitors"], xreg = ts_data_event[, "event"])

# ARIMA 모델 예측
arima_forecast <- forecast(arima_model, h=24) # 다음 24시간 예측

# ARIMA 모델 예측 (이벤트 여부 포함)
arima_forecast_event <- forecast(arima_model_event, h=24, xreg = tail(ts_data_event[, "event"], 24)) # 다음 24시간 예측, 마지막 24시간의 이벤트 여부를 외생 변수로 사용


# 4. PROPHET 모델 훈련 및 예측
# PROPHET을 위한 데이터 전처리
prophet_data <- data %>% select(datetime, visitors) %>% rename(ds=datetime, y=visitors)

# 이벤트 여부를 고려한 PROPHET 데이터 전처리
prophet_data_event <- data %>% select(datetime, visitors, event) %>% rename(ds=datetime, y=visitors)
holidays <- prophet_data_event %>% filter(event == 1) %

# PROPHET 모델 훈련
prophet_model <- prophet(prophet_data)

# PROPHET 모델 훈련 (이벤트 여부 포함)
prophet_model_event <- prophet(prophet_data_event, holi

# PROPHET 모델 예측
future <- make_future_dataframe(prophet_model, periods = 24*7, freq = "hour") # 다음 24시간 예측을 위한 데이터 프레임 생성
prophet_forecast <- predict(prophet_model, future)

# PROPHET 모델 예측 (이벤트 여부 포함)
future_event <- make_future_dataframe(prophet_model_event, periods = 24*7, freq = "hour") # 다음 24시간 예측을 위한 데이터 프레임 생성
prophet_forecast_event <- predict(prophet_model_event,future_event) 

# 5. 성능 비교 (MAE, RMSE)
# 실제 값 추출
actual <- tail(data$visitors, 24*7) # 실제 값 추출 (마지막 1주일)

# ARIMA 예측값 추출
arima_predicted <- arima_forecast$mean

# PROPHET 예측값 추출
prophet_predicted <- tail(prophet_forecast$yhat, 24*7)

# MAE 계산
mae_arima <- mean(abs(actual - arima_predicted))
mae_prophet <- mean(abs(actual - prophet_predicted))

# RMSE 계산
rmse_arima <- sqrt(mean((actual - arima_predicted)^2))
rmse_prophet <- sqrt(mean((actual - prophet_predicted)^2))

# 성능 비교 결과 출력
cat("ARIMA - MAE:", mae_arima, " RMSE:", rmse_arima, "\n")
cat("PROPHET - MAE:", mae_prophet, "RMSE:", rmse_prophet, "\n")

