#서초구
sch_data <- data  %>% filter(pick_rgn2_nm == '서초구')
dim(sch_data) #11631
min(sch_data$datetime)
max(sch_data$datetime)

# 3. ARIMA 모델 훈련 및 예측
# 이벤트 여부를 고려한 시계열 데이터로 변환
ts_data_event <- ts(sch_data[, c("rider_cnt_2", "is_rain")], frequency = 24) # 시간대별 방문자 수와 이벤트 여부를 시계열 데이터로 변환 (24시간 단위)

# ARIMA 모델 훈련 (이벤트 여부 포함)
arima_model_event <- auto.arima(ts_data_event[, "rider_cnt_2"], xreg = ts_data_event[, "is_rain"])

# ARIMA 모델 예측 (이벤트 여부 포함)
arima_forecast_event <- forecast(arima_model_event, h=24, xreg = tail(ts_data_event[, "is_rain"], 24*7)) # 다음 24시간 예측, 마지막 24시간의 이벤트 여부를 외생 변수로 사용


# 4. PROPHET 모델 훈련 및 예측

# 이벤트 여부를 고려한 PROPHET 데이터 전처리
prophet_data_event <- sch_data[c("datetime", "rider_cnt_2", "is_rain")] %>% 
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
actual <- tail(sch_data$rider_cnt_2, 24*7) # 실제 값 추출 (마지막 1주일)

# ARIMA 예측값 추출
arima_predicted <- arima_forecast_event$mean

# PROPHET 예측값 추출
prophet_predicted <- tail(prophet_forecast_event$yhat, 24*7)

# MAE 계산
mae_arima <- mean(abs(actual - arima_predicted))
mae_prophet <- mean(abs(actual - prophet_predicted))

# RMSE 계산
rmse_arima <- sqrt(mean((actual - arima_predicted)^2))
rmse_prophet <- sqrt(mean((actual - prophet_predicted)^2))

# 성능 비교 결과 출력
cat("ARIMA - MAE:", mae_arima, " RMSE:", rmse_arima, "\n")  # mae 21.67, rmse 34.27
cat("PROPHET - MAE:", mae_prophet, "RMSE:", rmse_prophet, "\n") # 26.30, rmse 34.98
