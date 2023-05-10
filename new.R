# 새로운 목록 생성
new_dates <- seq(as.POSIXct("2023-05-11 00:00:00"), as.POSIXct("2023-05-18 23:00:00"), by="hour")

# 데이터 불러오기
data <- read.csv("final_data.csv", fileEncoding = "cp949")

# train/test 데이터 분리
train_data <- data %>% filter(datetime <= "2022-12-31" & datetime >= '2022-01-02')
test_data <- data %>% filter(datetime >= "2023-01-01" & datetime <= "2023-04-30")

# 지역 목록 추출
locations <- unique(data$pick_rgn2_nm)

# 각 지역별로 모델 생성 및 예측 결과 저장
arima_results <- list()

for (location in locations) {
  data_location_train <- train_data %>% filter(pick_rgn2_nm == !!location)
  data_location_test <- test_data %>% filter(pick_rgn2_nm == !!location)

  # ARIMA 모델
  ts_data_location_train <- ts(data_location_train[, c("rider_cnt_2", "is_rain")], frequency = 24) 
  ts_data_location_test <- ts(data_location_test[, c("rider_cnt_2", "is_rain")], frequency = 24) 

  arima_model_location <- auto.arima(ts_data_location_train[,"rider_cnt_2"], xreg = ts_data_location_train[,"is_rain"])
  arima_forecast_location <- forecast(arima_model_location, xreg = ts_data_location_test[, "is_rain"])
  
  # 새로운 목록에 대한 예측
  new_data_location <- data.frame(datetime = new_dates, is_rain = 0)
  new_data_location$datetime <- as.POSIXct(new_data_location$datetime, format="%Y-%m-%d %H:%M:%S")
  ts_new_data_location <- ts(new_data_location[, c("is_rain")], frequency = 24)
  
  arima_forecast_new_location <- forecast(arima_model_location, xreg = ts_new_data_location[, "is_rain"])
  
  # ARIMA 모델 결과 데이터 프레임 생성
  arima_result_location <- data.frame(datetime = c(data_location_test$datetime, new_dates),
                                      rider_cnt_2 = c(arima_forecast_location$mean, arima_forecast_new_location$mean),
                                      lower = c(arima_forecast_location$lower[,2], arima_forecast_new_location$lower[,2]),
                                      upper = c(arima_forecast_location$upper[,2], arima_forecast_new_location$upper[,2]),
                                      pick_rgn2_nm = location)
  
  arima_results[[as.character(location)]] <- arima_result_location

}

result_df <- arima_results[[1]]
for (i in 2:length(locations)) {
  result_df <- rbind(result_df, arima_results[[i]])
}

write.csv(result_df, "result.csv", row.names = FALSE)
