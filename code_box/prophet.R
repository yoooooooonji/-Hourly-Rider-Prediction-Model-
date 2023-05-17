# library 불러오기 
library(dplyr)
library(lubridate)
library(forecast)
library(prophet)

# 데이터 불러오기
data <- read.csv("final_data.csv", fileEncoding = "cp949")

data <- data  %>% filter(pick_rgn2_nm %in% c("강남구", "서초구"))

# train/test 데이터 분리
train_data <- data %>% filter(datetime <= "2022-12-31" & datetime >= '2022-01-02')
test_data <- data %>% filter(datetime >= "2023-01-01" & datetime <= "2023-04-30")

# 지역 목록 추출
locations <- unique(data$pick_rgn2_nm)

# 각 지역별로 모델 생성 및 예측 결과 저장
arima_results <- list()
prophet_results <- list()

for (location in locations) {
  data_location_train <- train_data %>% filter(pick_rgn2_nm == !!location)
  data_location_test <- test_data %>% filter(pick_rgn2_nm == !!location)

  # Prophet 모델
  # 이벤트 여부를 고려한 PROPHET 데이터 전처리
  prophet_data_location_train <- data_location_train[c("datetime", "rider_cnt_2", "is_rain")] %>% 
    rename(ds = datetime, y = rider_cnt_2)

  holidays <- prophet_data_location_train %>% 
    filter(is_rain == 1) %>% 
    mutate(holiday = "rain") %>% 
    dplyr::select(ds, holiday)

 prophet_data_location_train$is_rain <- as.numeric(prophet_data_location_train$is_rain)

# # PROPHET 모델 생성 (이벤트 여부 포함)
 prophet_model_location <- prophet(prophet_data_location_train, holidays = holidays, yearly.seasonality = TRUE)
 prophet_model_location <- add_country_holidays(prophet_model_location, country_name =  'KR')
 prophet_model_location <- add_regressor(prophet_model_location, 'is_rain')
 prophet_model_location <- fit(prophet_model_location, prophet_data_location_train)

# # PROPHET 모델 예측 (이벤트 여부 포함)
 future_location <- make_future_dataframe(prophet_model_location, periods = nrow(data_location_test), freq = "hour")
 prophet_forecast_location <- predict(prophet_model_location, future_location)
 prophet_results[[as.character(location)]] <- data.frame(pick_rgn2_nm = location, forecast = prophet_forecast_location$yhat, lower = prophet_forecast_location$yhat_lower, upper = prophet_forecast_location$yhat_upper)

# ARIMA 모델 결과, Prophet 모델 결과 합치기
arima_results[[as.character(location)]] <- arima_forecast_location
prophet_results[[as.character(location)]] <- prophet_forecast_location

}

write.csv(result_df, "result_prophet.csv", row.names = FALSE)