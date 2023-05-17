# dataset 
# 0. install packages
options(scipen = 10)

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", 
"reshape2", "psych", "gtsummary", "readxl", "MASS", "forecast") 
ipak(pkg)

##########################################################################################################################################################
# data load
train_data <- read.csv("final_data.csv", fileEncoding = "cp949")
train_data$reg_date <- as.Date(train_data$reg_date)
train_data$datetime <- as.POSIXct(train_data$datetime)
train_data$day_of_reg <- factor(train_data$day_of_reg, levels = c('월요일', '화요일', '수요일', '목요일','금요일', '토요일','일요일'))

# train_data <- train_data  %>% filter(pick_rgn2_nm %in% c('서초구'))
locations <- unique(train_data$pick_rgn2_nm)

# 새로운 test 데이터 프레임 생성
test_data <- data.frame(datetime = seq(as.POSIXct("2023-05-01 00:00:00"), as.POSIXct("2023-05-11 23:00:00"), by = "hour"))
test_data <- expand.grid(datetime = test_data$datetime, pick_rgn2_nm = locations)
test_data <- test_data  %>% 
mutate(is_rain = ifelse(as.Date(datetime) %in% c('2023-05-04', '2023-05-05'), 1,0),
       rider_cnt_2 = 0)

# 각 지역별로 모델 생성 및 예측 결과 저장
arima_results <- list()

for (location in locations) {
    train_set <- train_data  %>% filter(pick_rgn2_nm == !!location)
    test_set <- test_data  %>%  filter(pick_rgn2_nm == !!location)

    # ARIMA model
    ts_train <- ts(train_set[,c("rider_cnt_2", "is_rain")], frequency = 24)
    ts_test <- ts(test_set[,c("rider_cnt_2","is_rain")], frequency = 24) 

    arima_model <- auto.arima(ts_train[,"rider_cnt_2"], xreg = ts_train[,"is_rain"], seasonal = TRUE)
    arima_model_forecast <- forecast(arima_model, xreg = ts_test[, "is_rain"])
    arima_result_location <- data.frame(datetime = test_set$datetime,
                                        pick_rgn2_nm = location,
                                        forecast = arima_model_forecast$mean,
                                        lower = arima_model_forecast$lower[,2],
                                        upper = arima_model_forecast$upper[,2])
    arima_results[[as.character(location)]] <- arima_result_location
}

arima_results_all <- bind_rows(arima_results)
arima_results_all <- arima_results_all  %>% filter(hour(datetime) %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,0,1,2))
table(arima_results_all$pick_rgn2_nm)

write.csv(arima_results_all, "arima_result.csv", row.names = FALSE, fileEncoding = "cp949") 



