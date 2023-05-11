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
"reshape2", "psych", "gtsummary", "readxl", "MASS") 
ipak(pkg)

##############################################################################################

# 1. data load
data <- data <- read.csv("final_data.csv", fileEncoding = "cp949")

data$reg_date <- as.Date(data$reg_date)
data$datetime <- as.POSIXct(data$datetime)

#str(data)
min(data$reg_date) # 2022-01-01
max(data$reg_date) # 2023-04-30

data$day_of_reg <- factor(data$day_of_reg, levels = c('월요일', '화요일', '수요일', '목요일','금요일', '토요일','일요일'))

# train/test 데이터 분리
train_data <- data %>% filter(datetime <= "2022-12-31" & datetime >= '2022-01-02')
test_data <- data %>% filter(datetime >= "2023-01-01" & datetime <= "2023-04-30")

# 지역 목록 추출
locations <- unique(data$pick_rgn2_nm)
locations


# 각 지역별로 모델 생성 및 예측 결과 저장
arima_results <- list()

for (location in locations) {
    train_set <- train_data  %>% filter(pick_rgn2_nm == !!location)
    test_set <- test_data  %>%  filter(pick_rgn2_nm == !!location)

    # ARIMA model
    ts_train <- ts(train_set[,c("rider_cnt_2", "is_rain")], frequency = 24)
    ts_test <- ts(test_set[,c("rider_cnt_2", "is_rain")], frequency = 24)

    arima_model <- auto.arima(ts_train[,"rider_cnt_2"], xreg = ts_train[,"is_rain"], seasonal = TRUE)
    arima_model_forecast <- forecast(arima_model, xreg = ts_test[,"is_rain"])
    arima_results[[as.character(location)]] <- data.frame(test_set$datetime, test_set$rider_cnt_2, arima_model_forecast$mean, arima_model_forecast$lower[,2], arima_model_forecast$upper[,2])

}
write.csv(arima_results, "arima_result.csv", row.names = FALSE, fileEncoding = "cp949")


