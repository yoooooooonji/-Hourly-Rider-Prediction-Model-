# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS") # nolint
ipak(pkg)

##########################################################################################################################################################

# data load
predict <- read.csv("prediction_results_test_set.csv", fileEncoding = "cp949")

head(predict)
predict$datetime <- as.POSIXct(predict$datetime)
min(predict$datetime)
max(predict$datetime)

predict <- predict  %>% mutate(reg_date = as.Date(predict$datetime))

library(Metrics)

# 일자별
result_day <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test)),
                        by = list(predict$reg_date), 
                        FUN = mean)
result_day

# 시간대별 
result_hour <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test)),
                        by = list(predict$hour_reg), 
                        FUN = mean)
result_hour

# 지역별
result_rgn2 <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test)),
                        by = list(predict$pick_rgn2_nm),
                    FUN = mean)
result_rgn2

# 지역별, 시간대별 
result_total <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test)),
                    by = list(predict$pick_rgn2_nm, predict$hour_reg), 
                    FUN = mean)
result_total


# 기상
result_rain <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test)),
                    by = list(predict$is_rain), 
                    FUN = mean)
result_rain
