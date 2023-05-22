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
predict <- predict  %>% mutate(reg_date = as.Date(predict$datetime))
min(predict$datetime)
max(predict$datetime)

# predict <- predict  %>% mutate(day_of_reg = weekdays(datetime))
# table(predict$day_of_reg)

# predict <- predict  %>% mutate(day_of_reg2 = case_when(day_of_reg %in% c("월요일", "화요일","수요일","목요일","금요일") ~"주중",
#                                                       TRUE ~ "주말"))


library(Metrics)
mae(predict$y_test, predict$y_pred_test_LinearRegression)
mae(predict$y_test, predict$y_pred_test_Ridge)
mae(predict$y_test, predict$y_pred_test_Lasso)
mae(predict$y_test, predict$y_pred_test_LGBMRegressor)
mae(predict$y_test, predict$y_pred_test_RandomForestRegressor)
mae(predict$y_test, predict$y_pred_test_DecisionTreeRegressor)

# 일자별
result_day <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                              abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test)),
                        by = list(predict$reg_date), 
                        FUN = mean)
result_day

# 시간대별 
result_hour <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                              abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test)),
                        by = list(predict$hour_reg), 
                        FUN = mean)
result_hour

# 지역별
result_rgn2 <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                              abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test)),
                        by = list(predict$pick_rgn2_nm),
                    FUN = mean)
result_rgn2

# 지역별, 시간대별 
result_total <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                              abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test)),
                    by = list(predict$pick_rgn2_nm, predict$hour_reg), 
                    FUN = mean)
result_total

#평일
result_normal <-  aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                              abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test)),
                    by = list(predict$is_rain, predict$is_holiday), 
                    FUN = mean)
result_normal

# 기상 
result_rain <- aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                              abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test)),
                    by = list(predict$is_rain), 
                    FUN = mean)
result_rain

# 요일, 기상
result <-  aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                              abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test)),
                    by = list(predict$day_of_reg2,predict$is_rain, predict$is_holiday), 
                    FUN = mean)
result


##########################################################################################################################################################
# 예측값 앙상블
str(predict)

predict <- predict %>%
  mutate(mean_model = rowMeans(dplyr::select(., c(y_pred_test_Lasso, y_pred_test_LGBMRegressor, y_pred_test_RandomForestRegressor)), na.rm = TRUE),
         median_model = apply(dplyr::select(., c(y_pred_test_Lasso, y_pred_test_LGBMRegressor, y_pred_test_RandomForestRegressor)), 1, median, na.rm = TRUE),
        weight1_model = ((y_pred_test_Lasso*0.978647) + (y_pred_test_LGBMRegressor*0.978647) + (y_pred_test_RandomForestRegressor*0.979338)) / (0.978647+0.978647+0.979338),
        weight2_model = ((y_pred_test_Lasso*9.526898) + (y_pred_test_LGBMRegressor*8.691870) + (y_pred_test_RandomForestRegressor*8.564885)) / (9.526898+8.691870+8.564885))


# MAE, RMSE, MAPE 계산하기
# mae
mae(predict$y_test, predict$y_pred_test_Lasso) #15.93
mae(predict$y_test, predict$y_pred_test_LGBMRegressor) #15.45
mae(predict$y_test, predict$y_pred_test_RandomForestRegressor) #14.93

mae(predict$y_test,predict$mean_model) #14.44
mae(predict$y_test,predict$median_model) #14.57
mae(predict$y_test,predict$weight1_model) #14.44
mae(predict$y_test,predict$weight2_model) # 14.46

#rmse 
rmse(predict$y_test, predict$y_pred_test_Lasso) #23.30
rmse(predict$y_test, predict$y_pred_test_LGBMRegressor) #22.68
rmse(predict$y_test, predict$y_pred_test_RandomForestRegressor) #22.31

rmse(predict$y_test,predict$mean_model) #21.49
rmse(predict$y_test,predict$median_model) #21.75
rmse(predict$y_test,predict$weight1_model) # 21.49
rmse(predict$y_test,predict$weight2_model) # 21.51

#mape
mape(predict$y_test, predict$y_pred_test_Lasso)*100 #9.52
mape(predict$y_test, predict$y_pred_test_LGBMRegressor)*100 #8.69
mape(predict$y_test, predict$y_pred_test_RandomForestRegressor)*100  #8.56

mape(predict$y_test,predict$mean_model)*100  # 8.21
mape(predict$y_test,predict$median_model)*100  # 8.27
mape(predict$y_test,predict$weight1_model)*100  # 8.21
mape(predict$y_test,predict$weight2_model)*100  # 8.22



# 요일, 기상
result <-  aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                               abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test),
                               abs(predict$y_test - predict$mean_model),
                               abs(predict$y_test - predict$median_model),
                               abs(predict$y_test - predict$weight1_model),
                               abs(predict$y_test - predict$weight2_model)),
                    by = list(predict$day_of_reg2,predict$is_rain, predict$is_holiday), 
                    FUN = mean)
result

#
result_holiday <-  aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Ridge - predict$y_test),
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                               abs(predict$y_pred_test_DecisionTreeRegressor - predict$y_test),
                               abs(predict$y_test - predict$mean_model),
                               abs(predict$y_test - predict$median_model),
                               abs(predict$y_test - predict$weight1_model),
                               abs(predict$y_test - predict$weight2_model)),
                    by = list(predict$is_rain, predict$is_holiday), 
                    FUN = mean)
result_holiday

write.csv(predict, "prediction_results_test_set_model_add.csv", fileEncoding = "cp949", row.names = FALSE)
