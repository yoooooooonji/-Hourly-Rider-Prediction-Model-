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

dim(predict)
head(predict)
predict$datetime <- as.POSIXct(predict$datetime)
predict <- predict  %>% mutate(reg_date = as.Date(predict$datetime))
min(predict$datetime)
max(predict$datetime)

##########################################################################################################################################################
saturday_data <- predict %>% filter(day_of_reg == '토요일')

predict <- predict  %>% 
mutate(reg_date_change = case_when(day_of_reg == '월요일' ~ reg_date+5,
                                   day_of_reg == '화요일' ~ reg_date+4,
                                   day_of_reg == '수요일' ~ reg_date+3,
                                   day_of_reg == '목요일' ~ reg_date+2,
                                   day_of_reg == '금요일' ~ reg_date+1,
                                   TRUE ~ reg_date))

predict <- left_join(predict, saturday_data, by = c("reg_date_change" = "reg_date", "pick_rgn2_nm" = "pick_rgn2_nm", "hour_reg" = "hour_reg"))

#predict <- predict  %>% filter(!is.na(reg_date_change.y))

# 
predict <- predict  %>% 
mutate(pred_final = case_when(is_holiday.x == 1 & day_of_reg.x %in% c("월요일","화요일", "수요일","목요일","금요일") ~ y_pred_test_avg1.y,
                              TRUE ~ y_pred_test_avg1.x))

                              
result_final <- aggregate(cbind(abs(predict$pred_final - predict$y_test.x), 
                               abs(predict$y_pred_test_avg1.x - predict$y_test.x)),
                        by = list(predict$group_s.x), 
                        FUN = mean)
result_final

result_mape <- aggregate(cbind(abs((predict$pred_final - predict$y_test.x) / predict$y_test.x) * 100, 
                               abs((predict$y_pred_test_avg1.x - predict$y_test.x) / predict$y_test.x) * 100),
                        by = list(predict$group_s.x), 
                        FUN = mean)

result_mape

#write.csv(predict, "model_result_평공_보정.csv", fileEncoding = "cp949", row.names = FALSE)



mae(predict$pred_final, predict$y_test.x)
mae(predict$y_pred_test_avg.x, predict$y_test.x)

mape(predict$pred_final, predict$y_test.x)
mape(predict$y_pred_test_avg.x, predict$y_test.x)



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
  mutate(
         median_model = apply(dplyr::select(., c(y_pred_test_Lasso, y_pred_test_LGBMRegressor, y_pred_test_RandomForestRegressor)), 1, median, na.rm = TRUE),
        weight1_model = ((y_pred_test_Lasso*0.968592) + (y_pred_test_LGBMRegressor*0.968592) + (y_pred_test_RandomForestRegressor*0.973041)) / (0.968592+0.968592+0.973041),
        weight2_model = ((y_pred_test_Lasso*10.036040) + (y_pred_test_LGBMRegressor* 9.710589) + (y_pred_test_RandomForestRegressor*9.081697 )) / (10.036040+9.710589+9.081697))


# MAE, RMSE, MAPE 계산하기
# mae
mae(predict$y_test, predict$y_pred_test_Lasso) 
mae(predict$y_test, predict$y_pred_test_LGBMRegressor)
mae(predict$y_test, predict$y_pred_test_RandomForestRegressor) 
mae(predict$y_test, predict$y_pred_test_avg)

mae(predict$y_test,predict$median_model) 
mae(predict$y_test,predict$weight1_model) 
mae(predict$y_test,predict$weight2_model)

#rmse 
rmse(predict$y_test, predict$y_pred_test_Lasso) 
rmse(predict$y_test, predict$y_pred_test_LGBMRegressor) 
rmse(predict$y_test, predict$y_pred_test_RandomForestRegressor) 

rmse(predict$y_test, predict$y_pred_test_avg)

rmse(predict$y_test,predict$median_model) 
rmse(predict$y_test,predict$weight1_model) 
rmse(predict$y_test,predict$weight2_model) 

#mape
mape(predict$y_test, predict$y_pred_test_Lasso)*100 
mape(predict$y_test, predict$y_pred_test_LGBMRegressor)*100 
mape(predict$y_test, predict$y_pred_test_RandomForestRegressor)*100  

mape(predict$y_test, predict$y_pred_test_avg)*100

mape(predict$y_test,predict$median_model)*100  
mape(predict$y_test,predict$weight1_model)*100  
mape(predict$y_test,predict$weight2_model)*100  

str(predict)

# 요일, 기상
result <-  aggregate(cbind(abs(predict$y_pred_test_LinearRegression - predict$y_test), 
                               abs(predict$y_pred_test_Lasso - predict$y_test),
                               abs(predict$y_pred_test_LGBMRegressor - predict$y_test),
                               abs(predict$y_pred_test_RandomForestRegressor - predict$y_test),
                               abs(predict$y_test - predict$y_pred_test_avg),
                               abs(predict$y_test - predict$median_model),
                               abs(predict$y_test - predict$weight1_model),
                               abs(predict$y_test - predict$weight2_model)),
                    by = list(predict$is_holiday,predict$is_rain), 
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

# write.csv(predict, "prediction_results_test_set_model_add.csv", fileEncoding = "cp949", row.names = FALSE)



###############################################################################################################################
# 평일 - 공휴일 - 기상인 날 -> 그 주 주말 데이터 사용 

head(predict)

test <- predict  %>% filter(is_rain == 1 & is_holiday ==1 & day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일'))
dim(test)

test$date <- as.Date(test$datetime)
n_distinct(test$date) # 2023-05-05 


predict <- predict %>% 
mutate(date = as.Date(datetime))

test <- predict %>% filter(date == '2023-05-05' | date == '2023-05-06')

test <- test  %>% 
group_by(pick_rgn2_nm, hour_reg) %>% 
mutate(y_pred_2 = ifelse(date == '2023-05-05' ,lead(y_pred_test_avg), y_pred_test_avg))

summary(test$y_pred_2)
summary(test$y_pred_test_avg)

ss <- test  %>% filter(date == '2023-05-05')

mae(ss$y_test, ss$y_pred_test_avg) #63.03
mae(ss$y_test, ss$y_pred_2) #26.31 

rmse(ss$y_test, ss$y_pred_test_avg) # 84.42
rmse(ss$y_test, ss$y_pred_2) # 35.96

mape(ss$y_test, ss$y_pred_test_avg) #23
mape(ss$y_test, ss$y_pred_2) # 10


