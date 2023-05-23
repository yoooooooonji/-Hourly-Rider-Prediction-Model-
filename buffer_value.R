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

##########################################################################################################################################################################################################################################
# 보정계수 값 산출 
df <- read.csv("prediction_results_test_set.csv", fileEncoding = "cp949")
head(df)

buf_df <- df %>% 
filter(as.Date(datetime) >= '2023-04-09' & '2023-05-08' >= as.Date(datetime))

test_df <- df  %>% 
filter(as.Date(datetime) >= '2023-05-08')

min(buf_df$datetime) # 2023-04-09
max(buf_df$datetime) # 2023-05-08

min(test_df$datetime) #2023-05-08
max(test_df$datetime) #2023-05-21


buffer_table <- buf_df %>% 
group_by(pick_rgn2_nm, is_rain, is_holiday) %>% 
summarise(buffer_value = mean(y_test/y_pred_test_avg))


test_df <- left_join (test_df, buffer_table, by = c("pick_rgn2_nm", "is_rain","is_holiday"))
test_df <- test_df  %>% 
mutate(y_pred_test_buf = y_pred_test_avg * buffer_value)


# df$linear_x2 <- cut(df$linear_x, breaks=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5, 3, 3.5, Inf), labels=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5,3, 3.5), right=FALSE) 

#################################################################################################################################################################################################################################################
# 보정계수 적절성 확인
library(Metrics)

mae_lasso <- mae(test_df$y_test, test_df$y_pred_test_Lasso)
mae_lgbm <- mae(test_df$y_test, test_df$y_pred_test_LGBMRegressor)
mae_rf <- mae(test_df$y_test, test_df$y_pred_test_RandomForestRegressor)
mae_raw  <- mae(test_df$y_test, test_df$y_pred_test_avg)
mae_buffer <- mae(test_df$y_test, test_df$y_pred_test_buf)

mae_lasso 
mae_lgbm 
mae_rf 
mae_raw 
mae_buffer 


# 시간대별 확인
result_hour <-  aggregate(cbind(abs(test_df$y_test - test_df$y_pred_test_avg), 
                               abs(test_df$y_test - test_df$y_pred_test_buf)),
                        by = list(test_df$hour_reg), 
                        FUN = mean)
result_hour

# 기상, 평시 
result_rain <- aggregate(cbind(abs(test_df$y_test - test_df$y_pred_test_avg), 
                               abs(test_df$y_test - test_df$y_pred_test_buf)),
                    by = list(test_df$is_rain), 
                    FUN = mean)
result_rain

# 지역별
result_loca <-  aggregate(cbind(abs(test_df$y_test - test_df$y_pred_test_avg), 
                               abs(test_df$y_test - test_df$y_pred_test_buf)),
                    by = list(test_df$pick_rgn2_nm), 
                    FUN = mean)
result_loca


# 일자별 확인
test_df <- test_df  %>% 
mutate(reg_date = as.Date(datetime))

result_day <- aggregate(cbind(abs(test_df$y_test - test_df$y_pred_test_avg), 
                               abs(test_df$y_test - test_df$y_pred_test_buf)),
                    by = list(test_df$reg_date), 
                    FUN = mean)
result_day
