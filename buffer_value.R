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

##############################################################################
# data load
df <- read.csv("prediction_results_test_set.csv", fileEncoding = "cp949")
head(df)

df <- df  %>% mutate(linear_x = round(y_test/y_pred_test_LinearRegression, 1),
                     ridge_x = round(y_test/y_pred_test_Ridge,1),
                     lasso_x = round(y_test/y_pred_test_Lasso,1),
                     rf_x = round(y_test/y_pred_test_RandomForestRegressor,1),
                     dt_x = round(y_test/y_pred_test_DecisionTreeRegressor,1),
                    LGBM_x = round(y_test/y_pred_test_LGBMRegressor,1))
summary(df)

# df$linear_x2 <- cut(df$linear_x, breaks=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5, 3, 3.5, Inf), labels=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5,3, 3.5), right=FALSE) 
# df$ridge_x2 <- cut(df$ridge_x, breaks=c(-Inf, 0.5, 1, 1.25, 1.5, 2, 2.5, 3, 3.5, Inf), labels=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5,3, 3.5), right=FALSE) 
# df$lasso_x2 <- cut(df$lasso_x, breaks=c(-Inf, 0.5, 1, 1.25, 1.5, 2, 2.5, 3, 3.5, Inf), labels=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5,3, 3.5), right=FALSE) 
# df$rf_x2 <- cut(df$rf_x, breaks=c(-Inf, 0.5, 1, 1.25, 1.5, 2, 2.5, 3, 3.5, Inf), labels=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5,3, 3.5), right=FALSE) 
# df$dt_x2 <- cut(df$dt_x, breaks=c(-Inf, 0.5, 1, 1.25, 1.5, 2, 2.5, 3, 3.5, Inf), labels=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5,3, 3.5), right=FALSE) 
# df$LGBM_x2 <- cut(df$LGBM_x, breaks=c(-Inf, 0.5, 1, 1.25, 1.5, 2, 2.5, 3, 3.5, Inf), labels=c(0, 0.5, 1, 1.25, 1.5, 2, 2.5,3, 3.5), right=FALSE) 

var <-  c('linear_x', 'ridge_x','lasso_x','rf_x','dt_x','LGBM_x')
df[,var]<- lapply(df[,var], factor)
str(df)

table(df$linear_x2)
table(df$ridgex2)
table(df$lasso_x2)
table(df$rf_x2)
table(df$dt_x2)
table(df$LGBM_x2)


linear  <- df  %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain, is_holiday) %>% 
summarise(most_linear_x = names(which.max(table(linear_x))),
         percent_linear = max(table(linear_x)) / n() * 100)

lgbm <- df  %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain, is_holiday) %>% 
summarise(most_lgbm_x = names(which.max(table(LGBM_x))),
         percent_lgbm = max(table(LGBM_x)) / n() * 100)

rf <- df  %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain, is_holiday) %>% 
summarise(most_rf_x = names(which.max(table(rf_x))),
         percent_rf = max(table(rf_x)) / n() * 100)

dt <- df  %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain, is_holiday) %>% 
summarise(most_df_x = names(which.max(table(dt_x))),
         percent_df = max(table(dt_x)) / n() * 100)


write.csv(lgbm, "lgbm_buffer.csv", row.names = FALSE, fileEncoding = "cp949")
write.csv(linear, "linear_buffer.csv", row.names = FALSE, fileEncoding = "cp949")
write.csv(rf, "rf_buffer.csv", row.names = FALSE, fileEncoding = "cp949")
write.csv(dt, "dt_buffer.csv", row.names = FALSE, fileEncoding = "cp949")


###################################### 
# 보정계수 적절성 확인
data <- read.csv("prediction_results_test_set.csv", fileEncoding = "cp949")
data$datetime <- as.POSIXct(data$datetime)

data <- data %>% filter(datetime >= '2023-04-27')
dim(data) #5450

data <- data  %>% 
mutate(buffer_linear = round(y_test/y_pred_test_LinearRegression,1),
       buffer_LGBM = round(y_test/y_pred_test_LGBMRegressor,1))

summary(data)


data <- data  %>% 
mutate(fix_linear = ifelse(is_rain == 0 ,y_pred_test_LinearRegression*1.05, y_pred_test_DecisionTreeRegressor*0.95),
        fix_LGBM = ifelse(is_rain == 0 , y_pred_test_LGBMRegressor*1.05, y_pred_test_LGBMRegressor*0.95))

# data <- data %>% 
# mutate(error_linear_raw = y_test - y_pred_test_LinearRegression,
#        error_linear_buffer = y_test - fix_linear,
#        error_LGBM_raw = y_test - y_pred_test_LGBMRegressor,
#        error_LGBM_buffer = y_test - fix_LGBM)

summary(data)

#  MAE 확인 
mae <- function(actual, predicted){
  mean(abs(actual - predicted))
}

mae_raw  <- mae(data$y_test, data$y_pred_test_LGBMRegressor)
mae_buffer <- mae(data$y_test, data$fix_LGBM)

mae_raw
mae_buffer


# 시간대별 확인
result_hour <-  aggregate(cbind(abs(data$y_test - data$y_pred_test_LGBMRegressor), 
                               abs(data$y_test- data$fix_LGBM)),
                        by = list(data$hour_reg), 
                        FUN = mean)
result_hour

# 기상, 평시 
result_rain <-  aggregate(cbind(abs(data$y_test - data$y_pred_test_LGBMRegressor), 
                               abs(data$y_test- data$fix_LGBM)),
                    by = list(data$is_rain), 
                    FUN = mean)
result_rain

# 지역별
result_loca <-  aggregate(cbind(abs(data$y_test - data$y_pred_test_LGBMRegressor), 
                               abs(data$y_test- data$fix_LGBM)),
                    by = list(data$pick_rgn2_nm), 
                    FUN = mean)
result_loca
