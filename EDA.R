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

##########################################################################################################################################################
# 1. data load
data <- read.csv("combined_data.csv", fileEncoding = "cp949")

data$reg_date <- as.Date(data$reg_date)
data$datetime <- as.POSIXct(data$datetime)

#str(data)
min(data$reg_date) # 2022-01-01
max(data$reg_date) # 2023-04-30

data$day_of_reg <- factor(data$day_of_reg, levels = c('월요일', '화요일', '수요일', '목요일','금요일', '토요일','일요일'))

table(data$day_of_reg, data$month)
dim(data) #290,775
table(data$pick_rgn2_nm) 


# 2. 이상치 여부 파악

# TSL 분해 방법 (절단이 안되었을 경우에 사용 가능)
# library(tsibble)
# library(timetk)

# grouped_data <- data %>% group_by(pick_rgn2_nm)
# grouped_data  %>% plot_anomaly_diagnostics(datetime, rider_cnt)
# anomaly_data <- grouped_data %>%
# tk_anomaly_diagnostics(datetime, rider_cnt)
# table(anomaly_data$anomaly) #1016 
# test <- left_join(grouped_data, anomaly_data, by = c("datetime" = "datetime", 'rider_cnt'= "observed"))
# test <- test  %>% filter(anomaly == 'Yes')
# table(test$is_rain) # 0 : 710, 1:225
# table(test$pick_rgn2_nm.x) # 강남구 205
# table(test$hour_reg) # 11 : 402, 17 : 182 

# IQR Rule-based Anomaly Detection 
data <- data %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain) %>% 
mutate(q1 = quantile(rider_cnt, 0.25),
      q3 = quantile(rider_cnt, 0.75),
      IQR1.5 = 1.5*(quantile(rider_cnt, 0.75) - quantile(rider_cnt, 0.25)))

summary(data)

data <- data  %>% 
mutate(outlier = case_when (is_rain == 0 & (q1 - IQR1.5 > rider_cnt | rider_cnt > q3 + IQR1.5) ~ 1,
                            TRUE ~ 0))

table(data$outlier) # 8,981

# outlier median 값으로 대체 
data <- data %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg)  %>%
mutate(rider_cnt_2 = case_when(outlier == 1 ~ median(rider_cnt),
                              TRUE ~ rider_cnt))

table(data$outlier, data$day_of_reg) #월, 수,일 많음


# 3. 자기상관성 확인
# ACF : 자기상관 함수
# PACF : 부분자기상관 함수

library(timetk)
acf_result <- data  %>% group_by(pick_rgn2_nm) %>% 
tk_acf_diagnostics(datetime, rider_cnt_2, .lags=200)

tb_acf <- acf_result  %>% group_by(lag) %>% summarise(mean_acf = mean(ACF), mean_pacf = mean(PACF))


# 4. 그래프
graph1 <- data  %>% 
ggplot(aes(as.factor(hour_reg),rider_cnt_2)) +
geom_boxplot() +
labs(title = '시간대별 라이더수 분포', x = '시간', y = '라이더수')
graph1

graph2 <- data %>% 
ggplot(aes(as.factor(month), rider_cnt_2)) +
geom_boxplot() +
labs(title = '월별 라이더수 분포', x = '시간', y = '라이더수')

graph2

#w-1,2,3,4 동일 요일 동시간대 주문수/라이더수
library(zoo)
data <- data %>%
  arrange(datetime, pick_rgn2_nm) %>% 
  group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain) %>% 
  mutate(rider_cnt_w_1 = na.locf(rider_cnt_2, n=1),
         rider_cnt_w_2 = na.locf(rider_cnt_2, n=2),
         rider_cnt_w_3 = na.locf(rider_cnt_2, n=3),
         rider_cnt_w_4 = na.locf(rider_cnt_2, n=4),
         order_cnt_w_1 = na.locf(order_cnt, n=1),
         order_cnt_w_2 = na.locf(order_cnt, n=2),
         order_cnt_w_3= na.locf(order_cnt, n=3),
         order_cnt_w_4 = na.locf(order_cnt, n=4))

# 그래프 저장
# ggsave("강남구 월별 (raw).png", raw1)
# ggsave("강남구 월별 (cleaned).png", ch1)
# ggsave("강남구 시간대별 (raw).png", raw2)
# ggsave("강남구 시간대별 (cleaned).png", ch2)

write.csv(data, "final_data.csv", row.names = FALSE, fileEncoding = "cp949")
