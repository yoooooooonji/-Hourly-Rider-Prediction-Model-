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
min(data$reg_date) # 2022-01-29
max(data$reg_date) # 2023-05-25

table(data$group_s)


# correlation
# 평일 = 공휴일 - C
# 평일 = 공휴일 = 기상 - D
# 주말 = 공휴일 - G
# 주말 = 공휴일 = 기상 H

df_c <- data  %>% filter(group_s == "C")
df_d <- data  %>% filter(group_s == "D")
df_g <- data  %>% filter(group_s == "G")
df_h <- data  %>% filter(group_s == "H")

df_a <- data  %>%  filter(group_s == "A")
df_b <- data  %>% filter(group_s == "B")
df_e <- data  %>%  filter(group_s == "E")
df_f <- data  %>% filter(group_s == "F")

max(df_a$reg_date) #2023-05-25
max(df_b$reg_date) #2023-05-18
max(df_e$reg_date) # 2023-05-21
max(df_f$reg_date) #2023-05-06


min(df_c$reg_date) 
max(df_c$datetime) #2023-05-01

min(df_d$reg_date) 
max(df_d$datetime) #2023-05-05

min(df_g$reg_date)
max(df_g$datetime) #2023-01-22

min(df_h$reg_date)
max(df_h$datetime) #2022-12-25

a_filter <- df_a  %>% filter(reg_date == '2023-05-25' & pick_rgn2_nm == '용산구')
b_filter <- df_b  %>% filter(reg_date == '2023-05-18' & pick_rgn2_nm == '용산구')
c_filter <- df_c  %>% filter(reg_date == '2023-05-01' & pick_rgn2_nm == '용산구')
d_filter <- df_d %>% filter(reg_date == '2023-05-05' & pick_rgn2_nm ==  '용산구')
e_filter <- df_e  %>% filter(reg_date == '2023-05-20' & pick_rgn2_nm == '용산구')
f_filter <- df_f  %>% filter(reg_date == '2023-05-06' & pick_rgn2_nm == '용산구')
g_filter <- df_g  %>% filter(reg_date == '2023-01-22' & pick_rgn2_nm == '용산구')
h_filter <- df_h  %>% filter(reg_date == '2022-12-25' & pick_rgn2_nm == '용산구')

a <- a_filter["rider_cnt"] %>% rename("a" = "rider_cnt")
b <- b_filter["rider_cnt"] %>% rename("b" = "rider_cnt")
c <- c_filter["rider_cnt"] %>% rename("c" = "rider_cnt")
d <- d_filter["rider_cnt"] %>% rename("d" = "rider_cnt")
e <- e_filter["rider_cnt"] %>% rename("e" = "rider_cnt")
f <- f_filter["rider_cnt"] %>% rename("f" = "rider_cnt")
g <- g_filter["rider_cnt"] %>% rename("g" = "rider_cnt")
h <- h_filter["rider_cnt"] %>% rename("h" = "rider_cnt")

df <- cbind(a,b,c,d,e,f,g,h)

df_cor <- cor(df)
df_cor
write.csv(df_cor, "correlation_result.csv", fileEncoding = "cp949")


graphA <- groupA  %>% 
ggplot(aes(as.factor(hour_reg),rider_cnt)) +
geom_boxplot() +
labs(title = '시간대별 라이더수 분포', x = '시간', y = '라이더수')
graphA

##########################################################################################################################################################
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
# data <- data %>% 
# group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain) %>% 
# mutate(q1 = quantile(rider_cnt, 0.25),
#       q3 = quantile(rider_cnt, 0.75),
#       IQR1.5 = 1.5*(quantile(rider_cnt, 0.75) - quantile(rider_cnt, 0.25)))

# summary(data)

# data <- data  %>% 
# mutate(outlier = case_when (is_rain == 0 & (q1 - IQR1.5 > rider_cnt | rider_cnt > q3 + IQR1.5) ~ 1,
#                             TRUE ~ 0))

# table(data$outlier) # 8,981

# # outlier median 값으로 대체 
# data <- data %>% 
# group_by(pick_rgn2_nm, day_of_reg, hour_reg)  %>%
# mutate(rider_cnt_2 = case_when(outlier == 1 ~ median(rider_cnt),
#                               TRUE ~ rider_cnt))

# table(data$outlier, data$day_of_reg) #월, 수,일 많음


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



# 그래프 저장
# ggsave("강남구 월별 (raw).png", raw1)
# ggsave("강남구 월별 (cleaned).png", ch1)
# ggsave("강남구 시간대별 (raw).png", raw2)
# ggsave("강남구 시간대별 (cleaned).png", ch2)

#write.csv(data, "final_data.csv", row.names = FALSE, fileEncoding = "cp949")
