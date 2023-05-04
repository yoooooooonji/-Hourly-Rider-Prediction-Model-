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

str(data)
min(data$reg_date) # 2022-01-01
max(data$reg_date) # 2023-03-31

data$day_of_reg <- factor(data$day_of_reg, levels = c('월요일', '화요일', '수요일', '목요일','금요일', '토요일','일요일'))

table(data$day_of_reg, data$month)
dim(data) #170,625
table(data$pick_rgn2_nm) 

# 2. 분포 
# 강남구 라이더수 분포 - 평시 (이상치 제거 전)
gangnam_graph <- data %>% 
filter(pick_rgn2_nm == '강남구' & is_rain == 0) %>% 
group_by(day_of_reg) %>% 
ggplot(aes(as.factor(hour_reg),rider_cnt_2)) +
geom_boxplot() +
labs(title = '강남구 평시 시간대별 라이더수 분포', x = '시간', y = '라이더수')

gangnam_graph

# 서초구 라이더수 분포 - 평시 (이상치 제거 전)
seocho_graph <- data %>% 
filter(pick_rgn2_nm == '서초구' & is_rain == 0) %>%
group_by(day_of_reg) %>%
ggplot(aes(as.factor(hour_reg),rider_cnt_2)) +
geom_boxplot() +
labs(title = '서초구 평시 시간대별 라이더수 분포', x = '시간', y = '라이더수')

seocho_graph

# 그래프 저장
# ggsave("monthly_graph.png", monthly_graph)
# ggsave("daily_graph.png", daily_graph)
# ggsave("hourly_graph.png", hourly_graph)

# 3. 이상치 여부 파악
# rgn2, is_rain 그룹별 
library(tsibble)
library(timetk)

grouped_data <- data %>% group_by(pick_rgn2_nm, is_rain)

rain <- data %>% filter(is_rain == 1)
rain_1 <- rain %>% tk_anomaly_diagnostics(datetime, rider_cnt)
rain %>% plot_anomaly_diagnostics(datetime, rider_cnt)
table(rain_1$anomaly) #78 


tt <- data  %>% 
tk_anomaly_diagnostics(datetime, rider_cnt)
table(tt$anomaly)
test <- tt  %>% filter(anomaly == 'Yes')
test <- test %>% mutate(hour = hour(datetime))
table(test$hour)

anomaly_data <- grouped_data %>% 
tk_anomaly_diagnostics(datetime, rider_cnt)
table(grouped_data$anomaly)

# 강남구
test1 <- data %>% filter(pick_rgn2_nm == '강남구' & reg_date >= '2023-03-01')

test1  %>% 
plot_anomaly_diagnostics(datetime, rider_cnt, .alpha = 0.05, .interactive = FALSE)

test1 <- test1 %>% tk_anomaly_diagnostics(datetime, rider_cnt, .alpha = 0.05)
table(test1$anomaly)

# 4. 자기상관성 확인
# ACF : 자기상관 함수
# PACF : 부분자기상관 함수

library(stats)

acf(data$rider_cnt_2)
acf(data$rider_cnt_2, plot = FALSE)

colSums(is.na(data))

grouped_data <- split(data$rider_cnt_2, data$pick_rgn2_nm)
groups_acf <- lapply(grouped_data, acf, plot = FALSE)
groups_acf

