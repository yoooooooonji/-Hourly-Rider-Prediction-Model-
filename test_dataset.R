# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg<-c("readr","dplyr","tidytext","tidyverse","lubridate","reshape2","psych","gtsummary", "readxl", "MASS")
ipak(pkg)

##########################################################################################################################################################
# data load
test_data <- read_excel("/Users/yj.noh/Desktop/test_data_20230501.xlsx") 

test_data <- test_data %>% 
  rename("rider_cnt" = "라이더수",
         "order_cnt" = "주문수")

# seoul
test_data <- test_data %>% 
  filter(pick_rgn1_nm == '서울특별시') 

test_data<- test_data %>% 
  filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,0,1,2))

dim(test_data) # 15,170

# 0~2시 하루 앞당기기 
test_data$reg_date <- as.Date(test_data$reg_date)

test_data <- test_data %>% 
  mutate(reg_date = case_when(hour_reg %in% c(0,1,2) ~ reg_date-1,
                              TRUE ~ reg_date))

dim(test_data)  #15170


# NA 채우기 - time table
library(lubridate)
library(tidyr)

# datetime 컬럼 만들기
test_data$reg_date <- as.Date(test_data$reg_date)
test_data$datetime <- ymd(test_data$reg_date) + hours(test_data$hour_reg)

# 모든 조합 생성
all_combinations <- seq(from = min(test_data$datetime), to = max(test_data$datetime),  by = "hour")

# pick_rgn2_nm
location_unique <- unique(test_data$pick_rgn2_nm)
location_full <- expand.grid(datetime = all_combinations, location = location_unique)

n_distinct(location_full$datetime) 
n_distinct(location_full$location) 

# 데이터와 조합 합치기
str(location_full$datetime)
str(test_data$datetime)

combined_data <- left_join(location_full, test_data, by = c("datetime" = "datetime", "location" = "pick_rgn2_nm"))

combined_data <- combined_data %>% 
  mutate(hour_reg2 = hour(datetime))

dim(combined_data) # 21,600

combined_data <- combined_data %>% 
  filter(hour_reg2 %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,0,1))

dim(combined_data) # 15,300

combined_data <- combined_data %>% 
  mutate(reg_date_2 = as.Date(datetime))

combined_data <- combined_data %>% 
  mutate(day_of_reg2 = substr(weekdays(reg_date_2),1,3))

combined_data <- subset(combined_data, select = -c(reg_date, hour_reg, day_of_reg, pick_rgn1_nm))      

combined_data <- combined_data %>% 
  filter(reg_date_2 >= '2023-03-28')

dim(combined_data) #14875

# weather
weather <- read.csv("/Users/yj.noh/Desktop/test_weather.csv", fileEncoding = "cp949") 
weather <- weather %>% 
  dplyr::rename(temp_c = 기온..C.,
                rain_c = 강수량.mm.,
                snow_c = 적설.cm.,
                date = 일시)

weather <- weather %>% 
  mutate(date_2 = as.Date(date),
         hour = hour(date))

weather <- weather %>% 
  mutate(date_3 = case_when (hour %in% c(0,1,2) ~ date_2-1, 
                             TRUE ~ date_2)) 

# join 
combined_data <- left_join(combined_data, weather[c("date_3","hour","temp_c","rain_c", "snow_c")], by = c("reg_date_2" = "date_3", "hour_reg2" = "hour"))


# NA 
combined_data$rain_c[is.na(combined_data$rain_c)] <- 0
combined_data$snow_c[is.na(combined_data$snow_c)] <- 0
colSums(is.na(combined_data))

# is_rain
combined_data <- combined_data %>% 
  mutate(is_rain = ifelse((rain_c > 0 | snow_c > 0),1,0))

# NA fill 
library(zoo)
filled_data <- combined_data %>% 
  arrange(datetime, location) %>% 
  group_by(location, day_of_reg2, hour_reg2, is_rain) %>% 
  mutate(rider_cnt_2 = ifelse(!is.na(na.locf(rider_cnt)), na.locf(rider_cnt), NA),
         order_cnt_2 = ifelse(!is.na(na.locf(order_cnt)), na.locf(order_cnt), NA))

colSums(is.na(filled_data)) 

# 변수 생성
table(filled_data$hour_reg2)

filled_data <- filled_data %>% 
  mutate(hour_reg2 = case_when(hour_reg2 == 0 ~ 24,
                               hour_reg2 == 1 ~ 25,
                               TRUE ~ hour_reg2))

filled_data <- filled_data %>% 
  mutate(month = month(reg_date_2),
         week = ceiling(day(reg_date_2) /7))


# w-1,2,3,4 동일 요일 동시간대 주문수/라이더수
filled_data <- filled_data %>%  
  group_by(day_of_reg2, hour_reg2, location) %>% 
  arrange(reg_date_2) %>% 
  mutate(order_cnt_w_1 = lag(order_cnt, n=1),
         order_cnt_w_2 = lag(order_cnt, n=2),
         order_cnt_w_3 = lag(order_cnt, n=3),
         order_cnt_w_4 = lag(order_cnt, n=4),
         rider_cnt_w_1 = lag(rider_cnt, n=1),
         rider_cnt_w_2 = lag(rider_cnt, n=2),
         rider_cnt_w_3 = lag(rider_cnt, n=3),
         rider_cnt_w_4 = lag(rider_cnt, n=4))


filled_data <- filled_data %>% 
  rename("pick_rgn2_nm" = "location",
         "hour_reg" = "hour_reg2",
         "reg_date" = "reg_date_2",
         "day_of_reg" = "day_of_reg2")

filled_data <- filled_data %>% 
  filter(reg_date >='2023-04-25' & reg_date <= '2023-05-01')

colSums(is.na(filled_data))

write.csv(filled_data, "test_data_20230501.csv", row.names = FALSE, fileEncoding = "cp949")


###################
test_data <- read.csv("/Users/yj.noh/Desktop/test_data_20230501.csv", fileEncoding = "cp949") 
test_data <- test_data %>% 
  filter(reg_date >= '2023-05-03')

# 변수 생성
str(test_data)
test_data$reg_date<- as.Date(test_data$reg_date)

test_data <- test_data %>% 
  mutate(month = month(reg_date),
         week = ceiling(day(reg_date) /7))


# 닐씨
test_data <- test_data %>% 
  mutate(temp_c = case_when (reg_date  == '2023-05-03' ~ 26,
                              reg_date == '2023-05-04' ~ 23,
                              reg_date == '2023-05-05' ~ 21,
                              reg_date == '2023-05-06' ~ 23,
                              reg_date == '2023-05-07' ~ 23,
                              reg_date == '2023-05-08' ~ 22),
         rain_c = case_when (reg_date  == '2023-05-03' ~ 0,
                             reg_date == '2023-05-04' ~ 10,
                             reg_date == '2023-05-05' ~ 10,
                             reg_date == '2023-05-06' ~ 0,
                             reg_date == '2023-05-07' ~ 0,
                             reg_date == '2023-05-08' ~ 0),
         snow_c = 0,
         is_rain = case_when(reg_date  == '2023-05-03' ~ 0,
                             reg_date == '2023-05-04' ~ 1,
                             reg_date == '2023-05-05' ~ 1,
                             reg_date == '2023-05-06' ~ 0,
                             reg_date == '2023-05-07' ~ 0,
                             reg_date == '2023-05-08' ~ 0),
         is_holiday = case_when(reg_date  == '2023-05-03' ~ 0,
                                reg_date == '2023-05-04' ~ 0,
                                reg_date == '2023-05-05' ~ 1,
                                reg_date == '2023-05-06' ~ 1,
                                reg_date == '2023-05-07' ~ 1,
                                reg_date == '2023-05-08' ~ 0))



colSums(is.na(test_data))

var <-  c('pick_rgn2_nm', 'hour_reg','day_of_reg','is_rain','month','week','is_holiday')
test_data[,var]<- lapply(test_data[,var], factor)


test_data_all_not <- test_data %>% 
  filter(pick_rgn2_nm != '강남구' & is_rain == 0)

test_data_gn_not <- test_data %>% 
  filter(pick_rgn2_nm == '강남구' & is_rain == 0)

test_data_all_rain <- test_data %>% 
  filter(is_rain == 1)

dim(test_data_all_not) # 1632
dim(test_data_gn_not) # 68
dim(test_data_all_rain) # 850

dim(test_data)
table(test_data$is_rain)

test_data_all_not_2 <- subset(test_data_all_not, select = -c(reg_date))
test_data_gn_not_2 <- subset(test_data_gn_not, select = -c(reg_date))
test_data_all_rain_2 <- subset(test_data_all_rain, select = -c(reg_date))

### modeling predict

# rain 
rain_predict <-predict(model_rain, newdata = test_data_all_rain_2)
not_rain_all_predict <- predict(model_not_rain, newdata = test_data_all_not_2)
not_rain_gn_predict <- predict(model_not_rain_gn, newdata = test_data_gn_not_2)


rain_predict <- data.frame(rain_predict, test_data_all_rain$reg_date, test_data_all_rain$hour_reg, test_data_all_rain$pick_rgn2_nm)
not_rain_all_predict <- data.frame(not_rain_all_predict, test_data_all_not$reg_date, test_data_all_not$hour_reg, test_data_all_not$pick_rgn2_nm)
not_rain_gn_predict <- data.frame(not_rain_gn_predict, test_data_gn_not$reg_date,test_data_gn_not$hour_reg, test_data_gn_not$pick_rgn2_nm)

write.csv(rain_predict, "우천일예측.csv", row.names = FALSE, fileEncoding = "cp949")
write.csv(not_rain_all_predict, "강남구제외예측.csv", row.names = FALSE, fileEncoding = "cp949")
write.csv(not_rain_gn_predict, "강남구예측.csv", row.names = FALSE, fileEncoding = "cp949")


