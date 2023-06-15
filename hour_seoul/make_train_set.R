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
data <- read_excel("/Users/yj.noh/Desktop/seoul/seoul_hour_new_data_20230614.xlsx")
#data2 <- read_excel("/Users/yj.noh/Desktop/train_data_2023.xlsx")

#data <- rbind(data1, data2)
data <- data%>%
  dplyr::rename(rider_cnt = 라이더수,
                order_cnt = 주문수,
                pick_rgn1_nm = dlvry_rgn1_nm,
                pick_rgn2_nm = dlvry_rgn2_nm)

# seoul
data <- data %>% 
  filter(pick_rgn1_nm == '서울특별시' & reg_date < max(data$reg_date))

dim(data) # 

# 9am ~ 23pm 
data <- data  %>% 
filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

dim(data) # 
table(data$hour_reg) # 결측치 없음. 


# NA 채우기 - time table
# datetime 컬럼 만들기
data$reg_date <- as.Date(data$reg_date)
data$datetime <- ymd(data$reg_date) + hours(data$hour_reg)

min(data$datetime) # "2022-06-01 09:00:00 UTC"
max(data$datetime) # "2023-06-14 23:00:00 UTC"

combined_data <- data %>% 
  mutate(hour_reg2 = hour(datetime),
         reg_date2 = as.Date(datetime),
         day_of_reg = substr(weekdays(as.Date(datetime)),1,3))

table(combined_data$day_of_reg) 
str(combined_data)

combined_data <- subset(combined_data, select = -c(reg_date, hour_reg, pick_rgn1_nm))      
combined_data <- combined_data  %>% 
rename("hour_reg" = "hour_reg2",
       "reg_date" = "reg_date2")


# is_holiday
holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09",  "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", 
"2022-10-03",  "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21","2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-01", "2023-05-05","2023-05-27", "2023-05-29", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
"2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))

combined_data <- combined_data  %>% 
mutate(is_holiday = ifelse(reg_date %in% holiday_list, 1,0))

table(combined_data$is_holiday)

# weather
weather <- read.csv("/Users/yj.noh/Desktop/seoul/seoul_hour_weather_20230614.csv", fileEncoding = "cp949")
#weather2 <- read.csv("/Users/yj.noh/Desktop/weather_2023.csv", fileEncoding = "cp949")

#weather <- rbind(weather1, weather2)
weather <- weather %>% 
  dplyr::rename(rain_c = 강수량.mm.,
                snow_c = 적설.cm.,
                date = 일시)

weather <- weather %>% 
  mutate(date_2 = as.Date(date),
         hour = hour(date)) 

table(weather$hour)

# join 
weather <- weather  %>% filter(hour %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
combined_data <- left_join(combined_data, weather[c("date_2","hour","rain_c", "snow_c")], by = c("reg_date" = "date_2", "hour_reg" = "hour"))

# NA 
combined_data$rain_c[is.na(combined_data$rain_c)] <- 0
combined_data$snow_c[is.na(combined_data$snow_c)] <- 0
colSums(is.na(combined_data))

# 변수 생성 
# is_rain2 (하루 중 1시간이라도 비가 오면 1 )

combined_data <- combined_data %>% 
  mutate(is_rain = ifelse((rain_c > 0 | snow_c > 0),1,0))

table(combined_data$is_rain)

combined_data <- combined_data %>% 
group_by(reg_date) %>% 
mutate(is_rain2 = ifelse(sum(is_rain) >0 ,1,0))

table(combined_data$is_rain2) # 

# 강수량 구분 (3, 15, 30)
# combined_data <- combined_data  %>% 
# mutate(rain_group = case_when(rain_c <= 0 ~ "no",
#                               0 < rain_c & rain_c < 3.0 ~ "weak",
#                               3.0<= rain_c & rain_c < 15 ~ "normal",
#                               15 <= rain_c & rain_c < 30 ~ "strong",
#                               30 <= rain_c ~ "very_strong"))

# table(combined_data$rain_group)
# table(combined_data$holiday_yn) # y 17250

#  이상치(outlier) 여부 파악
train_data <- combined_data  %>% 
filter(reg_date <= '2023-03-31')

train_data <- train_data %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain2, is_holiday) %>% 
mutate(q1 = quantile(rider_cnt, 0.25),
      q3 = quantile(rider_cnt, 0.75),
      IQR1.5 = 1.5*(quantile(rider_cnt, 0.75) - quantile(rider_cnt, 0.25)))

train_data <- train_data  %>% 
mutate(outlier = case_when ((is_rain2 == 0 & (q1 - IQR1.5 > rider_cnt | rider_cnt > q3 + IQR1.5) | 
is_holiday == 0 & (q1 - IQR1.5 > rider_cnt | rider_cnt > q3 + IQR1.5)) ~ 1, TRUE ~ 0))

table(train_data$outlier) 


# outlier median 값으로 대체 
train_data <- train_data %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg)  %>%
mutate(rider_cnt_2 = case_when(outlier == 1 ~ median(rider_cnt),
                              TRUE ~ rider_cnt))
str(train_data)
table(train_data$outlier, train_data$day_of_reg) 

train_data <- subset(train_data, select = -c(rider_cnt, q1, q3, IQR1.5, outlier))

train_data <- train_data  %>% rename("rider_cnt" = "rider_cnt_2") %>% 
dplyr::select (pick_rgn2_nm, rider_cnt, order_cnt, datetime, hour_reg, reg_date, day_of_reg, is_holiday, rain_c, snow_c, is_rain, is_rain2)


# test set
test_data <- combined_data  %>% 
filter(reg_date >= '2023-04-01')

combined_data <- rbind(train_data, test_data)
dim(combined_data) # 
colSums(is.na(combined_data))

# w-1,2,3,4 동일 요일 동시간대 주문수/라이더수 - 기상 여부 포함 
library(zoo)
combined_data <- combined_data %>%
  arrange(datetime, pick_rgn2_nm) %>% 
  group_by(pick_rgn2_nm, day_of_reg, hour_reg, is_rain2) %>%  
  mutate(rider_cnt_w_1 = lag(rider_cnt, n=1),
         rider_cnt_w_2 = lag(rider_cnt, n=2),
         rider_cnt_w_3 = lag(rider_cnt, n=3),
         rider_cnt_w_4 = lag(rider_cnt, n=4),
         order_cnt_w_1 = lag(order_cnt, n=1))

colSums(is.na(combined_data)) 

# 결측치 채우기 
combined_data <- combined_data  %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg) %>% 
mutate(rider_cnt_w_1 = ifelse(is.na(rider_cnt_w_1),lag(rider_cnt,n=1), rider_cnt_w_1),
       rider_cnt_w_2 = ifelse(is.na(rider_cnt_w_2), rider_cnt_w_1, rider_cnt_w_2),
       rider_cnt_w_3 = ifelse(is.na(rider_cnt_w_3), rider_cnt_w_2, rider_cnt_w_3),
       rider_cnt_w_4 = ifelse(is.na(rider_cnt_w_4), rider_cnt_w_3, rider_cnt_w_4),
       order_cnt_w_1 = ifelse(is.na(order_cnt_w_1), lag(order_cnt, n=1), order_cnt_w_1))

colSums(is.na(combined_data)) 

combined_data <- combined_data  %>% 
filter(reg_date >= '2022-07-01')

colSums(is.na(combined_data))  # 

# 평일 = 공휴일 -> 토요일 데이터 붙이기 
# 토요일 데이터를 별도로 분리
# saturday_data <- combined_data[c("reg_date","pick_rgn2_nm", "hour_reg","rider_cnt_w_1", "rider_cnt_w_2", "rider_cnt_w_3","rider_cnt_w_4",
#                                   "order_cnt_w_1", "day_of_reg")] %>% filter(day_of_reg == '토요일') %>% 
#                                   rename("rider_cnt_w_1_sat" = "rider_cnt_w_1",
#                                         "rider_cnt_w_2_sat" = "rider_cnt_w_2",
#                                         "rider_cnt_w_3_sat" = "rider_cnt_w_3",
#                                         "rider_cnt_w_4_sat" = "rider_cnt_w_4",
#                                         "order_cnt_w_1_sat" = "order_cnt_w_1")
# str(saturday_data)

# combined_data <- combined_data %>% 
# mutate(reg_date_change = case_when(day_of_reg == '월요일' ~ reg_date+5,
#                                    day_of_reg == '화요일' ~ reg_date+4,
#                                    day_of_reg == '수요일' ~ reg_date+3,
#                                    day_of_reg == '목요일' ~ reg_date+2,
#                                    day_of_reg == '금요일' ~ reg_date+1,
#                                    TRUE ~ reg_date))

# combined_data <- left_join(combined_data, saturday_data, by = c("reg_date_change" = "reg_date", "pick_rgn2_nm" = "pick_rgn2_nm", "hour_reg" = "hour_reg"))
# colSums(is.na(combined_data))

# combined_data <- combined_data  %>% 
# mutate(rider_cnt_w_1_new = case_when(holiday_yn == "Y" & day_of_reg.x %in% c("월요일","화요일", "수요일","목요일","금요일") ~ rider_cnt_w_1_sat, 
#                                       TRUE ~ rider_cnt_w_1),
#       rider_cnt_w_2_new = case_when(holiday_yn == "Y" & day_of_reg.x %in% c("월요일","화요일", "수요일","목요일","금요일") ~ rider_cnt_w_2_sat, 
#                                       TRUE ~ rider_cnt_w_2),
#       rider_cnt_w_3_new = case_when(holiday_yn == "Y" & day_of_reg.x %in% c("월요일","화요일", "수요일","목요일","금요일") ~ rider_cnt_w_3_sat, 
#                                     TRUE ~ rider_cnt_w_3),
#       rider_cnt_w_4_new = case_when(holiday_yn == "Y" & day_of_reg.x %in% c("월요일","화요일", "수요일","목요일","금요일") ~ rider_cnt_w_4_sat, 
#                                       TRUE ~ rider_cnt_w_4),
#       order_cnt_w_1_new = case_when(holiday_yn == "Y" & day_of_reg.x %in% c("월요일","화요일", "수요일","목요일","금요일") ~ order_cnt_w_1_sat, 
#                                       TRUE ~ order_cnt_w_1))

# colSums(is.na(combined_data))

# combined_data <- combined_data[c("day_of_reg.x", "pick_rgn2_nm","rider_cnt","order_cnt","datetime","hour_reg", "reg_date","rain_c","snow_c","is_rain",
#                                   "is_rain2","rain_group","holiday_yn","rider_cnt_w_1_new", "rider_cnt_w_2_new", "rider_cnt_w_3_new","rider_cnt_w_4_new",
#                                   "order_cnt_w_1_new")] %>% 
#                                   rename("day_of_reg" = "day_of_reg.x")
                            
# rider_crm 
# crm <- read_excel("/Users/yj.noh/Desktop/rider_crm.xlsx")
# crm$reg_date <- as.Date(crm$reg_date)

# colSums(is.na(combined_data))

# combined_data <- left_join(combined_data, crm, by = c("reg_date","hour_reg"))
# colSums(is.na(combined_data))

# group 
# combined_data <- combined_data %>% 
# mutate(group_s = case_when(day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일') & holiday_yn == "N"  & is_rain2 == 0 ~ "A",
#                            day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일') & holiday_yn == "N"  & is_rain2 ==1 ~ "B", 
#                            day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일') & holiday_yn == "Y"  & is_rain2 ==0 ~ "C", 
#                            day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일') & holiday_yn == "Y"  & is_rain2 ==1 ~ "D",
#                            day_of_reg %in% c('토요일','일요일') & is_rain2 ==0 ~ "E",
#                            day_of_reg %in% c('토요일','일요일') & is_rain2 == 1 ~ "F"))


# table(combined_data$group_s)

colSums(is.na(combined_data))

combined_data <- subset(combined_data, select = -c(order_cnt, rain_c, snow_c, is_rain2))

write.csv(combined_data, "hour_seoul/combined_data.csv", row.names = FALSE, fileEncoding = "cp949")

