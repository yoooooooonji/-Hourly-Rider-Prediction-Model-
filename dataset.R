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
data1 <- read_excel("/Users/yj.noh/Desktop/rider_pick2_hour_2022.xlsx") 
data2 <- read_excel("/Users/yj.noh/Desktop/rider_pick2_hour_2023.xlsx")

data <- rbind(data1, data2)
data <- data %>%
  dplyr::rename(rider_cnt = 라이더수,
                order_cnt = 주문수)
# seoul
data <- data %>% 
  filter(pick_rgn1_nm == '서울특별시')

data<- data %>% 
  filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) & reg_date <='2023-04-30')

dim(data) # 181,875

table(data$pick_rgn2_nm)
table(data$hour_reg)

# NA 채우기 - time table
library(lubridate)
library(tidyr)

# datetime 컬럼 만들기
data$reg_date <- as.Date(data$reg_date)
data$datetime <- ymd(data$reg_date) + hours(data$hour_reg)

# 모든 조합 생성
all_combinations <- seq(from = min(data$datetime), to = max(data$datetime),  by = "hour")

# datetime_full <- seq(as.POSIXct("2022-01-01 09:00"), as.POSIXct("2023-03-31 23:00"), by = "1 hour")

# pick_rgn2_nm
location_unique <- unique(data$pick_rgn2_nm)
location_full <- expand.grid(datetime = all_combinations, pick_rgn2_nm = location_unique)

n_distinct(location_full$datetime) # 11,631
n_distinct(location_full$pick_rgn2_nm) #25

# 데이터와 조합 합치기
str(location_full$datetime)
str(data$datetime)
combined_data <- left_join(location_full, data, by = c("datetime" = "datetime", "pick_rgn2_nm" = "pick_rgn2_nm"))

combined_data <- combined_data %>% 
  mutate(hour_reg2 = hour(datetime))

dim(combined_data) # 290,775
table(combined_data$hour_reg2)

combined_data <- combined_data %>% 
  filter(hour_reg2 %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

dim(combined_data) # 181,875
table(combined_data$hour_reg2)

colSums(is.na(combined_data)) # 결측치 없음. 

combined_data <- combined_data %>% 
  mutate(reg_date_2 = as.Date(datetime))

combined_data <- combined_data %>% 
  mutate(day_of_reg2 = substr(weekdays(reg_date_2),1,3))

table(combined_data$day_of_reg2)
head(combined_data)

combined_data <- subset(combined_data, select = -c(reg_date, hour_reg, day_of_reg, pick_rgn1_nm))      
str(combined_data)

# weather
weather1 <- read.csv("/Users/yj.noh/Desktop/weather_2022.csv", fileEncoding = "cp949") 
weather2 <- read.csv("/Users/yj.noh/Desktop/weather_2023.csv", fileEncoding = "cp949")

weather <- rbind(weather1, weather2)
weather <- weather %>% 
  dplyr::rename(temp_c = 기온..C.,
                rain_c = 강수량.mm.,
                snow_c = 적설.cm.,
                date = 일시)

weather <- weather %>% 
  mutate(date_2 = as.Date(date),
         hour = hour(date)) %>% 
  filter(hour %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

table(weather$hour)

# join 
combined_data <- left_join(combined_data, weather[c("date_2","hour","temp_c","rain_c", "snow_c")], by = c("reg_date_2" = "date_2", "hour_reg2" = "hour"))

combined_data <- combined_data %>% 
  rename(
         "hour_reg" = "hour_reg2",
         "reg_date" = "reg_date_2",
         "day_of_reg" = "day_of_reg2")
head(combined_data)

# NA 
combined_data$rain_c[is.na(combined_data$rain_c)] <- 0
combined_data$snow_c[is.na(combined_data$snow_c)] <- 0
colSums(is.na(combined_data))

# 변수 생성
# is_rain
combined_data <- combined_data %>% 
  mutate(is_rain = ifelse((rain_c > 0 | snow_c > 0),1,0))
table(combined_data$is_rain) # 0: 164,825 1: 17,050

# month, week
combined_data <- combined_data %>% 
  mutate(month = month(reg_date),
         week = ceiling(day(reg_date) /7))

# is_holiday
holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09",  "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", 
"2022-10-03",  "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21","2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-01", "2023-05-05","2023-05-27", "2023-05-29", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
"2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))


combined_data$reg_date <- as.Date(combined_data$reg_date)
combined_data <- combined_data %>% 
  mutate(is_holiday = ifelse((reg_date %in% holiday_list) | (day_of_reg %in% c("SAT", "SUN")),1,0))

colSums(is.na(combined_data))

#  NA fill ()
# library(zoo)
# filled_data <- combined_data %>% 
#   arrange(datetime, location) %>% 
#   group_by(location, day_of_reg2, hour_reg2, is_rain) %>% 
#   mutate(rider_cnt_2 = ifelse(!is.na(na.locf(rider_cnt)), na.locf(rider_cnt), NA),
#          order_cnt_2 = ifelse(!is.na(na.locf(order_cnt)), na.locf(order_cnt), NA))

# colSums(is.na(filled_data)) 

# w-1,2,3,4 동일 요일 동시간대 주문수/라이더수
# filled_data <- filled_data %>%  
#   group_by(day_of_reg2, hour_reg2, location) %>% 
#   arrange(reg_date_2) %>% 
#   mutate(order_cnt_w_1 = lag(order_cnt, n=1),
#          order_cnt_w_2 = lag(order_cnt, n=2),
#          order_cnt_w_3 = lag(order_cnt, n=3),
#          order_cnt_w_4 = lag(order_cnt, n=4),
#          rider_cnt_w_1 = lag(rider_cnt, n=1),
#          rider_cnt_w_2 = lag(rider_cnt, n=2),
#          rider_cnt_w_3 = lag(rider_cnt, n=3),
#          rider_cnt_w_4 = lag(rider_cnt, n=4))


table(combined_data$hour_reg)
dim(combined_data) #181,875
min(combined_data$reg_date) #2022-01-01
max(combined_data$reg_date) #2023-04-30
str(combined_data)

# filled_data[c("rider_cnt", "order_cnt", "pick_rgn2_nm")] %>% tbl_summary( by = "pick_rgn2_nm") %>% add_p()
# filled_data[c("rider_cnt","order_cnt","pick_rgn2_nm", "is_rain")] %>% tbl_summary(by="is_rain")  %>% add_p()
# filled_data[c("rider_cnt","order_cnt","pick_rgn2_nm", "is_peak1")] %>% tbl_summary(by="is_peak1")  %>% add_p()
# filled_data[c("rider_cnt","order_cnt","pick_rgn2_nm", "is_peak2")] %>% tbl_summary(by="is_peak2") %>% add_p()

write.csv(combined_data, "combined_data.csv", row.names = FALSE, fileEncoding = "cp949")
