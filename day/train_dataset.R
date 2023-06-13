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
data <- read_excel("/Users/yj.noh/Desktop/seoul/seoul_day_new_data_20230613.xlsx") 

data <- data %>%
  dplyr::rename(rider_cnt = 라이더수,
                order_cnt = 주문수)
dim(data) 
head(data)

table(data$pick_rgn1_nm) 

data <- data %>% filter(pick_rgn1_nm == '서울특별시' & reg_date < max(reg_date))

min(data$reg_date) #2022.6.1
max(data$reg_date) # 2023.6.12

# NA 채우기 - time table
# datetime 컬럼 만들기
data$reg_date <- as.Date(data$reg_date)

combined_data <- data %>% 
  mutate(
         day_of_reg = substr(weekdays(as.Date(reg_date)),1,3))

table(combined_data$day_of_reg) 


# weather
weather <- read.csv("/Users/yj.noh/Desktop/seoul/seoul_day_weather_20230613.csv", fileEncoding = "cp949")

str(weather)

weather <- weather %>% 
  dplyr::rename(rain_c = 일강수량.mm.,
                snow_c = 일.최심적설.cm.,
                date = 일시)

weather <- weather  %>% filter(지점명 == "서울")
dim(weather) # 377

weather$date <- as.Date(weather$date)

# join 
combined_data <- left_join(combined_data, weather[c("date","rain_c", "snow_c")], by = c("reg_date" = "date"))


# NA 
combined_data$rain_c[is.na(combined_data$rain_c)] <- 0
combined_data$snow_c[is.na(combined_data$snow_c)] <- 0
colSums(is.na(combined_data))


# 변수 생성 
combined_data <- combined_data %>% 
  mutate(is_rain = ifelse((rain_c > 0 | snow_c > 0),1,0))

table(combined_data$is_rain) # 0: 258, 1: 120


# is_holiday
holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09",  "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", 
"2022-10-03",  "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21","2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-01", "2023-05-05","2023-05-27", "2023-05-29", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
"2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))

combined_data <- combined_data  %>% 
mutate(is_holiday = ifelse(reg_date %in% holiday_list, 1,0))

# 강수량 구분 (3, 15, 30)
# combined_data <- combined_data  %>% 
# mutate(rain_group = case_when(rain_c <= 0 ~ "no",
#                               0 < rain_c & rain_c < 3.0 ~ "weak",
#                               3.0<= rain_c & rain_c < 15 ~ "normal",
#                               15 <= rain_c & rain_c < 30 ~ "strong",
#                               30 <= rain_c ~ "very_strong"))

# table(combined_data$rain_group)


#w-1,2,3,4 동일 요일 동시간대 주문수/라이더수
library(zoo)

combined_data <- combined_data %>%
  arrange(reg_date) %>% 
  group_by(day_of_reg, is_rain) %>%  
  mutate(rider_cnt_w_1 = lag(rider_cnt, n=1),
         rider_cnt_w_2 = lag(rider_cnt, n=2),
         rider_cnt_w_3 = lag(rider_cnt, n=3),
         rider_cnt_w_4 = lag(rider_cnt, n=4),
         order_cnt_w_1 = lag(order_cnt, n=1))

colSums(is.na(combined_data)) # 14,28


combined_data <- combined_data  %>% 
filter(reg_date > '2022-07-01')

colSums(is.na(combined_data)) # 

# 결측치 채우기 
combined_data <- combined_data  %>% 
group_by(day_of_reg) %>% 
mutate(rider_cnt_w_1 = ifelse(is.na(rider_cnt_w_1),lag(rider_cnt,n=1), rider_cnt_w_1),
       rider_cnt_w_2 = ifelse(is.na(rider_cnt_w_2), rider_cnt_w_1, rider_cnt_w_2),
       rider_cnt_w_3 = ifelse(is.na(rider_cnt_w_3), rider_cnt_w_2, rider_cnt_w_3),
       rider_cnt_w_4 = ifelse(is.na(rider_cnt_w_4), rider_cnt_w_3, rider_cnt_w_4),
       order_cnt_w_1 = ifelse(is.na(order_cnt_w_1), lag(order_cnt, n=1), order_cnt_w_1))

colSums(is.na(combined_data))

# group 
# combined_data <- combined_data %>% 
# mutate(group_s = case_when(day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일') & holiday_yn == "N"  & is_rain ==0 ~ "A",
#                            day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일') & holiday_yn == "N"  & is_rain ==1 ~ "B", 
#                            day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일') & holiday_yn == "Y"  & is_rain ==0 ~ "C", 
#                            day_of_reg %in% c('월요일','화요일','수요일','목요일','금요일') & holiday_yn == "Y"  & is_rain ==1 ~ "D",
#                            day_of_reg %in% c('토요일','일요일') & holiday_yn == "N"  & is_rain ==0 ~ "E",
#                            day_of_reg %in% c('토요일','일요일') & holiday_yn==  "N"  & is_rain ==1 ~ "F",
#                            day_of_reg %in% c('토요일','일요일') & holiday_yn == "Y"  & is_rain ==0 ~ "G",
#                            day_of_reg %in% c('토요일','일요일') & holiday_yn == "Y"  & is_rain==1 ~ "H"))

combined_data <- subset(combined_data, select = -c(pick_rgn1_nm, order_cnt, holiday_yn))

write.csv(combined_data, "day/combined_data_day.csv", row.names = FALSE, fileEncoding = "cp949")







