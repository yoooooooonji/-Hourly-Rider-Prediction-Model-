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
data <- read_excel("/Users/yj.noh/Desktop/predict_data.xlsx")

data <- data %>%
  dplyr::rename(rider_cnt = 라이더수,
                order_cnt = 주문수)
# seoul
min(data$reg_date) #2023-04-18
max(data$reg_date) #2023-05-15

data$reg_date <- as.Date(data$reg_date)

head(data)

data <- data %>% 
  filter(pick_rgn1_nm == '서울특별시' & reg_date < max(data$reg_date))

dim(data) # 11,675

table(data$pick_rgn2_nm)
table(data$hour_reg) 

# 9am ~ 23pm
data <- data  %>%
filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

dim(data) #10125
table(data$hour_reg) # 결측치 없음. 

# 변수 생성
# is_rain
data <- data  %>% 
mutate(is_rain = ifelse(reg_date == '2023-05-11', 1, 0))
table(data$is_rain)

# month, week
data <- data  %>% 
  mutate(month = month(reg_date),
         week = ceiling(day(reg_date) /7),
         day_of_reg2 = substr(weekdays(as.Date(reg_date)),1,3),
         day_of_reg = case_when(day_of_reg2 %in% c("토요일","일요일") ~"주말",
                                day_of_reg2 == '금요일' ~"금",
                                TRUE ~"월목"))
table(data$day_of_reg2)
table(data$day_of_reg)

#w-1,2,3,4 동일 요일 동시간대 주문수/라이더수
library(zoo)

data <- data %>%
  arrange(reg_date,pick_rgn2_nm) %>% 
  group_by(pick_rgn2_nm, day_of_reg2, hour_reg) %>% 
  mutate(rider_cnt_w_2 = lag(rider_cnt, n=1),
         rider_cnt_w_3 = lag(rider_cnt, n=2),
         rider_cnt_w_4 = lag(rider_cnt, n=3),
         order_cnt_w_2 = lag(order_cnt, n=1),
         order_cnt_w_3 = lag(order_cnt, n=2),
         order_cnt_w_4 = lag(order_cnt, n=3))


str(data)

# 정리
# 정리
data <- data  %>% mutate(reg_date = reg_date + 7,
                          rider_cnt_w_1 = rider_cnt,
                          order_cnt_w_1 = order_cnt)


data<- data  %>% filter(reg_date >='2023-05-16')
dim(data) #2250

# is_holiday
holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09",  "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", 
"2022-10-03",  "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21","2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-01", "2023-05-05","2023-05-27", "2023-05-29", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
"2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))



data <- data %>% 
mutate(is_holiday = ifelse((reg_date %in% holiday_list) | (day_of_reg2 %in% c("토요일", "일요일")),1,0),
          is_holiday2 = ifelse((reg_date %in% holiday_list),1,0))


colSums(is.na(data))
table(data$is_holiday)
table(data$is_holiday2)

dim(data) #2250

min(data$reg_date)
max(data$reg_date)
colSums(is.na(data))


data <- subset(data, select = -c(pick_rgn1_nm, rider_cnt, order_cnt))

write.csv(data, "predict_data.csv", fileEncoding = "cp949", row.names = FALSE)






