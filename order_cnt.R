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

####################################################################################

# 일별 주문수 비율 계산하기 
# data load
data <- read_excel("/Users/yj.noh/Desktop/train_data_2023.xlsx")
head(data)

data$reg_date <- as.Date(data$reg_date)
data <- data  %>% mutate(day_of_reg = weekdays(reg_date))
data <- data %>%
  dplyr::rename(rider_cnt = 라이더수,
                order_cnt = 주문수)

data <- data %>% 
  filter(pick_rgn1_nm == '서울특별시' & reg_date >= '2023-04-27' & reg_date < '2023-05-12')

data <- data  %>% 
filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
  
dim(data) # 5625
min(data$reg_date)  # 2023-04-27
max(data$reg_date) #2023-05-11

table <- data %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg) %>% 
summarise(total_order_cnt = sum(order_cnt)) %>% 
mutate(order_cnt_ratio = round(total_order_cnt / sum(total_order_cnt),2))


# write.csv(table, "order_cnt_ratio.csv", row.names = FALSE, fileEncoding = "cp949")

# 일단위 예측값 불러오기 
order_goal = read_excel("/Users/yj.noh/Desktop/order_cnt_goal.xlsx", sheet =2) 
head(order_goal)
head(table)

order_goal$date <- as.Date(order_goal$date)
order_goal <- order_goal  %>% 
mutate(day_of_reg = weekdays(date))
 
hour_unique <- unique(table$hour_reg)
date_unique <- unique(order_goal$date)

hour_full <- expand.grid(date = date_unique, hour_reg = hour_unique)

order_goal_full<- full_join(hour_full, order_goal, by = "date")

order_goal_full <- left_join(order_goal_full, table[c("pick_rgn2_nm","day_of_reg", "hour_reg","order_cnt_ratio")], 
by =  c ("day_of_reg" = "day_of_reg", "pick_rgn2_nm" = "pick_rgn2_nm", "hour_reg" = "hour_reg"))

order_goal_full$order_cnt_predict <- round(order_goal_full$order_cnt*order_goal_full$order_cnt_ratio)


#tt <- order_goal_full  %>% filter(pick_rgn2_nm == '강남구')


write.csv(order_goal_full, "order_cnt_goal.csv", row.names = FALSE, fileEncoding = "cp949")
