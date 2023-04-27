# 0. install packages 

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg<-c("readr","dplyr","tidytext","tidyverse","lubridate","reshape2","psych",
       "PerformanceAnalytics","MatchIt", "gtsummary","gridExtra", "ggridges", "patchwork","caret", "glmnet", "caTools", "xgboost", "fastDummies", "nnet",
       "MASS","car","Epi","readxl","summarytools", "ggplot2", "survminer", "survival","gridExtra","grid", "ggsurvfit", "unitedR")
ipak(pkg)

##########################################################################################################################################################
# 1. data load
data1 <- read_excel("/Users/yj.noh/Desktop/rider_group_hour_2022.xlsx") 
data2 <- read_excel("/Users/yj.noh/Desktop/rider_group_hour_2023.xlsx")

data <- rbind(data1,data2) 
data <- data %>% 
  rename(rider_cnt = 라이더수)

str(data)
# 2. 변수 
# month, week, hour, day, location -> category 
str(data)

data <- data %>% 
  mutate(month = month(reg_date),
         day = day(reg_date),
         week = ceiling(day(reg_date) /7))

data<- data %>% 
  mutate(hour = case_when (hour_reg == "09_11"~1,
                           hour_reg == "11_13"~2,
                           hour_reg == "13_17"~3,
                           hour_reg == "17_20"~4,
                           hour_reg == "20_26"~5))
table(data$hour)
table(data$hour_reg)

# 전일 동시간대 주문수
data <- data %>% 
  group_by(hour_reg, pick_rgn1_nm) %>% 
  mutate(order_cnt_lag = lag(주문수))

# 전주 동일 요일 동시간대 주문수
data <- data %>% 
  group_by(day_of_reg, hour_reg, pick_rgn1_nm) %>% 
  arrange(reg_date) %>% 
  mutate(order_cnt_last_week = lag(주문수))

data <- data %>% 
  filter(reg_date <= '2023-03-31')
dim(data) #24845 

# holiday
# 공휴일 여부 판단 함수 정의
holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09", 
                     "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15",
                     "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", "2022-10-03", 
                     "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21",
                     "2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-05",
                     "2023-05-27", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
                     "2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))

data$reg_date <- as.Date(data$reg_date)
data <- data %>% 
  mutate(is_holiday = ifelse((reg_date %in% holiday_list) | (day_of_reg %in% c("FRI", "SAT", "SUN")),1,0))

# check NA
colSums(is.na(data))
data <- data %>% 
  filter(!is.na(order_cnt_lag)&!is.na(order_cnt_last_week))
dim(data) #24285

#write.csv(data, "data_rider_predict.csv", row.names = FALSE, fileEncoding = "cp949")
#####################################################################################################
#modeling
str(data)

var <-  c('month','day','week','hour','is_holiday', 'pick_rgn1_nm')
data[,var]<- lapply(data[,var], factor)

# train_test 
train <- data %>% 
  filter(reg_date <= '2022-12-31')

test <- data %>% 
  filter(reg_date >= '2023-01-01')

model1 <- lm(라이더수 ~ order_cnt_lag+order_cnt_last_week, train =train)
model2 <- lm(라이더수 ~ order_cnt_lag, train =train)
model3 <- lm(라이더수 ~ order_cnt_last_week, train =train)

summary(model1) #0.9386
summary(model2) #0.9088
summary(model3) #0.9334

model <- lm(라이더수~ pick_rgn1_nm + month + day + week + hour + order_cnt_lag + order_cnt_last_week + is_holiday, train = train)
summary(model) # 0.9771 

model_fin <- lm(라이더수~ pick_rgn1_nm + hour + order_cnt_lag + order_cnt_last_week + is_holiday, train = train)
summary(model_fin) # 0.9766 

# accuracy 
p <- predict(model_fin, newdata= test)
error<- p-test$라이더수
RMSE <- sqrt(mean(error^2))
RMSE #348.4607


# k-fold cross-validation
model_cv <- train(
  라이더수~ month + day + week + hour + order_cnt_lag + order_cnt_last_week + is_holiday,
  train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number =10,
    verboseIter = true 
  )
)






