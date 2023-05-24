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

#####################################################################################################
# data load 
getwd()
data <- read.csv("combined_data.csv", fileEncoding = "cp949")
dim(data) #179,250
str(data)

colSums(is.na(data))

data <- data  %>% filter(!is.na(rider_cnt_w_4))

dim(data)
min(data$reg_date) #2022-01-29
max(data$reg_date) #2023-05-21


#modeling
var <-  c('pick_rgn2_nm', 'hour_reg','day_of_reg','is_rain','month','week','is_holiday1', 'is_holiday2', 'specific_value')
data[,var]<- lapply(data[,var], factor)

# correlation
cor(data[c("rider_cnt_2", "order_cnt_w_1", "order_cnt_w_2","order_cnt_w_3","order_cnt_w_4",
           "rider_cnt_w_1", "rider_cnt_w_2", "rider_cnt_w_3", "rider_cnt_w_4")])

# train_test 
train <- data %>% 
  filter(reg_date <= '2022-12-31')

dim(train) # 126,375

test <- data %>% 
  filter(reg_date >= '2023-01-01')

dim(test) # 52,875

train_set <- subset(train, select = -c(datetime,rider_cnt, order_cnt, temp_c,rain_c, snow_c, q1,q3,IQR1.5, outlier, reg_date))
test_set <- subset(test, select = -c(datetime,rider_cnt, order_cnt, temp_c,rain_c, snow_c, q1,q3,IQR1.5, outlier, reg_date))

str(train_set)
head(train_set)


library(forecast)
library(MASS)
library(caret)

# is_holiday 결정
tt <- lm(rider_cnt_2~is_holiday1, data = train_set)
summary(tt)

ss <- lm(rider_cnt_2 ~ is_holiday2, data= train_set)
summary(ss) # 유의미하지않음. 

# specific_value # 무의미 
sv <- lm(rider_cnt_2 ~ specific_value, data = train_set)
summary(sv)

#is_rain 유의미 
ir <- lm(rider_cnt_2 ~ is_rain, data = train_set)
summary(ir)

# month, week, day_of_reg, day_of_reg2, hour_reg
mr <- lm(rider_cnt_2 ~ month, data = train_set)
summary(mr)

wk <- 
# all
model <- lm(rider_cnt_2 ~., data = train_set)
summary(model) # 0.9887
accuracy(model) # 14.31, 21.07

# 10-fold cross-validation 수행
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
lm_model <- train(rider_cnt_2 ~ ., data = train_set, method = "lm", trainControl = train_control)
summary(lm_model) # 0.9887


# p-value 0.05 
model_pv <- lm(rider_cnt_2 ~ pick_rgn2_nm + hour_reg + day_of_reg + is_rain + month + week + is_holiday + rider_cnt_w_1 + rider_cnt_w_2 + rider_cnt_w_3 + rider_cnt_w_4 + order_cnt_w_1 + order_cnt_w_2 + order_cnt_w_4, data = train_set)
summary(model_pv) #0.9823
accuracy(model_pv) #14.31 , 21.07

# stepAIC
stepAIC(model, direction = "both")
model_AIC <- lm(formula = rider_cnt_2 ~ pick_rgn2_nm + hour_reg + day_of_reg + 
    is_rain + month + week + is_holiday + rider_cnt_w_1 + rider_cnt_w_2 + 
    rider_cnt_w_3 + rider_cnt_w_4 + order_cnt_w_1 + order_cnt_w_2 + 
    order_cnt_w_4, data = train_set)
summary(model_AIC) 
accuracy(model_AIC)  

#다중공선성
library(car)

vif(model) 
vif(model_AIC)
# plot 
plot(test$rider_cnt)
lines(model_AIC$fitted.values, col = "red")

# test set 
# accuracy 
y_pred <- predict(lm_model, newdata = test_set) # 모델의 예측값
#y_pred_rescaled <- y_pred * (test_max - test_min) + test_min


r_square = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

actual <- test$rider_cnt_2

rmse <- rmse(actual, y_pred)
r_square <- r_square(actual, y_pred)
mae <- mae(actual, y_pred)
error = y_pred - actual

rmse # 16.09
r_square # 0.98
mae # 10.56

# result <- data.frame(y_pred, actual,test$reg_date, test$hour_reg,test$day_of_reg, test$pick_rgn2_nm, test$month, test$is_holiday, error)

#############################################################################################################################################
# 지역별 시간대별 mae 
result_hour <- aggregate(abs(y_pred - actual), 
                    by = list(test_set$hour_reg, test_set$pick_rgn2_nm), 
                    FUN = mean)

result_hour 

# write.csv(result2, "result4_2.csv", row.names = FALSE, fileEncoding = "cp949")

#############################################################################################################################################
# 잔차 
# 잔차 등분산성
par(mfrow = c(2,2))
plot(model_AIC)

#잔차 독립성
library(car)
hap_1_res <- residuals(model_AIC)
durbinWatsonTest(hap_1_res) #0.7499512

#잔차 정규성
shapiro.test(hap_1_res)

#############################################################################################################################################

# real data predict 
real_data <- read_excel("/Users/yj.noh/Desktop/test_data.xlsx") 
real_data <- real_data %>% 
filter(pick_rgn1_nm == "서울특별시")

dim(real_data) # 10,875
min(real_data$reg_date)

real_data$reg_date <- as.Date(real_data$reg_date)

var <-  c('hour_reg', 'pick_rgn1_nm', 'pick_rgn2_nm')
real_data[,var]<- lapply(real_data[,var], factor)

real_data <- real_data %>% 
rename("rider_cnt_w_1" = "라이더수",
        "order_cnt_w_1" = "주문수")

table(real_data$hour_reg)
table(real_data$hour_reg, real_data$pick_rgn2_nm) #na 없음

# 변수 생성 
# month, week,day_of_reg, is_rain, is_holiday, rider_cnt_w_1~w_4, order_cnt_w_1~w_4

real_data <- real_data %>% 
mutate(month = month(reg_date),
        week = ceiling(day(reg_date) /7),
         day_of_reg = substr(weekdays(reg_date),1,3))

real_data <- real_data %>% 
arrange(reg_date) %>% 
group_by(pick_rgn2_nm, day_of_reg, hour_reg) %>%
mutate(rider_cnt_w_2 = lag(rider_cnt_w_1, n=1),
        rider_cnt_w_3 = lag(rider_cnt_w_1, n=2),
        rider_cnt_w_4 = lag(rider_cnt_w_1, n=3),
        order_cnt_w_2 = lag(order_cnt_w_1, n=1),
        order_cnt_w_3 = lag(order_cnt_w_1, n=2),
        order_cnt_w_4 = lag(order_cnt_w_1, n=3))

real_data <- real_data %>% 
filter(reg_date >='2023-05-01' & '2023-05-07' >= reg_date)
dim(real_data) #2,625
colSums(is.na(real_data)) 


real_data <- real_data %>% 
mutate(reg_date = reg_date + 7)

min(real_data$reg_date)
max(real_data$reg_date)
table(real_data$reg_date)

real_data <- real_data %>% 
ungroup() %>% 
mutate(is_rain = ifelse(reg_date == "2023-05-12" | reg_date == "2023-05-14", 1, 0),
       is_holiday = ifelse(reg_date == "2023-05-13" | reg_date == "2023-05-14", 1, 0))

table(real_data$is_rain)
table(real_data$is_holiday)


# apply model

str(real_data)

test_data <- subset(real_data, select = -c(reg_date, pick_rgn1_nm))

var <-  c('day_of_reg', 'is_rain', 'is_holiday', 'month','week')
test_data[,var]<- lapply(test_data[,var], factor)

str(test_data)

prediction <- predict(model_pv, newdata = test_data, interval = 'prediction', level = 0.90)

result <- data.frame(real_data$reg_date, real_data$pick_rgn2_nm, real_data$hour_reg, prediction)
write.csv(result, "precdict.csv", row.names = FALSE, fileEncoding = "cp949")
