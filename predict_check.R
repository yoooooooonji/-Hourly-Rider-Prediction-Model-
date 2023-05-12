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
predict <- read_excel("/Users/yj.noh/Desktop/predict_data.xlsx", sheet = 1) 
answer <- read_excel("/Users/yj.noh/Desktop/answer_data.xlsx")

predict$날짜 <- as.Date(predict$날짜)
answer$reg_date <- as.Date(answer$reg_date)

min(predict$날짜)
max(predict$날짜)
min(answer$reg_date)
max(answer$reg_date)

answer <- answer  %>% filter(pick_rgn1_nm == '서울특별시' & hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

predict <- predict  %>% filter(시간대 %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

dim(predict)
dim(answer)

library(Metrics)

predict <- left_join(predict, answer[c("reg_date", "hour_reg", "pick_rgn2_nm", "라이더수")] , by = c("날짜" = "reg_date", "시간대" = "hour_reg", "지역구" = "pick_rgn2_nm"))

head(predict)

predict <- predict  %>% 
mutate(error = abs(예측값 - 라이더수))

colSums(is.na(predict))

write.csv(predict, "predict_data.csv", row.names = FALSE, fileEncoding = "cp949")

mae(predict$예측값, predict$라이더수) # 35.98

# 일자별
result_day <- aggregate(abs(predict$예측값 - predict$라이더수), 
                    by = list(predict$날짜), 
                    FUN = mean)

result_day

# 시간대별 
result_hour <- aggregate(abs(predict$예측값 - predict$라이더수), 
                    by = list(predict$시간대), 
                    FUN = mean)
result_hour

# 지역별
result_rgn2 <- aggregate(abs(predict$예측값 - predict$라이더수), 
                    by = list(predict$지역구), 
                    FUN = mean)
result_rgn2

# 지역별, 시간대별 
result_total <- aggregate(abs(predict$예측값 - predict$라이더수), 
                    by = list(predict$지역구, predict$시간대), 
                    FUN = mean)
result_total

write.csv(result_total, "result_total.csv", row.names = FALSE, fileEncoding = "cp949")







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
gn <- read_excel("/Users/yj.noh/Desktop/check_data.xlsx", sheet = 1) 
sc <- read_excel("/Users/yj.noh/Desktop/check_data.xlsx", sheet = 2)

data <- rbind(gn,sc)
dim(data)
head(data)

data$datetime <- as.Date(data$datetime)
data$hour <- as.factor(hour(data$hour))

# total
library(Metrics)
mae(data$actual, data$forecast_arima) #43.87
mae(data$actual, data$forecast_lm) #32.61

# 일자별
result_day <- aggregate(cbind(abs(data$forecast_arima - data$actual), 
                               abs(data$forecast_lm - data$actual)),
                        by = list(data$datetime), 
                        FUN = mean)
result_day

# 시간대별 
result_hour <- aggregate(cbind(abs(data$forecast_arima - data$actual), 
                               abs(data$forecast_lm - data$actual)),
                        by = list(data$hour), 
                        FUN = mean)
result_hour
table(data$hour)
# 지역별
result_rgn2 <- aggregate(cbind(abs(data$forecast_arima - data$actual), 
                               abs(data$forecast_lm - data$actual)),
                        by = list(data$pick_rgn2_nm),
                    FUN = mean)
result_rgn2

# 지역별, 시간대별 
result_total <- aggregate(cbind(abs(data$forecast_arima - data$actual), 
                               abs(data$forecast_lm - data$actual)),
                    by = list(data$pick_rgn2_nm, data$hour), 
                    FUN = mean)
result_total
