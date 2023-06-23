# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "pROC", "readxl", "MASS", "Epi", "caTools", "DAAG", "caret", "psych") # nolint
ipak(pkg)

##########################################################################################################################################################
# day 
# data load
data_day <- read_excel("/Users/yj.noh/Desktop/need_rider_day_rgn1.xlsx")

head(data_day)
dim(data_day)
str(data_day)

min(data_day$business_day)
max(data_day$business_day)

table(data_day$dlvry_rgn1_nm)

# 파생변수 생성 - 요일, 주문수 대비 라이더 수 
data_day<- data_day  %>% 
mutate(weekday = weekdays(as.Date(business_day)),
        ord_by_rider = ord_cnt/rider_cnt)

var <-  c('weekday', 'dlvry_rgn1_nm')
data_day[,var]<- lapply(data_day[,var], factor)

rgn1_nm_values <- unique(data_day$dlvry_rgn1_nm)
results_dlvry <- data.frame(rgn1_nm = character(), r2 = numeric(), coef = numeric(),  stringsAsFactors = FALSE)
results_consign <- data.frame(rgn1_nm = character(), r2 = numeric(), coef = numeric(),  stringsAsFactors = FALSE)
result_20min <- data.frame(rgn1_nm = character(), r2 = numeric(), coef = numeric(),  stringsAsFactors = FALSE)

# 평배시 모델
get_dlvry_lm <- function(data){
       model_dlvry <- lm(avg_dlvry_diff ~ rider_cnt + ord_cnt + weekday + mean_fee, data = data)
        r2 <- summary(model_dlvry)$adj.r.squared
        coef <- coefficients(model_dlvry)
        result <- data.frame(r2, coef)
        result
}

# 배차소요시간 모델
get_consign_lm <- function(data){
        model_consign <- lm(avg_consign_diff ~ rider_cnt + ord_cnt + weekday + mean_fee, data = data)
        r2 <- summary(model_consign)$adj.r.squared
        coef <- coefficients(model_consign)
        result <- data.frame(r2, coef)
        result
}

# 장미배 20분 초과 비율
get_over_20_lm <- function(data){
       model_over20 <- lm(over_20 ~ rider_cnt + ord_cnt + weekday + mean_fee, data = data)
        r2 <- summary(model_over20)$adj.r.squared
        coef <- coefficients(model_over20)
        result <- data.frame(r2, coef)
        result
}

for (rgn1_nm in rgn1_nm_values) {
  filtered_data <- data_day %>%
    filter(dlvry_rgn1_nm == rgn1_nm)
   result_value <- get_dlvry_lm(filtered_data)
   result_row <- data.frame(rgn1_nm = rgn1_nm, result_value)
   results_dlvry <- rbind(results_dlvry, result_row)
}

results_dlvry <- write.csv(results_dlvry, "results_dlvry.csv", fileEncoding = "cp949")

for (rgn1_nm in rgn1_nm_values) {
  filtered_data <- data_day %>%
    filter(dlvry_rgn1_nm == rgn1_nm)
   result_value <- get_consign_lm(filtered_data)
   result_row <- data.frame(rgn1_nm = rgn1_nm, result_value)
   results_consign <- rbind(results_consign, result_row)
}

results_consign <- write.csv(results_consign, "results_consign.csv", fileEncoding = "cp949")


for (rgn1_nm in rgn1_nm_values) {
  filtered_data <- data_day %>%
    filter(dlvry_rgn1_nm == rgn1_nm)
   result_value <- get_over_20_lm(filtered_data)
   result_row <- data.frame(rgn1_nm = rgn1_nm, result_value)
   results_over_20 <- rbind(results_over_20, result_row)
}

results_over_20 <- write.csv(results_over_20, "results_over_20.csv", fileEncoding = "cp949")



# seoul만
se <- data_day  %>% filter(dlvry_rgn1_nm == '서울특별시')

model_dlvry <- lm(avg_dlvry_diff ~ mean_fee, data = se)
summary(model_dlvry)

y_pred <- predict(model_dlvry, newdata = se)
library(Metrics)

mae(y_pred, se$avg_dlvry_diff) #0.60
rmse(y_pred, se$avg_dlvry_diff) #0.67 

model_all <- lm(avg_dlvry_diff ~ ord_cnt + rider_cnt + weekday + mean_fee + min_fee + max_fee, data =se)
y_pred <- predict(model_all, newdata = se)
summary(model_all)
mae(y_pred, se$avg_dlvry_diff) #0.34
rmse(y_pred, se$avg_dlvry_diff) #0.40 
