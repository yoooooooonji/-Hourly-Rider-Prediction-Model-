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
main_all <- read.table("/Users/yj.noh/Desktop/main_all.tsv", sep = "\t", header = TRUE)
main_peak <- read.table("/Users/yj.noh/Desktop/main_peak.tsv", sep = "\t", header = TRUE)
part_all <- read.table("/Users/yj.noh/Desktop/part_all.tsv", sep = "\t", header = TRUE)
part_peak <- read.table("/Users/yj.noh/Desktop/part_peak.tsv", sep = "\t", header = TRUE)

n_distinct(main_all$rider_id) #8959
n_distinct(main_peak$rider_id) #16119
n_distinct(part_all$rider_id) #9903
n_distinct(part_peak$rider_id) #25977

main_all <- main_all %>% filter(!rider_id %in% c(main_peak$rider_id, part_all$rider_id, part_peak$rider_id))
main_peak <- main_peak  %>% filter(!rider_id %in% c(main_all$rider_id, part_all$rider_id, part_peak$rider_id))
part_all <- part_all %>% filter(!rider_id %in% c(main_all$rider_id, main_peak$rider_id, part_peak$rider_id))
part_peak <- part_peak  %>% filter(!rider_id %in% c(main_all$rider_id, part_all$rider_id, main_peak$rider_id))

n_distinct(main_all$rider_id) #8533
n_distinct(main_peak$rider_id) #13608
n_distinct(part_all$rider_id) # 8768
n_distinct(part_peak$rider_id) #25977

data <- rbind(main_all, main_peak, part_all, part_peak)
n_distinct(data$rider_id) # 56886

data$business_day <- as.Date(data$business_day)
data <- data  %>% arrange(rider_id, business_day, rgn1_nm, rgn2_nm)

# 요일/ 공휴일 유무
holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09",  "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", 
"2022-10-03",  "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21","2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-01", "2023-05-05","2023-05-27", "2023-05-29", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
"2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))

data <- data %>% 
mutate(weekday = weekdays(as.Date(business_day)),
        is_holiday = ifelse(business_day %in% holiday_list, 1,0))
table(data$is_holiday)

var <-  c('weekday', 'is_holiday')
data[,var]<- lapply(data[,var], factor)

rider_id_values <- unique(data$rider_id)
glm_result <- data.frame(rider_id_values= character(), accuracy = numeric(), sensitivity = numeric(), specificity = numeric(), best_threshold = numeric(), coef = numeric(), stringsAsFactors = FALSE)


get_glm <- function(data) {
  model_id <- glm(outcome ~ weather_type + weekday + is_holiday, data = data, family = "binomial")
  predicted_prob <- predict(model_id, type = "response")
  
  thresholds <- seq(0, 1, by = 0.01)  # 임계값 범위 설정
  accuracy_sensitivity <- rep(0, length(thresholds))  # 정확도 + 민감도의 합을 저장할 벡터
  
  for (i in 1:length(thresholds)) {
    predicted <- ifelse(predicted_prob > thresholds[i], 1, 0)
    accuracy_sensitivity[i] <- sum(predicted == data$outcome) / nrow(data) + sum(predicted[data$outcome == 1] == 1) / sum(data$outcome == 1)
  }  
  best_threshold <- thresholds[which.max(accuracy_sensitivity)]  # 정확도 + 민감도 합이 최대인 임계값 선택
  predicted <- ifelse(predicted_prob > best_threshold, 1, 0)
  accuracy <- sum(predicted == data$outcome) / nrow(data)
  sensitivity <- sum(predicted[data$outcome == 1] == 1) / sum(data$outcome == 1) # 1을 1로 예측한 비율 
  specificity <- sum(predicted[data$outcome == 0] == 0) / sum(data$outcome == 0)
  coef <- coefficients(model_id)
  print(c(rider_nm, accuracy, sensitivity, specificity, best_threshold, coef))
  result <- data.frame(accuracy, sensitivity, specificity, best_threshold, coef)
  result
}

for (rider_nm in rider_id_values) {
  filtered_data <- data %>%
    filter(rider_id == rider_nm)
   result_value <- get_glm(filtered_data)
   result_row <- data.frame(rider_nm = rider_nm, result_value)
   glm_result <- rbind(glm_result, result_row)
}

glm_result <- write.csv(glm_result, "results_glm.csv", fileEncoding = "cp949")

# group 별 비교 
glm_result <- read.csv("results_glm.csv", fileEncoding = "cp949")

glm_result <- glm_result[!duplicated(glm_result[c("rider_nm")]),]
data <- data[!duplicated(data[c("rider_id")]),]

glm_result <- left_join(glm_result, data[c("rider_id", "cluster", "rider_delivery_method")], by = c("rider_nm" = "rider_id"))

glm_result <- subset(glm_result , select = -c(X, coef))
glm_result <- write.csv(glm_result, "results_glm_filter.csv", fileEncoding = "cp949")
