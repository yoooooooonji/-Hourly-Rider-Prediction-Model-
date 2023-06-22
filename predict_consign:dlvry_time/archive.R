# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "pROC", "readxl", "MASS", "Epi") # nolint
ipak(pkg)

##########################################################################################################################################################
# rgn1 
# data load
data <- read_excel("/Users/yj.noh/Desktop/need_rider_rgn1.xlsx")
data <- data %>% filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) & dlvry_rgn1_nm == '서울특별시')

data <- data  %>% 
mutate(weekday = weekdays(as.Date(reg_date)),
        ord_by_rider = ord_cnt/rider_cnt)

table(data$weekday)
head(data)

cor(data[c("ord_cnt", "rider_cnt","avg_consign_diff", "avg_dlvry_diff","min_fee", "mean_fee","max_fee", "ord_by_rider")])

# lm_model
data$hour_reg <- as.factor(data$hour_reg)
data$weekday <- as.factor(data$weekday)
data$dlvry_rgn1_nm <- as.factor(data$dlvry_rgn1_nm)

model1 <- lm(avg_consign_diff ~  mean_fee + ord_by_rider + hour_reg + weekday, data = data)
summary(model1)

##########################################################################################################################################################
# rgn2
# data load
data <- read_excel("/Users/yj.noh/Desktop/need_rider_rgn2.xlsx")
data <- data %>% filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) & dlvry_rgn1_nm == '서울특별시')

data <- data  %>% 
mutate(weekday = weekdays(as.Date(reg_date)),
        ord_by_rider = ord_cnt/rider_cnt)

table(data$weekday)
head(data)

cor(data[c("ord_cnt", "rider_cnt","avg_consign_diff", "avg_dlvry_diff","min_fee", "mean_fee","max_fee", "ord_by_rider")])

# lm_model
data$hour_reg <- as.factor(data$hour_reg)
data$weekday <- as.factor(data$weekday)
data$dlvry_rgn2_nm <- as.factor(data$dlvry_rgn2_nm)
data$dlvry_rgn1_nm <- as.factor(data$dlvry_rgn1_nm)

model1 <- lm(avg_dlvry_diff ~ rider_cnt + ord_cnt + mean_fee + ord_by_rider + hour_reg + weekday + dlvry_rgn2_nm, data = data)
summary(model1)

# hour - rgn1 
# data load
data_rgn1 <- read_excel("/Users/yj.noh/Desktop/need_rider_rgn1.xlsx")
data_rgn1 <- data_rgn1  %>% filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
dim(data_rgn1)
str(data_rgn1)

data_rgn1 <- data_rgn1  %>% 
mutate(weekday = weekdays(as.Date(reg_date)),
        ord_by_rider = ord_cnt/rider_cnt)

var <-  c('weekday', 'hour_reg')
data_rgn1[,var]<- lapply(data_rgn1[,var], factor)

# model
model3 <- lm(avg_dlvry_diff ~ rider_cnt + ord_cnt + weekday + mean_fee + hour_reg , data = data_rgn1)
summary(model3)
coefficients(model3)

model4 <- lm(avg_consign_diff ~ rider_cnt + ord_cnt + weekday + mean_fee + hour_reg, data = data_rgn1)
summary(model4)
coefficients(model4)

# hour - rgn2 
# data load
data_rgn2 <- read_excel("/Users/yj.noh/Desktop/need_rider_rgn2.xlsx")
data_rgn2 <- data_rgn2  %>% filter(hour_reg %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

dim(data_rgn2)
str(data_rgn2)

data_rgn2 <- data_rgn2  %>% 
mutate(weekday = weekdays(as.Date(reg_date)),
        ord_by_rider = ord_cnt/rider_cnt)

var <-  c('weekday', 'hour_reg', 'dlvry_rgn2_nm')
data_rgn2[,var]<- lapply(data_rgn2[,var], factor)

# model
model5 <- lm(avg_dlvry_diff ~ rider_cnt + ord_cnt + weekday + mean_fee + hour_reg + dlvry_rgn2_nm + min_fee + max_fee, data = data_rgn2)
summary(model5)
coefficients(model5)

model6 <- lm(avg_consign_diff ~ rider_cnt + ord_cnt + weekday + mean_fee + hour_reg + dlvry_rgn2_nm + min_fee + max_fee, data = data_rgn2)
summary(model6)
coefficients(model6)


# 배달시간 25.6분 이상이면 1, 아니면 0 
data <- data %>% mutate(ord_by_rider = ord_cnt/rider_cnt,
                        outcome = ifelse((weekday %in% c("월요일","화요일", "수요일","목요일", "금요일") & avg_dlvry_diff > 26.0) | (weekday %in% c("토요일","일요일") & avg_dlvry_diff > 24.9), 1, 0))

table(data$outcome, data$dlvry_rgn2_nm) 
table(data$outcome) # 0: 6266, 1 : 6025
data$outcome <- as.factor(data$outcome)

a1 = ROC(form= outcome ~ ord_by_rider,data=data,plot="ROC")

optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}

optimal_cutpoint=function(x){
   y=optimal_lr.eta(x)
   b0=unname(x$lr$coeff[1])
   b1=unname(x$lr$coeff[2])
   result=(-log(1/y-1)-b0)/b1

   result
}

get_optimal_ord_auc_sens_spec <- function(group_data) {
  roc_obj <- ROC(form = outcome ~ ord_by_rider, data = group_data)
  max_index <- which.max(roc_obj$res$sens + roc_obj$res$spec) 
  cutoff_value <- optimal_cutpoint(roc_obj)
  auc_value <- roc_obj$AUC
  sens_value <- roc_obj$res$sens[max_index] 
  spec_value <- roc_obj$res$spec[max_index]
  result <- data.frame(AUC = auc_value, Sensitiv2ity = sens_value, Specificity = spec_value, optimal_ord_by_rider = cutoff_value)
  roc_obj
  result
}

rgn2_nm_values <- unique(data$dlvry_rgn2_nm)
results <- data.frame(rgn2_nm = character(), AUC = numeric(), Sensitivity = numeric(), Specificity = numeric(), optimal_ord_by_rider = numeric(), stringsAsFactors = FALSE)

for (rgn2_nm in rgn2_nm_values) {
  filtered_data <- data %>%
    filter(dlvry_rgn2_nm == rgn2_nm)
  optimal_ord_auc_sens_spec <- get_optimal_ord_auc_sens_spec(filtered_data)
  result_row <- data.frame(rgn2_nm = rgn2_nm, optimal_ord_auc_sens_spec)
  results <- rbind(results, result_row)
}

results

