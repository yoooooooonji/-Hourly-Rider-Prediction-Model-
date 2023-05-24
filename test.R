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

#############################################################

data <- read.csv("combined_data.csv", fileEncoding= "cp949")

data[c("is_rain", "rider_cnt_2", "order_cnt")] %>% tbl_summary(by="is_rain") %>% add_p()
data[c("is_holiday", "rider_cnt_2", "order_cnt")] %>% tbl_summary(by="is_holiday") %>% add_p()

tt <- data %>% filter(pick_rgn2_nm == '강남구')

tt[c("is_rain", "rider_cnt_2", "order_cnt")] %>% tbl_summary(by="is_rain") %>% add_p()
tt[c("is_holiday", "rider_cnt_2", "order_cnt")] %>% tbl_summary(by="is_holiday") %>% add_p()

ss <- data %>% filter(pick_rgn2_nm == '용산구')

ss[c("is_rain", "rider_cnt_2", "order_cnt")] %>% tbl_summary(by="is_rain") %>% add_p()
ss[c("is_holiday", "rider_cnt_2", "order_cnt")] %>% tbl_summary(by="is_holiday") %>% add_p()

data$is_rain <- as.factor(data$is.rain)
data$is_holiday <- as.factor(data$is_holiday)
data$pick_rgn2_nm <- as.factor(data$pick_rgn2_nm)

# Kruskal-Wallis 검정을 위해 kruskal.test 함수 사용
model <- aov(rider_cnt_2~ pick_rgn2_nm*is_rain*is_holiday, data = data)
summary(model)
