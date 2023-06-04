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

##################################################################################################################
# dataload
qual <- read_excel("/Users/yj.noh/Desktop/quality_predict.xlsx") 
predict <- read_excel("/Users/yj.noh/Desktop/predict_latest6days.xlsx") 
weather <- read.csv("/Users/yj.noh/Desktop/weather_2023.csv", fileEncoding = "cp949") 

head(qual)
head(predict)

qual$reg_date <- as.Date(qual$reg_date)
predict$reg_date <- as.Date(predict$reg_date)
qual <- qual  %>% filter(pick_rgn1_nm == '서울특별시')
qual <- qual  %>% mutate(day_of_reg = weekdays(reg_date))
qual <- qual  %>% rename("rider" = "운행 라이더 수")

str(qual)
str(predict)

weather <- weather  %>% 
mutate(hour_reg = hour(일시), 일시 = as.Date(일시)) %>% 
rename ("temp" = "기온..C.",
        "rain" = "강수량.mm.",
        "snow_c" = "적설.cm.")
head(weather)

weather$rain[is.na(weather$rain)] <-0
weather$snow_c[is.na(weather$snow_c)] <-0

qual <- left_join(qual, weather[c("일시", "rain","snow_c", "hour_reg")], by = c("reg_date" = "일시", "hour_reg" = "hour_reg"))
qual <- qual  %>% mutate(is_rain = ifelse(rain >0 | snow_c >0, 1,0))

qual <- subset(qual, select = -c(reg_date, pick_rgn1_nm, rain, snow_c))
predict <- subset(predict, select = -c(is_holiday, y_pred_linear, y_pred_LGBM, 보정값_linear))

str(qual)
str(predict)


full_data <- full_join(predict, qual, by = c("pick_rgn2_nm" = "pick_rgn2_nm", "day_of_reg" = "day_of_reg", "is_rain" = "is_rain"))

full_data <- full_data  %>% 
mutate(order_diff = abs(order_cnt_predict - 주문수), 
      rider_diff = abs(보정값_LGBM - rider))

full_data <- full_data  %>% 
mutate(multiple = order_diff * rider_diff)

full_data <- full_data %>%
group_by(reg_date, pick_rgn2_nm, hour_reg.x, day_of_reg, is_rain, 보정값_LGBM, order_cnt_predict)  %>% 
arrange(multiple)
