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

predict <- predict  %>% filter(날짜 < '2023-05-08')

library(Metrics)

predict <- left_join(predict, answer[c("reg_date", "hour_reg", "pick_rgn2_nm", "라이더수")] , by = c("날짜" = "reg_date", "시간대" = "hour_reg", "지역구" = "pick_rgn2_nm"))

predict <- predict  %>% 
mutate(error = abs(예측값 - 라이더수))

write.csv(predict, "predict_data.csv", row.names = FALSE, fileEncoding = "cp949")

mae(predict$예측값, predict$라이더수) # 39.32

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
