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
data <- read_excel("/Users/yj.noh/Desktop/need_rider.xlsx") 
head(data)

correlation <- cor (data[c(2,3,4,5,6,7,8,9,10,11,12)])
print(correlation)

# 배달 인프라
# cut-off 값 설정
cut_off <- 5.0

# cut-off 값 이하인 데이터 필터링
filtered_data <- data[data$배달인프라_점수<= cut_off, ]

# 비율 계산
주문수_합 <- sum(filtered_data$배달건수)
라이더수_합 <- sum(filtered_data$운행_라이더수)
비율 <- 주문수_합 / 라이더수_합

# 결과 출력
print(비율)

colSums(is.na(data))

model <- lm(배차소요시간 ~ ord_cnt + rider_cnt, data = data )
summary(model)

# 0.9437*배달_라이더_비율-7.6824 
 
model2 <- lm(배달인프라_점수 ~ 배달_라이더_비율, data = data)
summary(model2)

model3 <- lm(배차소요시간 ~ 배달_라이더_비율, data = data)
summary(model3)

model4 <- lm(배달인프라_점수 ~ 배달건수 + 운행_라이더수, data = data)
summary(model4)

# 배차소요시간 = 배달건수*(0.0000041583) + 운행 라이더수 * -0.001622072 + 38.967420739
# 배달인프라 점수 배달건수 * (0.00002747) + 운행 라이더수 * (-0.00107829) + 24.84904628 


data <- data  %>% 
mutate(rider_2 = 운행_라이더수^2,
       ord_2 = 배달건수^2)

model5 <- lm(배차소요시간 ~ 운행_라이더수 + 배달건수 + rider_2 + ord_2  + 배달_라이더_비율, data= data)
summary(model5)

coefficients(model5)
