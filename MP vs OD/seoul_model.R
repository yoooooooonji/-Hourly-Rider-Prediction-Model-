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
# seoul
seoul <- read_excel("/Users/yj.noh/Desktop/mpvsod.xlsx", sheet = 1)
seoul <- seoul  %>% filter(시간대 %in% c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
seoul$일자 <- as.Date(seoul$일자, format =  "%Y.%m.%d")

head(seoul)
dim(seoul) #10848
str(seoul)

seoul <- seoul  %>% rename("outcome" = "MP/OD",
                            "MP_ord" = "실제 MP 주문수",
                            "OD_ord" = "실제 OD 주문수",
                            "hour" = "시간대")

seoul$MP_ord <- as.numeric(seoul$MP_ord)
seoul$OD_ord <- as.numeric(seoul$OD_ord)
seoul$outcome<- as.numeric(seoul$outcome)


# 요일/ 공휴일 유무
holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09",  "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", 
"2022-10-03",  "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21","2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-01", "2023-05-05","2023-05-27", "2023-05-29", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
"2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))

seoul <- seoul %>% 
mutate(weekday = weekdays(as.Date(일자)),
        is_holiday = ifelse(일자 %in% holiday_list, 1,0))

table(seoul$is_holiday)

var <-  c('weekday', 'is_holiday', 'hour', 'rgn2')
seoul[,var]<- lapply(seoul[,var], factor)

train <- seoul  %>% filter(일자 <= '2023-06-28')
test <- seoul  %>% filter(일자 == '2023-06-29') %>% arrange(일자, rgn2, hour)

model <- lm(outcome ~ hour + MP_ord + OD_ord + weekday + rgn2 , data = train)
summary(model)

y_pred <- predict(model, newdata = test)
test <- test  %>% mutate(y_pred = y_pred)

write.csv(test, "seoul_pred.csv", fileEncoding = "cp949")

