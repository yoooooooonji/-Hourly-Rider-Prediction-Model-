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

dim(data) 

data <- data %>% 
filter(is_rain ==1 & is_holiday ==1)

dim(data) # 5775

data <- data  %>% 
mutate(cnt = order_cnt/rider_cnt,
        month = month(reg_date))

summary(data$cnt)
sd(data$cnt)


month1 <- data %>% 
group_by(month) %>% 
summarise(mean = mean(cnt),
            sd = sd(cnt))
month1

hour1 <- data %>% 
group_by(hour_reg) %>% 
summarise(mean = mean(cnt),
            sd = sd(cnt))
hour1


df <- read.csv("combined_data.csv", fileEncoding= "cp949")
tt <- df  %>% filter(is_rain !=1)

tt <- tt  %>% 
mutate(cnt = order_cnt/rider_cnt,
        month = month(reg_date))

summary(tt$cnt) #1.8
sd(tt$cnt) #0.33


month2 <- tt %>% 
group_by(month) %>% 
summarise(mean = mean(cnt),
            sd = sd(cnt))
month2

hour2 <- tt %>% 
group_by(hour_reg) %>% 
summarise(mean = mean(cnt),
            sd = sd(cnt))
hour2

month1
month2

hour1
hour2

