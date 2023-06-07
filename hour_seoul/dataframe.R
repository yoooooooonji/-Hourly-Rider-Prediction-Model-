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


data1 <- read.csv("/Users/yj.noh/Desktop/data_20230507.csv", fileEncoding = "utf-16") # 849409
data2 <- read.csv("/Users/yj.noh/Desktop/data_20230515.csv", fileEncoding = "cp949")  # 842282
data3 <- read.csv("/Users/yj.noh/Desktop/data_20230522.csv", fileEncoding = "cp949")  # 753192
data4 <- read.csv("/Users/yj.noh/Desktop/data_20230531.csv", fileEncoding = "cp949")  # 981935

data <- rbind(data1,data2,data3,data4)
write.csv(data, "data.csv", row.names = FALSE, fileEncoding = "cp949")
