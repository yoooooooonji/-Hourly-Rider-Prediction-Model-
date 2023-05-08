train_data <- read.csv("final_data.csv", fileEncoding = "cp949")
train_data$datetime <- as.POSIXct(train_data$datetime)

test <- train_data[c("datetime", "rider_cnt_2")]
plot(test)
str(test)

ts(test, frequency = 1, start = c(min(test$datetime), max(test$datetime)))
decompose(test)
