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
# data load
data <- read_excel("/Users/yj.noh/Desktop/need_rider.xlsx")
data <- data %>% filter(dlvry_rgn1_nm == '서울특별시')

data <- data %>% mutate(ord_by_rider = ord_cnt/rider_cnt,
                        outcome = ifelse(avg_dlvry_diff > 25.6, 1, 0))

table(data$outcome, data$dlvry_rgn2_nm) 
data$outcome <- as.factor(data$outcome)

# t-test
test_result <- t.test(data$outcome, data$ord_by_rider, var.equal = FALSE)
test_result  #p-value 2.2e-16


#a1 = ROC(form= outcome ~ ord_by_rider,data=data,plot="ROC")

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
  result <- data.frame(AUC = auc_value, Sensitivity = sens_value, Specificity = spec_value, optimal_ord_by_rider = cutoff_value)
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

