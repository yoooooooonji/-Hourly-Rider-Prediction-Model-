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


data <- read_excel("/Users/yj.noh/Desktop/AI_accept.xlsx")
head(data)

data  %>% 
 tbl_summary(by = crm_group, 
              type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
              missing_text = "(Missing value)", 
              digits = list(all_continuous() ~ c(1, 2), 
                            all_categorical() ~ c(0, 1))) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()

str(data)
data$시간대 <- as.factor(data$시간대)
data$crm_group <- as.factor(data$crm_group)
data$reward_yn <- as.factor(data$reward_yn)
data$cluster <- as.factor(data$cluster)

data  %>% 
tbl_strata(
    strata = crm_group,~.x  %>% 
    tbl_summary(
        by = reward_yn,
        type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
              missing_text = "(Missing value)", 
              digits = list(all_continuous() ~ c(1, 2), 
                            all_categorical() ~ c(0, 1))) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()
)
