# Load packages
library(dplyr)
library(lubridate)
library(tidyr)


## Dir: external_metrics
aws_billing <- read.csv("external_metrics/aws_billing_from_2012_10.csv", stringsAsFactors = FALSE)

str(aws_billing) # 1584 obs. of  12 variables

aws_billing$UsageStartDate <- ymd_hms(aws_billing$UsageStartDate)
aws_billing$UsageEndDate <- ymd_hms(aws_billing$UsageEndDate)


head(aws_billing)
summary(aws_billing)

# Hist
aws_billing %>%
  ggplot(aes(x = TotalCost)) +
  geom_histogram()


# Total Cost per Product
aws_billing %>% group_by(ProductCode) %>%
  summarise(sum_price = sum(TotalCost)) %>%
  summary()

aws_billing %>% group_by(ProductCode) %>%
  summarise(sum_TotalCost = sum(TotalCost)) %>%
  ggplot(aes(x =ProductCode, y = sum_TotalCost)) +
  geom_bar(stat = "identity")

aws_billing %>% group_by(ProductCode) %>%
  summarise(sum_TotalCost = sum(TotalCost)) %>%
  ggplot(aes(x =ProductCode, y = sum_TotalCost)) +
  geom_boxplot()



# Total Cost per Product and Usage Type
aws_billing %>% group_by(ProductCode, UsageType) %>%
  summarise(sum_TotalCost = sum(TotalCost)) %>%
  summary()

aws_billing %>% group_by(ProductCode, UsageType) %>%
  summarise(sum_TotalCost = sum(TotalCost)) %>%
  filter(sum_TotalCost)

ggplot(aes(x = UsageType, y = sum_price)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ProductCode)



  
  

  
