library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
require(stargazer)
require(plm)
require(lfe)


train <-read.csv("train.csv")
store <- read.csv("stores.csv")
features <- read.csv('features.csv')

data <- train %>% merge(store, by = "Store", all = TRUE) %>% 
  merge(features, by = "concat", all = TRUE)
 
write.csv(data, file = "walmartData.csv") 

data <- fread('walmartData.csv')

glimpse(data)

within_reg2 <- felm(data=data, Weekly_Sales~Unemployment | store)
