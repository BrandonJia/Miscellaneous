

walmart_train <-read.csv("C:/Users/sarat/Desktop/Spring/Causal Inference/Project/train.csv")

store_data <- read.csv("C:/Users/sarat/Desktop/Spring/Causal Inference/Project/stores.csv")

features <- read.csv('C:/Users/sarat/Desktop/Spring/Causal Inference/Project/features.csv')

#Make similar field to combine features and store data
walmart_train$concat <- paste(walmart_train$Store, walmart_train$Date, sep = '/')
features$concat <- paste(features$Store, features$Date, sep = '/')

#combine datasets
train_store <- merge(x = walmart_train, y = store_data, by = "Store", all = TRUE)
all <- merge(x = train_store, y = features, by = "concat", all = TRUE)
all_1 <- all[!duplicated(all),]
all <- data.frame(all)
summary(all_1)

View(all)

#write.csv(all, file = "CombinedWalmartData.csv")

all_2 <- all_1[complete.cases(all_1), ]

sapply(all_2,class)
# there are 45 stores 
all_3 <- all_2[,c("concat"     ,  "Store.x"     , "Dept"      ,   "Date.x"     ,  "Weekly_Sales",
                  "IsHoliday.x"  ,"Type"       ,  "Size"  ,
                  "Temperature"  ,"Fuel_Price" ,  "MarkDown1",    "MarkDown2",    "MarkDown3"   ,
                  "MarkDown4"   , "MarkDown5",    "CPI"       ,   "Unemployment")]

names(all_3) <- c("concat"     ,  "Store"     , "Dept"      ,   "Date"     ,  "Weekly_Sales",
                  "IsHoliday.x"  ,"Type"       ,  "Size"  ,
                  "Temperature"  ,"Fuel_Price" ,  "MarkDown1",    "MarkDown2",    "MarkDown3"   ,
                  "MarkDown4"   , "MarkDown5",    "CPI"       ,   "Unemployment")
library(chron)
all_3$Date<- as.Date(all_3$Date)
sapply(all_3,class)
all_3$Store<- as.factor(all_3$Store)
all_3$Dept<- as.factor(all_3$Dept)

# Its mostly at 8
hist(all_3$Unemployment)

# total Stores - 45 stores
length(unique(all_3$Store))
# we dont care about department 

## No idea what markdown is - probably an indicator of economy 
hist(all_3$MarkDown1)

hist(all_3$MarkDown2)

hist(all_3$MarkDown3)

hist(all_3$MarkDown4)

## why is CPI distribution weird?
hist(all_3$CPI)

hist(all_3[all_3$Store==15,]$CPI)

### Fuel Rate - perfect histogram 

hist(all_3$Fuel_Price)

##Total Time - 350 days almost an year 
max(all_3$Date)- min(all_3$Date)

median(all_3$Unemployment)

##Median  unemployement is 7.28. If greater then 1 else 0  
all_3$median_unemployement <- ifelse(all_3$Unemployment>7.28,1,0)

### Stores with both high and low - only 9 stores have both high and low. 36 stores only have one value
library(dplyr)
all_3 %>% group_by(Store) %>% summarise(n=n_distinct(median_unemployement)) %>% filter(n==2) 

##
library(ggplot2)
ggplot(all_3[all_3$Store==35,],aes(x=Date,y=Unemployment))+geom_line()
ggplot(all_3[all_3$Store==35,],aes(x=Date,y=Weekly_Sales))+geom_line()





