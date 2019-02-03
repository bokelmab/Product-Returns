#required libraries
if(!require("cluster")) install.packages("cluster"); library("cluster")
library(data.table)
library(dplyr)
library(magrittr)
library(lubridate)

## read data
data <- fread('Data/BADS_WS1718_known.csv')

### product-specific features -----------------------------------------

## total amount of orders per product
data$orders_pid <- data %$% ave(x = item_id, item_id, FUN = length)

## average return rate per product
data$returns_pid <- data %$% ave(x = return, item_id, FUN = sum)
data$return_rate_pid <- (data$returns_pid - data$return) / (data$orders_pid - 1)

## convert size and color
data$item_size %<>% as.factor
data$item_color %<>% as.factor


### customer-specific features ---------------------------------------

## total amount of orders per product
data$orders_user <- data %$% ave(x = user_id, user_id, FUN = length)

## average return rate
data$returns_user <- data %$% ave(x = return, user_id, FUN = sum)
data$return_rate_user <- (data$returns_user - data$return) / (data$orders_user - 1)

## creates the variable age (at time of order) out of user_dob and order_date
data$user_dob %<>% as_date
data$age <- year(data$order_date) - year(data$user_dob)
data$age %<>% as.numeric

## add week
data = cbind(data, week = week(data$order_date))

## customer 'loyalty' determined by 
data$loyalty <- as_date(data$order_date) - as_date(data$user_reg_date)
data$loyalty %<>% as.numeric

## delivery time
data$deliveryT <- as_date(data$delivery_date) - as_date(data$order_date)

data$na_del <- ifelse(is.na(data$deliveryT), "missing", "not missing") %>% as.factor
data$mis_del <- ifelse(data$deliveryT < 0, "negative", "not negative") %>% as.factor
data$deliveryT <- ifelse(data$deliveryT < 0, NA, data$deliveryT)

## converts variables into factors
data$user_title %<>% as.factor
data$user_state %<>% as.factor

## calculates average item price per customer
data$ave_price <- data %$% ave(x = item_price, user_id, FUN = mean)

### clean data set
data[, c('order_date', 'delivery_date', 'item_id', 'user_id', 'user_dob', 'user_reg_date', 'returns_user',
          'returns_pid') := NULL]

