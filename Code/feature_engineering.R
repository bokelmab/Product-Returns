## libraries
library(data.table)
library(dplyr)
library(magrittr)
engineer_features <- function(p.data){
  
  ### product-specific features -----------------------------------------
  
  ## total amount of orders per product
  p.data$orders_pid <- p.data %$% ave(x = item_id, item_id, FUN = length)
  
  ## average return rate per product
  p.data$returns_pid <- p.data %$% ave(x = return, item_id, FUN = sum)
  p.data$return_rate_pid <- (p.data$returns_pid - p.data$return) / (p.data$orders_pid - 1)
  p.data$return_rate_pid %<>% round(1) ## prevent a fascinating kind of error
  
  ## convert size and color
  p.data$item_size %<>% as.factor
  p.data$item_color %<>% as.factor
  p.data$brand_id %<>% as.factor
  
  ## create return rates for size, color and brand id
  p.data$return_rate_size <- p.data %$% ave(return, by = list(item_size), FUN = mean)
  p.data$return_rate_color <- p.data %$% ave(return, by = list(item_color), FUN = mean)
  p.data$return_rate_brand <- p.data %$% ave(return, by = list(brand_id), FUN = mean)
  
  
  ### customer-specific features ---------------------------------------
  
  ## total amount of orders per product
  p.data$orders_user <- p.data %$% ave(x = user_id, user_id, FUN = length)
  
  ## average return rate
  p.data$returns_user <- p.data %$% ave(x = return, user_id, FUN = sum)
  p.data$return_rate_user <- (p.data$returns_user - p.data$return) / (p.data$orders_user - 1)
  
  ## creates the variable age (at time of order) out of user_dob and order_date
  p.data$user_dob %<>% as_date
  p.data$age <- year(p.data$order_date) - year(p.data$user_dob)
  p.data$age %<>% as.numeric
  
  ## add week
  p.data = cbind(p.data, week = week(p.data$order_date))
  
  ## customer 'loyalty' determined by 
  p.data$loyalty <- as_date(p.data$order_date) - as_date(p.data$user_reg_date)
  p.data$loyalty %<>% as.numeric
  
  ## delivery time
  p.data$deliveryT <- as_date(p.data$delivery_date) - as_date(p.data$order_date)
  
  p.data$na_del <- ifelse(is.na(p.data$deliveryT), "missing", "not missing") %>% as.factor
  p.data$mis_del <- ifelse(p.data$deliveryT < 0, "negative", "not negative") %>% as.factor
  p.data$deliveryT <- ifelse(p.data$deliveryT < 0, NA, p.data$deliveryT)
  
  ## converts variables into factors
  p.data$user_title %<>% as.factor
  p.data$user_state %<>% as.factor
  
  ## calculates average item price per customer
  p.data$ave_price <- p.data %$% ave(x = item_price, user_id, FUN = mean)
  
  ### clean data set ------------------------------------------------------
  p.data[, c('order_date', 'delivery_date', 'item_id', 'user_id', 'user_dob', 'user_reg_date', 'returns_user',
           'returns_pid', 'item_size', 'order_item_id', 'item_color', 'brand_id') := NULL]
  
  return(p.data)
}







