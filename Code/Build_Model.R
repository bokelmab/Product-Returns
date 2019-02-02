## libraries
library(data.table)
library(dplyr)
library(xgboost)

## read data
data <- fread('Data/BADS_WS1718_known.csv')


## train model
features <- c('return','item_price')

options(na.action = 'na.pass')
dtrain <- model.matrix(return~.-1, data %>% subset(select = features)) 

model <- xgboost(dtrain, label = data$return, max_depth = 8, eta = 0.08, nthread = 2, 
                nrounds = 35, colsample_bytree = 1, eval_metric = 'error')




