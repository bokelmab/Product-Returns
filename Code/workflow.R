## libraries
library(data.table)
library(tidyr) ## create parameter grid
library(dplyr)
library(xgboost)
library(Metrics) ## calculate errors
library(caret) ## cross validation
library(foreach) ## parallel computation
library(doSNOW) ## parallel computation
library(magrittr)
library(lubridate)

## functions for data processing and predictive modeling
source('Code/feature_engineering.R') ## feature engineering
source('Code/parameter_tuning.R') ## parameter tuning
source('Code/model_evaluation.R') ## model evaluation

## read data
data <- fread('Data/BADS_WS1718_known.csv')

## 1) data pre-processing and feature engineering -----------------------------------------------
data %<>% engineer_features

features <- names(data)  ## list of features 


## 2) parameter tuning for extreme gradient boosting --------------------------------------------
grid.xgboost <- crossing(max_depth = 2:10, eta = (1:7) / 10, nrounds = 20) ## parameter grid
idx.best.parameters <- tune_parameters(data, grid.xgboost, features)


## 3) model evaluation --------------------------------------------------------------------------
evaluate_model(data, grid.xgboost[idx.best.parameters, ], features) ## evaluate model with best tuning parameters


## 4) build and analyze model -------------------------------------------------------------------

## build model
data.xgb <- model.matrix(return~.-1, data %>% subset(select = features)) 
param <- grid.xgboost[idx.best.parameters, ]
model <- xgboost(data.xgb, label = data$return, max_depth = param$max_depth, 
                 eta = param$eta, nthread = 2, nrounds = param$nrounds, 
                 colsample_bytree = 1, eval_metric = 'error', verbose = 0)

## variable importance
importance_matrix <- xgb.importance(colnames(data.xgb), model = model)
xgb.plot.importance(importance_matrix)

## calibration plot
calibration(factor(data$return, levels = c("1", "0")) ~ predict(model, data.xgb)) %>% xyplot




