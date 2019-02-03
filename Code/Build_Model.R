## libraries
library(data.table)
library(tidyr) ## create parameter grid
library(dplyr)
library(xgboost)
library(Metrics) ## calculate errors
library(caret) ## cross validation
library(foreach) ## parallel computation
library(doSNOW) ## parallel computation

## read data
data <- fread('Data/BADS_WS1718_known.csv')

features <- c('return','item_price') ## list of features 

grid.xgboost <- crossing(max_depth = 2:10, eta = (1:7) / 10, nrounds = 20) ## parameter grid

## cross validation to choose best set of parameters
folds <- createFolds(data$return, k = 10) ## 10-fold cross validation
cv.results.ts <- data.frame(matrix(rep(0, 10 * nrow(grid.xgboost)), ncol = 10)) ## results test set
cv.results.tr <- data.frame(matrix(rep(0, 10 * nrow(grid.xgboost)), ncol = 10)) ## results training set

for(i in 1:10){ ## folds
  
  ## divide into training and test set
  data.tr <- data[-folds[[i]],]
  data.ts <- data[folds[[i]],]
  
  ## convert data for use of xgboost
  options(na.action = 'na.pass')
  data.tr.xgb <- model.matrix(return~.-1, data.tr %>% subset(select = features)) 
  data.ts.xgb <- model.matrix(return~.-1, data.ts %>% subset(select = features)) 
  
  ## settings for parallel computing
  cl<-makeCluster(2) #change the 2 to your number of CPU cores
  registerDoSNOW(cl)
  
  cv.results <- foreach(j = 1:nrow(grid.xgboost), .packages = c('dplyr', 'xgboost','Metrics'), .combine = rbind) %dopar% { ## paramters
    
    model <- xgboost(data.tr.xgb, label = data.tr$return, max_depth = grid.xgboost$max_depth[j], eta = grid.xgboost$eta[j], nthread = 2, 
                     nrounds = grid.xgboost$nrounds[i], colsample_bytree = 1, eval_metric = 'error', verbose = 0)
    
    predicted.probabilities <- predict(model, newdata = data.ts.xgb) ## predict probability of return
    predicted.class <- ifelse(predicted.probabilities > 0.5, 1, 0) ## return or no return
    
    c(1- model$evaluation_log$train_error[grid.xgboost$nrounds[i]],accuracy(data.ts$return, predicted.class)) ## results accuracy
    #cv.results.ts[j, i] <- accuracy(data.ts$return, predicted.class) ## accuracy test
    #cv.results.tr[j, i] <- 1- model$evaluation_log$train_error[grid.xgboost$nrounds[i]] ## accuracy training
  }
  
  stopCluster(cl) ## stop parallel computing
  
  cv.results.tr[, i] <- cv.results[, 1]
  cv.results.ts[, i] <- cv.results[, 2]
  
  
}

## choose best set of parameters 
average.accuracy <- apply(cv.results.ts, 1, mean)
index.best.parameters <- which(average.accuracy == max(average.accuracy))

## check for overfitting
apply(cv.results.tr, 1, mean)[index.best.parameters]
apply(cv.results.ts, 1, mean)[index.best.parameters]


## 2nd cross validation to evaluate model performance
## (The result of the above CV may be biased, because a parameter selection according to accuracy took place)
folds2 <- createFolds(data$return, k = 10) ## 10-fold cross validation

## settings for parallel computing
cl<-makeCluster(2) #change the 2 to your number of CPU cores
registerDoSNOW(cl)

cv2.results <- foreach(i = 1:10, .packages = c('dplyr', 'xgboost','Metrics')) %dopar% { ## folds
  
  ## divide into training and test set
  data.tr <- data[-folds2[[i]],]
  data.ts <- data[folds2[[i]],]
  
  ## convert data for use of xgboost
  options(na.action = 'na.pass')
  data.tr.xgb <- model.matrix(return~.-1, data.tr %>% subset(select = features)) 
  data.ts.xgb <- model.matrix(return~.-1, data.ts %>% subset(select = features)) 
  
  model <- xgboost(data.tr.xgb, label = data.tr$return, max_depth = grid.xgboost$max_depth[index.best.parameters], 
                   eta = grid.xgboost$eta[index.best.parameters], nthread = 2, nrounds = grid.xgboost$nrounds[index.best.parameters], 
                   colsample_bytree = 1, eval_metric = 'error', verbose = 0)
    
  predicted.probabilities <- predict(model, newdata = data.ts.xgb) ## predict probability of return
  predicted.class <- ifelse(predicted.probabilities > 0.5, 1, 0) ## return or no return
  
  accuracy(data.ts$return, predicted.class)  
  #cv2.results[i] <- accuracy(data.ts$return, predicted.class)
}

stopCluster(cl) ## stop parallel computing

cv2.results %>% unlist %>% mean ## evaluate model






