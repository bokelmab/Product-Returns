## a function which returns the index of the best tuning parameters
tune_parameters <- function(p.data, p.grid, p.features){
  
  ## to shorten run time only 10% of the data is used
  idx.include <- createDataPartition(p.data$return, p = 0.1)
  data_small <- p.data[idx.include[[1]], ]
  
  start.time <- Sys.time() ## calculate run time 
  
  ## cross validation to choose best set of parameters
  folds <- createFolds(data_small$return, k = 10) ## 10-fold cross validation
  cv.results.ts <- data.frame(matrix(rep(0, 10 * nrow(p.grid)), ncol = 10)) ## results test set
  cv.results.tr <- data.frame(matrix(rep(0, 10 * nrow(p.grid)), ncol = 10)) ## results training set
  
  for(i in 1:10){ ## folds
    
    ## divide into training and test set
    data_small.tr <- data_small[-folds[[i]],]
    data_small.ts <- data_small[folds[[i]],]
    
    ## convert data_small for use of xgboost
    options(na.action = 'na.pass')
    data_small.tr.xgb <- model.matrix(return~.-1, data_small.tr %>% subset(select = p.features)) 
    data_small.ts.xgb <- model.matrix(return~.-1, data_small.ts %>% subset(select = p.features)) 
    
    ## settings for parallel computing
    cl<-makeCluster(2) #change the 2 to your number of CPU cores
    registerDoSNOW(cl)
    
    cv.results <- foreach(j = 1:nrow(p.grid), .packages = c('dplyr', 'xgboost','Metrics'), .combine = rbind) %dopar% { ## paramters
      
      model <- xgboost(data_small.tr.xgb, label = data_small.tr$return, max_depth = p.grid$max_depth[j], eta = p.grid$eta[j], nthread = 2, 
                       nrounds = p.grid$nrounds[i], colsample_bytree = 1, eval_metric = 'error', verbose = 0)
      
      predicted.probabilities <- predict(model, newdata = data_small.ts.xgb) ## predict probability of return
      predicted.class <- ifelse(predicted.probabilities > 0.5, 1, 0) ## return or no return
      
      c(1- model$evaluation_log$train_error[p.grid$nrounds[i]],accuracy(data_small.ts$return, predicted.class)) ## results accuracy
    }
    
    stopCluster(cl) ## stop parallel computing
    
    cv.results.tr[, i] <- cv.results[, 1]
    cv.results.ts[, i] <- cv.results[, 2]
    
  }
  
  ## choose best set of parameters 
  average.accuracy <- apply(cv.results.ts, 1, mean)
  index.best.parameters <- which(average.accuracy == max(average.accuracy))
  
  ## check for overfitting
  print('In-Sample Accuracy')
  apply(cv.results.tr, 1, mean)[index.best.parameters] %>% print
  print('----------------------------')
  print('Out-Of-Sample Accuracy')
  apply(cv.results.ts, 1, mean)[index.best.parameters] %>% print
  
  return(index.best.parameters)
}

