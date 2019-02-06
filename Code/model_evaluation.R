

## returns average accuracy from cross validation
evaluate_model <- function(p.data, p.param, p.features){
  
  folds2 <- createFolds(p.data$return, k = 10) ## 10-fold cross validation
  
  ## settings for parallel computing
  cl<-makeCluster(2) #change the 2 to your number of CPU cores
  registerDoSNOW(cl)
  
  cv2.results <- foreach(i = 1:10, .packages = c('dplyr', 'xgboost','Metrics')) %dopar% { ## folds
    
    ## divide into training and test set
    data.tr <- p.data[-folds2[[i]],]
    data.ts <- p.data[folds2[[i]],]
    
    ## convert data for use of xgboost
    options(na.action = 'na.pass')
    data.tr.xgb <- model.matrix(return~.-1, data.tr %>% subset(select = p.features)) 
    data.ts.xgb <- model.matrix(return~.-1, data.ts %>% subset(select = p.features)) 
    
    model <- xgboost(data.tr.xgb, label = data.tr$return, max_depth = p.param$max_depth, 
                     eta = p.param$eta, nthread = 2, nrounds = p.param$nrounds, 
                     colsample_bytree = 1, eval_metric = 'error', verbose = 0)
    
    predicted.probabilities <- predict(model, newdata = data.ts.xgb) ## predict probability of return
    predicted.class <- ifelse(predicted.probabilities > 0.5, 1, 0) ## return or no return
    
    accuracy(data.ts$return, predicted.class)  
  }
  
  stopCluster(cl) ## stop parallel computing
  
  cv2.results %>% unlist %>% mean %>% return## evaluate model
}

