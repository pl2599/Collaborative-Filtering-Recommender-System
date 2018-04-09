########################
### Cross Validation ###
########################

cv.function <- function(UI_matrix, K, par, dataset='movies') {
  
  K <- 3 # 3-fold cross validation
  num_non_NA <- length(which(!is.na(movie_UI)))
  n.fold     <- floor(num_non_NA/K)
  s          <- sample(rep(1:K, c(rep(n.fold, K-1), num_non_NA-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    if (dataset=='movies'){
      # copy UI_matrix twice, replace values of current test fold by NAs in train and leave these values in test
      train <- UI_matrix
      test  <- UI_matrix
      
      train[which(s != i)] = NA
      test[which(s == i)] = NA
    } else if (dataset=='MS'){
      # copy UI_matrix twice, replace values of current test fold by 0s in train and leave these values in test
      train <- UI_matrix
      test  <- UI_matrix
      
      train[which(s != i)] = 0
      test[which(s == i)] = 0
    }
    
    result <- em(train, C=par$C, epsilon=par$epsilon, dataset=dataset)
    pred <- predict_all_v2(UI_matrix, result[[2]], result[[3]], result[[1]], dataset=dataset)
    cv.error[i] <- ifelse(dataset=='movies', test_acc_movie(pred, test), test_acc_MS(pred, test))
  }
  return(c(mean(cv.error), sd(cv.error)))
}
