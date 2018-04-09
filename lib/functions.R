###################################################################
### Memory-based Collaborative Filtering Algorithm Starter Code ###
###################################################################

### Authors: Group 3 
### Project 3
### ADS Spring 2018


MS_data_transform <- function(MS) {
  
  ## Calculate UI matrix for Microsoft data
  ##
  ## input: data   - Microsoft data in original form
  ##
  ## output: UI matrix
  
  
  # Find sorted lists of users and vroots
  users  <- sort(unique(MS$V2[MS$V1 == "C"]))
  vroots <- sort(unique(MS$V2[MS$V1 == "V"]))
  
  nu <- length(users)
  nv <- length(vroots)
  
  # Initiate the UI matrix
  UI            <- matrix(0, nrow = nu, ncol = nv)
  row.names(UI) <- users
  colnames(UI)  <- vroots
  
  user_locs <- which(MS$V1 == "C")
  
  # Cycle through the users and place 1's for the visited vroots.
  for (i in 1:nu) {
    name     <- MS$V2[user_locs[i]]
    this_row <- which(row.names(UI) == name)
    
    # Find the vroots
    if (i == nu) {
      v_names <- MS$V2[(user_locs[i] + 1):nrow(MS)]
    } else {
      v_names <- MS$V2[(user_locs[i] + 1):(user_locs[i+1] - 1)]
    }  
    
    # Place the 1's
    UI[this_row, colnames(UI) %in% v_names] <- 1
  }
  return(UI)
}



movie_data_transform <- function(movie) {
  
  ## Calculate UI matrix for eachmovie data
  ##
  ## input: data   - movie data in original form
  ##
  ## output: UI matrix

  
  # Find sorted lists of users and vroots
  users  <- sort(unique(movie$User))
  movies <- sort(unique(movie$Movie))
  
  # Initiate the UI matrix
  UI            <- matrix(NA, nrow = length(users), ncol = length(movies))
  row.names(UI) <- users
  colnames(UI)  <- movies
  
  # We cycle through the users, finding the user's movies and ratings
  for (i in 1:length(users)) {
    user    <- users[i]
    movies  <- movie$Movie[movie$User == user]
    ratings <- movie$Score[movie$User == user]
    
    ord     <- order(movies)
    movies  <- movies[ord]
    ratings <- ratings[ord]
    
    # Note that this relies on the fact that things are ordered
    UI[i, colnames(UI) %in% movies] <- ratings
  }
  return(UI)
}  






calc_weight <- function(data, method) {
  
  ## Calculate similarity weight matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        method - 'pearson'
  ##
  ## output: similarity weight matrix
  
  
  # Iniate the similarity weight matrix
  data       <- as.matrix(data)
  weight_mat <- matrix(NA, nrow = nrow(data), ncol = nrow(data))
  
  weight_func <- function(rowA, rowB) {
    
    # weight_func takes as input two rows (thought of as rows of the data matrix) and 
    # calculates the similarity between the two rows according to 'method'
    
    joint_values <- !is.na(rowA) & !is.na(rowB)
    if (sum(joint_values) == 0) {
      return(0)
    } else {
      if (method == 'pearson') {
        return(cor(rowA[joint_values], rowB[joint_values], method = 'pearson'))
      }
      if (method == 'spearman') {
        return(cor(rowA[joint_values], rowB[joint_values], method = 'spearman'))
      }
      if (method == 'mean_squared_diff') {
        return(1/(1+sum((rowA[joint_values]-rowB[joint_values])^2)/length(rowA[joint_values])))
      }
      if (method == 'entropy') {
        return(MutInf(rowA[joint_values], rowB[joint_values]))
      }
      if (method == 'vector') {
        return(cosine(rowA[joint_values], rowB[joint_values]))
      }
    }
  }
  
  # Loops over the rows and calculate all similarities using weight_func
  for(i in 1:nrow(data)) {
    weight_mat[i, ] <- apply(data, 1, weight_func, data[i, ])
    print(i)
  }
  return(round(weight_mat, 4))
}






calc_simrank <- function(data) {
  
  # Initiate the user similarity matrix.
  weight_mat <- matrix(0, nrow = nrow(data), ncol = nrow(data))
  # Initiate the item similarity matrix.
  item_sim <- matrix(0,nrow=ncol(data),ncol=ncol(data))
  # the similarity of the same user and item remains 1.
  diag(weight_mat) <- 1
  diag(item_sim) <- 1
  
  # Change MS entries from 0 to NA
  data[data == 0] <- NA
  
  # Iteration 5 times
  for (iter in 1:5) {
    
    # fill in user similarity matrix
    weight_func <- function(rowA, rowB) {
      return(0.8*sum(item_sim[which(!is.na(rowA)),which(!is.na(rowB))])/(sum(rowA,na.rm=T)*sum(rowB,na.rm=T)))
    }
    for(i in 1:nrow(data)) {
      weight_mat[i, ] <- apply(data, 1, weight_func, data[i, ])
    }
    diag(weight_mat) <- 1
    
    # fill in item similarity matrix
    sim_func <- function(col1, col2) {
      return(0.8*sum(weight_mat[which(!is.na(col1)),which(!is.na(col2))])/(sum(col1,na.rm=T)*sum(col2,na.rm=T)))
    }
    for(i in 1:ncol(data)) {
      item_sim[i, ] <- apply(data, 2, sim_func, data[,i])
    }
    diag(item_sim) <- 1
    print(iter)
  }
  return(round(weight_mat,4))
}






get_significance <- function(data,threshold) {
  
  data[data == 0] <- NA # in case of MS data
  #Needs matrix of [users,movies]
  n_users = dim(data)[1]
  rated = list()
  for (i in 1:n_users) {
    rated[[i]] = which(!is.na(data[i,]))
  }
  movie_significances = matrix(1.0,n_users,n_users)
  
  for (i in 1:n_users) {
    for (j in i+1:n_users){
      signif = length(intersect(rated[[i]],rated[[j]]))
      if (signif < threshold){
        movie_significances[i,j] = signif / threshold
        movie_significances[j,i] = signif / threshold
      }
    }
  }
  return(movie_significances)
}






get_item_variances <- function(data) {
  
  n = dim(data)[2]
  item_variances = rep(0,n)
  for (i in 1:n){
    workingcol = data[,i]
    rated_ids = !is.na(workingcol)
    workingcol = workingcol[rated_ids]
    item_var = var(workingcol)
    if (is.na(item_var)){
      item_var = 0
    }
    item_variances[i] = item_var
  }
  item_variances = (item_variances - min(item_variances)) / max(item_variances)
  return(item_variances)
}






calc_var_weights <- function(data,item_variances) {
  
  #give it the data, and list of the item_variances
  data       <- as.matrix(data)
  weight_mat <- matrix(NA, nrow = nrow(data), ncol = nrow(data))
  
  #gets correlation between 2 rows
  get_var_weighted_cor <- function(rowAin,rowBin){
    joint_values <- !is.na(rowAin) & !is.na(rowBin)
    if (sum(joint_values) == 0){ 
      return(0)
    }
    rowA = rowAin[joint_values]
    rowB = rowBin[joint_values]
    meanA = mean(rowA)
    meanB = mean(rowB)
    n = length(rowA)
    num = 0
    magA = 0
    magB = 0
    for (i in 1:n){
      Apart = rowA[i] - meanA
      Bpart = rowB[i] - meanB
      num = num + Apart * Bpart * item_variances[i]
      magA = magA + Apart ^ 2
      magB = magB + Bpart ^ 2
    }
    return(num / sqrt(magA) / sqrt(magB) * n / sum(item_variances))
  }
  
  for(i in 1:nrow(data)) {
    weight_mat[i, ] <- apply(data, 1, get_var_weighted_cor, data[i, ])
    print(i)
  }
  return(round(weight_mat, 4))
}






pred_matrix <- function(data, simweights) {
  
  ## Calculate prediction matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        simweights - a matrix of similarity weights
  ##
  ## output: prediction matrix
  
  # Initiate the prediction matrix.
  pred_mat <- data
  
  # Change MS entries from 0 to NA
  pred_mat[pred_mat == 0] <- NA
  
  row_avgs <- apply(data, 1, mean, na.rm = TRUE)
  
  for(i in 1:nrow(data)) {
    
    # Find columns we need to predict for user i and sim weights for user i
    cols_to_predict <- which(is.na(pred_mat[i, ]))
    num_cols        <- length(cols_to_predict)
    neighb_weights  <- simweights[i, ]
    
    # Transform the UI matrix into a deviation matrix since we want to calculate
    # weighted averages of the deviations
    dev_mat     <- data - matrix(rep(row_avgs, ncol(data)), ncol = ncol(data))
    weight_mat  <- matrix(rep(neighb_weights, ncol(data)), ncol = ncol(data))
    
    weight_sub <- weight_mat[, cols_to_predict]
    dev_sub    <- dev_mat[ ,cols_to_predict]
    
    pred_mat[i, cols_to_predict] <- row_avgs[i] +  apply(dev_sub * weight_sub, 2, sum, na.rm = TRUE)/sum(neighb_weights, na.rm = TRUE)
    print(i)
  }
  
  return(pred_mat)
}





test_acc_movie <- function(pred, test) {
  
  # Since test data only contains part of the columns. We need to filter out the columns 
  # from prediction matrix so they have the same dimension as test data
  pred <- pred[,colnames(test)]
  
  # Initiate the Average Absolute Deviation Score Vector 
  scores <- rep(NA, nrow(test))
  
  for(i in 1:nrow(test)) {
    
    # Find columns we need to test for user i
    cols_to_test <- which(!is.na(test[i, ]))
    num_cols <- length(cols_to_test)
    
    # Calculate the score for user i
    scores[i] <- sum(abs(pred[i, cols_to_test] - test[i, cols_to_test])) / num_cols
  }
  
    # another way: 
    # names_cols <- names(cols_to_test)
    # scores[i] <- sum(abs(pred[i, names_cols] - test[i, names_cols])) / num_cols
  
  return(mean(scores))
}







test_acc_MS <- function(pred, test) {
  
  exp_util <- rep(NA, nrow(test))
  max_util <- rep(NA, nrow(test))
  
  for(i in 1:nrow(test)) {
    
    # Calculate the expected utility and max utility for user i
    recom_items_cols <- which(pred[i,] > 0.5 & pred[i,] < 1)
    names_recom <- names(recom_items_cols)
    actual_items_cols <- which(test[i,] == 1)
    actual_vote_recom <- test[i, names_recom]
    #recom_items_score <- pred[i, names_recom]
    #names_actual <- names(actual_items_cols)
    #recom_items_rank <- rank(-recom_items_score)
    #cols_to_score <- names_recom %in% names_actual
    #relevant_recom_items_rank <- recom_items_rank[cols_to_score]
    
    exp_util[i] <- sum(actual_vote_recom/(2^((1:length(recom_items_cols) - 1) / (5 - 1))))
    max_util[i] <- sum(1/(2^((1:length(actual_items_cols) - 1) / (5 - 1))))
  }
  
  return(100*(sum(exp_util)/sum(max_util)))
}


