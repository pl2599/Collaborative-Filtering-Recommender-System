# Functions to be run after the EM algorithm estimated the parameters, to predict ratings

# Better version:
estimate_rating_item <- function(UI_matrix, j, prob_class, prob_rating, assignment, possible_ratings){
  n_users <- dim(UI_matrix)[1]
  # estimate all ratings for movie j at once
  ratings <- matrix(0, nrow=n_users)
  k_fix <- ifelse(length(possible_ratings) == 2, 1, 0) # for MS, possible ratings are 0 or 1, but prob_rating is indexed starting 1, so shift if necessary
  for (k in possible_ratings){
    ratings <- ratings + k*assignment%*%prob_rating[,j,k + k_fix]
  }
  return(ratings)
}

predict_all_v2 <- function(UI_matrix, prob_class, prob_rating, assignment, dataset='movies'){
  n_items <- dim(UI_matrix)[2]
  if (dataset=='movies'){
    possible_ratings = (1:6)
  } else if (dataset=='MS'){
    possible_ratings = (0:1)
  }

  for (j in 1:n_items){
    UI_matrix[,j] <- estimate_rating_item(UI_matrix, j, prob_class, prob_rating, assignment, possible_ratings)
  }
  return(UI_matrix)
}

# ###############################

# Initial version:
estimate_rating_user_item <- function(C, i, j, prob_class, prob_rating, assignment, possible_ratings){
  # estimate rating of user i for item j, given learned parameters
  # Step 1: compute probabilities for each rating
  probs <- matrix(0, nrow=6)
  k_fix <- ifelse(length(possible_ratings) == 2, 1, 0) # for MS, possible ratings are 0 or 1, but prob_rating is indexed starting 1, so shift if necessary
  for (k in possible_ratings){
    for (c in 1:C){
      probs[k + k_fix] = probs[k + k_fix] + assignment[i, c] * prob_rating[c, j, k + k_fix]
    }
  }
  # Step 2: compute expectation
  expected_rating <- 0
  for (k in possible_ratings){
    expected_rating <- expected_rating + k*probs[k + k_fix]
  }
  return(expected_rating)
}

predict_all_slow <- function(UI_matrix, C, prob_class, prob_rating, assignment, dataset='movies'){
  n_users <- dim(UI_matrix)[1]
  n_items <- dim(UI_matrix)[2]
  if (dataset=='movies'){
    possible_ratings = (1:6)
  } else if (dataset=='MS'){
    possible_ratings = (0:1)
  }
  
  # fills in UI_matrix and returns full matrix
  for (i in 1:n_users){
    for (j in 1:n_items){
      if (is.na(UI_matrix[i,j])){
        UI_matrix[i,j] <- estimate_rating_user_item(C, i, j, prob_class, prob_rating, assignment, possible_ratings)
      }
    }
  }
  return(UI_matrix)
}
