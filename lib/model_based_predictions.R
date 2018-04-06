# Functions to be run after the EM algorithm estimated the parameters, to predict ratings

# Better version:
estimate_rating_item <- function(UI_matrix, j, prob_class, prob_rating, assignment){
  n_users <- dim(UI_matrix)[1]
  # estimate all ratings for movie j at once
  ratings <- matrix(0, nrow=n_users)
  for (k in 1:6){
    ratings <- ratings + k*assignment%*%prob_rating[,j,k]
  }
  return(ratings)
}

predict_all_v2 <- function(UI_matrix, prob_class, prob_rating, assignment){
  n_items <- dim(UI_matrix)[2]
  for (j in 1:n_items){
    UI_matrix[,j] <- estimate_rating_item(UI_matrix, j, prob_class, prob_rating, assignment)
  }
  return(UI_matrix)
}

# ###############################

# Initial version:
estimate_rating_user_item <- function(C, i, j, prob_class, prob_rating, assignment){
  # estimate rating of user i for item j, given learned parameters
  # Step 1: compute probabilities for each rating
  probs <- matrix(0, nrow=6)
  for (k in 1:6){
    for (c in 1:C){
      probs[k] = probs[k] + assignment[i, c] * prob_rating[c, j, k]
    }
  }
  # Step 2: compute expectation
  expected_rating <- 0
  for (k in 1:6){
    expected_rating <- expected_rating + k*probs[k]
  }
  return(expected_rating)
}

predict_all_slow <- function(UI_matrix, C, prob_class, prob_rating, assignment){
  n_users <- dim(UI_matrix)[1]
  n_items <- dim(UI_matrix)[2]
  # fills in UI_matrix and returns full matrix
  for (i in 1:n_users){
    for (j in 1:n_items){
      if (is.na(UI_matrix[i,j])){
        UI_matrix[i,j] <- estimate_rating_user_item(C, i, j, prob_class, prob_rating, assignment)
      }
    }
  }
  return(UI_matrix)
}
