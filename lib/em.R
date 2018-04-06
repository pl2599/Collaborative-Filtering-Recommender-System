
# EM algorithm:
em <- function(UI_matrix, C, epsilon, max_steps=100){
  
  n_users <- dim(UI_matrix)[1]
  n_items <- dim(UI_matrix)[2]
  step <- 0
  prob_class <- matrix(1/C, nrow=C)
  prob_rating <- array(1/6, dim=c(C, n_items, 6))
  assignment <- matrix(NA, nrow=n_users, ncol=C)
  
  while (TRUE){
    step <- step + 1
    # E:
    new_assignment <- update_assignment(UI_matrix, C, assignment, prob_class, prob_rating)
    
    # M:
    new_prob_class <- update_prob_class(UI_matrix, C, prob_class, new_assignment)
    new_prob_rating <- update_prob_rating(UI_matrix, C, prob_rating, new_assignment)
    
    if ((step > 1) & (norm(new_prob_class - prob_class) < epsilon)){
      # convergence: we check if prob_class and prob_rating converge
      # TODO: maybe checking for convergence of likelihood would be better ?
      print(paste0('converged in ', step, ' steps.'))
      return(list(assignment, prob_class, prob_rating))
    } else if (step > max_steps){
      print('reached max number of steps without converging, returning...')
      return(list(assignment, prob_class, prob_rating))
    }
    
    assignment <- new_assignment
    prob_class <- new_prob_class
    prob_rating <- new_prob_rating
    print(paste0('step ', step,' done'))
  }
}


# Expectation step:
update_assignment <- function(UI_matrix, C, assignment, prob_class, prob_rating){
  # trick: prob_rating contains floats between 0 and 1
  # since we do a products of a lot of these floats, we'll underflow at some point
  # to fix this scale prob_rating (to mean=1) initially
  # since we're taking ratios eventually this is still correct.
  n_users <- dim(UI_matrix)[1]
  prob_rating <- prob_rating/mean(prob_rating)
  for (i in 1:n_users){
    movies_rated_by_i <- which(!is.na(UI_matrix[i,]))
    ratings_given_by_i <- UI_matrix[i,!is.na(UI_matrix[i,])]
    
    # we first compute all numerators in the formula for assignment[i,c]:
    for (c in 1:C){
      assignment[i,c] <- prob_class[c]
      for (k in 1:length(movies_rated_by_i)){
        # still looking for a way to avoid this loop...
        assignment[i,c] <- assignment[i,c] * prob_rating[c, movies_rated_by_i[[k]], ratings_given_by_i[[k]]]
      }
    }
    
    # and then divide by the sum:
    assignment[i,] <- assignment[i,]/sum(assignment[i,])
    
    if (is.na(sum(assignment[i,]))){
      print("problem in assignment update")
    }
  }
  return(assignment)
}

# Maximization steps:
update_prob_class <- function(UI_matrix, C, prob_class, assignment){
  # C being generally small, no need to optimize this for loop
  n_users <- dim(UI_matrix)[1]
  for (c in 1:C){
    prob_class[c] <- sum(assignment[!is.na(assignment[,c]),c])/n_users
  }
  return(prob_class)
}

update_prob_rating <- function(UI_matrix, C, prob_rating, assignment){
  n_items <- dim(UI_matrix)[2]
  for (c in 1:C){
    for (j in 1:n_items){
      users_who_rated_item_j <- which(!is.na(UI_matrix[,j]))
      denominator <- sum(assignment[users_who_rated_item_j,c])
      # warning: denominator can be = 0 ! (if prob_class got to 0 for class c at previous step)
      # print(denominator)
      for(k in 1:6){
        if (denominator != 0){
          numerator <- sum((UI_matrix[users_who_rated_item_j,j] == k) * assignment[users_who_rated_item_j,c])
          prob_rating[c,j,k] <- numerator/denominator
        } else{
          prob_rating[c,j,k] <- 0
        }
      }
    }
    return(prob_rating)
  }
}