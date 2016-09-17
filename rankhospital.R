# Write a function called rankhospital that takes three arguments: 
# the 2-character abbreviated name of a state (state), an outcome (outcome), and
# the ranking of a hospital in that state for that outcome (num). 
# The function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the ranking 
# specified by the num argument.
rankhospital <- function(state, outcome, rank) {
  ## Read outcome data
  out_table <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character", na.strings = "Not Available",
                        stringsAsFactors = FALSE) 
  #  out_table_class <- sapply(out_table, class) #Find out the class of input variables
  #  print(paste("out_table column classes are",out_table_class))
  # Test for valid state entry
  test_state <- out_table[,7] == state
  if(sum(test_state) < 1) stop("invalid state")
  
  # HEART ATTACK
  if(outcome == "heart attack") {
    outcome <- "heart.attack"
    out2 <- out_table[,2]
    out7 <- out_table[,7]
    out_param <- out_table[,11]
    print("Take a quick look at the data without NAs removed")
    print(paste(head(out2),head(out7), head(out_param)))
    class_out_param <- class(out_param)
    print(paste("Class of the outcome parameter is", class_out_param))
    out_comb <- data.frame("Hospital" = out2[!is.na(out_param) & out7 == state], 
                           "State" = out7[!is.na(out_param) & out7 == state], 
                           "Heart Attack" = as.numeric(as.character
                                                       (out_param[!is.na(out_param) & out7 == state]))) # Don't add the rank yet
    out_param_test <- out_comb$Heart.Attack
    print(head(out_param_test))
    #    print(paste("Parameter is", class(out_param_test)))
    
    # Change factors to characters
    out_param_char <- as.character(out_param_test)
    #    print(head(out_param_char)) # for debugging
    #    test_class <- class(out_param_char) # for debugging
    #    print(paste("The parameter is ", test_class))
    out_param_num <- as.numeric(out_param_char)
    test_class <- class(out_param_num)
    #    print(paste("The parameter is ", test_class))
    
    #    print(out_comb)
    #    as.numeric(out_comb$Heart.Attack) # Doesn't work
    #    as.numeric(levels(out_param_test))[out_param_test] # Doesn't work
    #    as.numeric(paste(out_param_test)) # Doesn't work
    as.numeric(as.character(out_param_test))
    test_class <- class(out_param_test)
    print(paste("The parameter revised class is ", test_class))
    
    # HEART FAILURE    
  } else if(outcome == "heart failure") {
    outcome <- "heart.failure"
    out2 <- out_table[,2]
    out7 <- out_table[,7]
    out_param <- out_table[,17]
    print("Take a quick look at the data without NAs removed")
    print(paste(head(out2),head(out7), head(out_param)))
    class_out_param <- class(out_param)
    print(paste("Class of the outcome parameter is", class_out_param))
    out_comb <- data.frame("Hospital" = out2[!is.na(out_param) & out7 == state], 
                           "State" = out7[!is.na(out_param) & out7 == state], 
                           "Heart Failure" = as.numeric(as.character
                                                        (out_param[!is.na(out_param) & out7 == state])))
    print(out_comb)
    
    # PNEUMONIA 
  } else if(outcome == "pneumonia") {
    outcome <- "pneumonia"
    out2 <- out_table[,2]
    out7 <- out_table[,7]
    out_param <- out_table[,23]
    print("Take a quick look at the data without NAs removed")
    print(paste(head(out2),head(out7), head(out_param)))
    class_out_param <- class(out_param)
    print(paste("Class of the outcome parameter is", class_out_param))
    out_comb <- data.frame("Hospital" = out2[!is.na(out_param) & out7 == state], 
                           "State" = out7[!is.na(out_param) & out7 == state], 
                           "Pneumonia" = as.numeric(as.character
                                                    (out_param[!is.na(out_param) & out7 == state])))
    print(head(out_comb))
  } else {
    stop("invalid outcome")
  }
  
  # NOW SORT AND ADD RANK COLUMN
  
  # Create a table of hospitals in the specified state for the designated outcome
  out_table_sorted <- out_comb[ order(out_comb[,3], out_comb[,1]), ]
  out_param <- out_table_sorted[, 3]
  temp_matrix <- matrix(out_param, nrow = length(out_param), ncol = 1)
  print("temp_matrix is") 
  print(head(temp_matrix))
  row_ <- row(temp_matrix)
  row_ <- as.vector(row_) # Creates a vector out of the row numbers
  #  print("row vector is")
  #  print(head(row_))
  
  # Now add the rank column
  out_table_sorted <- data.frame("Hospital" = out_table_sorted[,1], 
                                 "State" = out_table_sorted[,2], 
                                 "Heart Failure" = out_table_sorted[,3],
                                 "Rank" = row_)
  print("The sorted table is shown below")
  print(out_table_sorted) 
  # print(cbind(as.character(out_best[1:counter,1])))
  
  # Now pull the name of the hospital with the given rank
  if(rank == "best"){
    the_hospital <- out_table_sorted[1,1]
    print(paste("The best hospital in the state with respect to", outcome, "is"))
    print(the_hospital)
  }
  else if(rank == "worst") {
    the_hospital <- out_table_sorted[length(out_table_sorted[, 1]),1]
    print(paste("The worst hospital in the state with respect to", outcome, "is"))
    print(the_hospital)
  }
  # Test if the rank is reasonable
  else if(rank > 0 & rank <= length(out_table_sorted[, 1])){
    the_hospital <- out_table_sorted[rank, 1]
    print(paste("The hospital in the state of rank", rank, 
                "with respect to", outcome, "is"))
    print(the_hospital)
  }
  else {
    stop("invalid rank")
  }
}


