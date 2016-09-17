# Function to read in the state and the outcome parameter, 
# Then returns a list of the best hospitals in a state based on that parameter
best <- function(state, outcome) {
  ## Read outcome data
#  remove("out2", "out7","out_param")
  out_table <- read.csv("outcome-of-care-measures.csv",
    colClasses = "character", na.strings = "Not Available",  # removed to test read.csv
    stringsAsFactors = FALSE)                                # removed to test read.csv
  
# Test for valid state entry
  test_state <- out_table[,7] == state
  if(sum(test_state) < 1) stop("invalid state")
# Test for valid outcome entry - this works to return 
  if(outcome == "heart attack") {
    outcome <- "heart.attack"
    # out_table <- cbind(out_table[,2], out_table[,7], out_table[, 11])
#    df <- data.frame(c(out_table[,2], out_table[,7], out_table[,11]))
#    print(head(out_table[,2]))
    out2 <- out_table[,2]
    out7 <- out_table[,7]
    out_param <- out_table[,11]
    print(head(out2))
    print(head(out7))
    print(head(out_param))
#    out_comb <- data.frame("Hospital" = out_table[,2], 
#        "State" = out_table$State, "Heart Attack" = out_param) # This works
#    out_comb <- data.frame("Hospital" = out2[!is.na(out_param)], # This works
#          "State" = out7[!is.na(out_param)], 
#          "Heart Attack" = out_param[!is.na(out_param)])
    out_comb <- data.frame("Hospital" = out2[!is.na(out_param) & out7 == state], # Test, with state
           "State" = out7[!is.na(out_param) & out7 == state], 
           "Heart Attack" = as.numeric(as.character(out_param[!is.na(out_param) & out7 == state])))
    out_param_test <- out_comb$Heart.Attack
    print(head(out_param_test))
    print(paste("Parameter is", class(out_param_test)))

# Change factors to characters
    out_param_char <- as.character(out_param_test)
#    print(head(out_param_char)) # for debugging
#    test_class <- class(out_param_char) # for debugging
#    print(paste("The parameter is ", test_class))
    out_param_num <- as.numeric(out_param_char)
    test_class <- class(out_param_num)
    print(paste("The parameter is ", test_class))
    
#    print(out_comb)
#    as.numeric(out_comb$Heart.Attack) # Doesn't work
#    as.numeric(levels(out_param_test))[out_param_test] # Doesn't work
    as.numeric(paste(out_param_test)) # Doesn't work
    as.numeric(as.character(out_param_test))
    test_class <- class(out_param_test)
    print(paste("The revised parameter is ", test_class))
    
#    print(head(out_comb))
#        print(out_table[,2])
#        print(out_table[,7])
    #    print(head(df))
  } else if(outcome == "heart failure") {
      outcome <- "heart.failure"
      out2 <- out_table[,2]
      out7 <- out_table[,7]
      out_param <- out_table[,17]
      out_comb <- data.frame("Hospital" = out2[!is.na(out_param) & out7 == state], # Test, with state
                             "State" = out7[!is.na(out_param) & out7 == state], 
                             "Heart Failure" = as.numeric(as.character
                              (out_param[!is.na(out_param) & out7 == state])))
#      print(head(out_comb))
      print(out_comb)
  } else if(outcome == "pneumonia") {
      outcome <- "pneumonia"
      out2 <- out_table[,2]
      out7 <- out_table[,7]
      out_param <- out_table[,23]
      out_comb <- data.frame("Hospital" = out2[!is.na(out_param) & out7 == state], 
                             "State" = out7[!is.na(out_param) & out7 == state], 
                             "Pneumonia" = as.numeric(as.character
                              (out_param[!is.na(out_param) & out7 == state])))
      print(head(out_comb))
  } else {
      stop("invalid outcome")
    }
  
  # Get the column of data from the selected outcome parameter
  out_table_col_names <- colnames(out_table)
#  print(out_table_col_names) # code tester this works
  print(outcome)
  for(i in 1:length(out_table_col_names)) {
    if(out_table_col_names[i] == outcome) {
      targ_outcome_col <- i
      print(targ_outcome_col)
     print(paste("targ_outcome_col is ", i))
    }
  }
  ## Check that state and outcome are valid
  # redefine outcome to be only those where the State is NOT "Not Available."
#  out_table <- out_table[!is.na(out_table), ]
#  out_table <- out_table[!out_table$as.character(outcome) == "Not Available", ] # Didn't work
#  print(outcome) # code tester
#  out_table <- out_table[!is.na(out_table[, targ_outcome_col]), ]
#  print(out_table$as.character(outcome)) #didn't work
#  print(head(out_table)) # code tester
#  print(outcome)
  ## Return hospital name in that state with lowest 30-day death rate
#  print(class(out_table$))
  out_table_sorted <- out_comb[ order(out_comb[,3], out_comb[,1]), ]
  test_unique <- length(unique(out_comb[,3]))
  print(test_unique)
    print(out_table_sorted) # print this again after debugging
#  out_best <- out_comb[min(out_comb[,3])] # Didn't work
  out_best <- out_table_sorted[1,] # Find the best hospital
#  print(out_best) # print this again after debugging
counter <- 0
# Handling ties
  for(i in 1:length(out_table_sorted[,1])){
    if(out_table_sorted[i,3] == out_table_sorted[1,3]){
      out_best[i,] <- out_table_sorted[i,]
      counter <- counter+1
    }
  }
print(cbind(as.character(out_best[1:counter,1])))
  
}