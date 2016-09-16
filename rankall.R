# Write a function called rankall that takes two arguments: 
# an outcome name (outcome) and a hospital rank- ing (num). 
# The function reads the outcome-of-care-measures.csv file 
# and returns a 2-column data frame containing the hospital 
# in each state that has the ranking specified in num. 
# For example the function call rankall("heart attack", "best") 
# would return a data frame containing the names of the hospitals 
# that are the best in their respective states for 30-day heart attack death rates. 
rankall <- function(outcome, rank) {
  ## Read outcome data
  out_table <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character", na.strings = "Not Available",
                        stringsAsFactors = FALSE)
# HEART ATTACK
  if(outcome == "heart attack") {
    outcome <- "heart.attack"
    out2 <- out_table[,2]
    out7 <- out_table[,7]
    out_param <- out_table[,11] # this works
    print("Take a quick look at the data without NAs removed")
    class_out_param <- class(out_param)
    out_comb <- data.frame("Hospital" = out2[!is.na(out_param)], 
                           "State" = out7[!is.na(out_param)], 
                           "Heart Attack" = as.numeric(as.character
                                                       (out_param[!is.na(out_param)])))
    out_param_test <- out_comb$Heart.Attack
    # Change factors to characters
    out_param_char <- as.character(out_param_test)
    out_param_num <- as.numeric(out_param_char)
    as.numeric(as.character(out_param_num))
    test_class <- class(out_param_num)
    print(paste("The parameter revised class (at line 59) is ", test_class))
    
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
    out_comb <- data.frame("Hospital" = out2[!is.na(out_param)], 
                           "State" = out7[!is.na(out_param)], 
                           "Heart Failure" = as.numeric(as.character
                                                        (out_param[!is.na(out_param)])))
    #    print(out_comb)
    print(head(out_comb))
    
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
    out_comb <- data.frame("Hospital" = out2[!is.na(out_param)], 
                           "State" = out7[!is.na(out_param)], 
                           "Pneumonia" = as.numeric(as.character
                                                    (out_param[!is.na(out_param)])))
    print(head(out_comb))
  } else {
    stop("invalid outcome")
  }
  
# NOW SORT AND ADD RANK COLUMN
  
  # Create a table of hospitals in the specified state for the designated outcome
  print(paste("Class of the outcome parameter (at line 100) is", class_out_param))
  out_param_char <- as.character(out_param)
  out_param_num <- as.numeric(out_param_char)
  as.numeric(as.character(out_param_num))
  test_class <- class(out_param_num)
  print(paste("The parameter revised class (at line 85) is ", test_class))
  
# SORT
  out_table_sorted <- out_comb[ order(out_comb[,3], out_comb[,1]), ] # 
  print(out_table_sorted[1:100, ])
  print(out_table_sorted[out_comb[,3]<10, ])
  
  out_table_col3_class <- class(out_comb[,3])
  print("out_table_col3_class is")
  print(out_table_col3_class)
  
# CREATE THE RANK COLUMN
  out_param <- out_table_sorted[, 3]
  temp_matrix <- matrix(out_param, nrow = length(out_param), ncol = 1)
  print("temp_matrix is") 
  print(head(temp_matrix))
  row_ <- row(temp_matrix)
  row_ <- as.vector(row_) # Creates a vector out of the row numbers
  
  # Now add the rank column - THIS IS FOR THE FULL SET OF STATES
  out_table_sorted <- data.frame("Hospital" = out_table_sorted[,1], 
                                 "State" = out_table_sorted[,2], 
                                 "Out Measure" = out_table_sorted[,3], #This works, but the outcome isn't always Heart failure
#                                 as.character(outcome) = out_table_sorted[,3], #Doesn't work
                                 "Rank" = row_)
  print("The sorted table is shown below")
  #  print(head(out_table_sorted))
  print(out_table_sorted[1:100, ])
  
# SPLIT THE NON-SORTED TABLE  
  table_sorted_split2 <- split(out_table_sorted, out_table_sorted$State)
  print("Split table is:")
#  print(table_sorted_split2) # Cool. This works.
  print(table_sorted_split2$WV)
# TEST LAPPLY
#  test_lapply <- lapply(table_sorted_split2, mean)
  print(names(table_sorted_split2))
  str(table_sorted_split2)
    test_lapply <- lapply(table_sorted_split2, function(dummy) {
     colMeans(dummy[, c("Out.Measure", "Rank")])
      })
  print(test_lapply)
  # NOW TRY TO SORT THE HOSPITALS IN EACH STATE BY SPECIFIED OUTCOME
  #  lapply_split <- lapply(table_sorted_split, function(dd) {
  #    order(dd[out_comb[,3], out_comb[,1]])
  #  })
  #  print(lapply_split)
  #  tapply_test <- tapply(out_comb, out_comb$State, order(out_comb[,3], 
  #                                                        out_comb[,1])) #Didn't work
  #  tapply_test <- tapply(out_comb, out_comb$State, order) #Didn't work
  
  #    print("tapply_test is")
  #  print(tapply_test)
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

