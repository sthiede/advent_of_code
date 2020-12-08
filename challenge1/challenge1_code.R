# READ IN INPUT DATA
input = read.table('challenge1/input_data/input')

# Part 1: FIND THE two ENTRIES THAT SUM TO 2020, THEN MULTIPLY

# initialize entry sum 
entry_sum = 0 

# Iterate pairwise through the elements  
# Assumes that we want to return the product of the first 2 elements we find that 
# sum to 2020 
for (i in 1:nrow(input)){
  for (j in 1:nrow(input)){
    
    # Sum all pairwise row elements, but not the row to itself 
    if (i != j){
      entry_sum = input[i,] + input[j,]
    }
    
    # If the sum is 2020, return the product and stop the search 
    if (entry_sum == 2020){
      print(input[i,] * input[j,])
      stop('Found the two entries that sum to 2020!')
    }
    
  }
}



# Part 2: FIND THE three ENTRIES THAT SUM TO 2020, THEN MULTIPLY
# Trying out a faster way...not iterating over the length of the input matrix 
# 3 times. Rather, shortening the input matrix to entries less than 2020-row element

# initialize entry sum 
entry_sum = 0 

for (i in 1:nrow(input)){
  # Get the remainder of 2020, the goal sum and the current entry 
  remainder = 2020-input[i,]
  
  # subset matrix to those less than the remainder 
  new_mat = input[-i,, drop = FALSE] # remove the current row sp it is not included in the new matrix
  new_mat = input[input < remainder, ,drop = FALSE]

  #  if the new mat has more than 0 rows 
  if (nrow(new_mat) > 0){
    # iterate through the new smaller mat, to get the 2 numbers that sum to the
    # remainder 
    for (j in 1:nrow(new_mat)){
      for (k in 1:nrow(new_mat)){
        
        # Sum all pairwise row elements, but not the row to itself 
        if (j != k){
          entry_sum = new_mat[j,] + new_mat[k,]
        }
        
        # If the sum is 2020, return the product and stop the search 
        if (entry_sum == remainder){
          print(new_mat[j,] * new_mat[k,] * input[i,])
          stop('Found the three entries that sum to 2020!')
        }
        
      }
    }
  }
  
}

  

