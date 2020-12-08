# READ IN INPUT DATA
input = read.table('challenge1/input_data/input')

# FIND THE TWO ENTRIES THAT SUM TO 2020, THEN MULTIPLY

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
