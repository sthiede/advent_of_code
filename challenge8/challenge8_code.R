# PART 1 GOAL ----: 
# With these rules: 
# acc = add the value, advance 1 line
# jmp = advance specified number of lines
# nop = advance 1 line 

# Something is broken in the instruction manual, so it's stuck in an infinite loop...
# Immediately before any instruction is executed a second time, 
# what value is in the accumulator?

# READ IN INPUT
input = read.table('challenge8/input_data/input')

# RULES AS A FUNCTION 
# inputs: 
# 1. command = string "nop", "jmp" or "acc"
# 2. value = value in the second column, where jmp value tells you how far to jump, 
# acc value gets added to the accumulator and nop value means nothing
# 3. accumuator = numeric, numbers getting added to this as we move around
# 4. index = numeric, update the index based on the rules described above 
rules <- function(command, value, accumulator, index){
  if (command == "nop"){
    accumulator = accumulator + 0 
    index = index + 1
  } else if (command == "jmp"){
    accumulator = accumulator + 0 
    index = index + value
  } else if (command == 'acc'){
    index = index + 1
    accumulator = accumulator + value 
  }else {
    stop('Expects command to be nop, jmp or acc')
  }
  return(list(index, accumulator))
}

# Intialize variables 
# curr_i = current index 
# hist_i = vector of all indices we've been before 
# acc = accumulator value 
curr_i = 1
hist_i = curr_i
acc = 0 

# Iterate through until our curr_i appears in our hist_i 
while (!curr_i %in% hist_i[-length(hist_i)]){ # remove the last curr_i, that is, ignore the curr_i until the next iteration

  # run function 
  output = rules(input[curr_i,1], input[curr_i, 2], acc, curr_i)
  
  # update hist_i
  hist_i = c(hist_i, output[[1]])
  
  # update curr_i 
  curr_i = output[[1]]
  
  # update acc 
  acc = output[[2]]
  
  
}
# Print the accumulator value when the loop breaks, and that's the answer! 
print(acc)

# PART 2 GOAL ----: 
# a conversion of one jmp -> nop or nop -> jmp will result in fixing the infinite loop
# fix the problem and get the accumulator value 

# We know that the problem is one of the numbers in our hist_i, because it results
# in an infinite loop. Let's see how many jmp's and nop's we have in those

# Get a vector of indices that could be wrong 
could_be_wrong = hist_i[input[hist_i,1] %in% c('jmp', 'nop')]
length(could_be_wrong) # number of indices that could be wrong 

# The program ran correctly if there are no duplicated indices in the history of indices
# does_termindate()
# input = vector of indices 
# returns TRUE/FALSE, TRUE if there are no duplicated indices 
does_terminate <- function(indices){
  length(indices) == length(unique(indices))
}

# opposite()
# input = string "jmp" or "nop" 
# Return "jmp" if "nop" is inputted and vice versa
opposite <- function(jmp_or_nop){
  if (jmp_or_nop == 'jmp'){
    return('nop')
  }else if (jmp_or_nop == 'nop'){
    return('jmp')
  }else{
    stop('Expects string of jmp or nop')
  }
}

# Loop through each index that could be wrong 
for (index in could_be_wrong){
  
  # Reinitialize values each time
  curr_i = 1 #  current index 
  hist_i = curr_i # vector of all indices we've been before 
  acc = 0 # accumulator value 
  
  # Change nop -> jmp or vice versa, one at at time, for each that could be wrong
  input_fixed = input
  input_fixed[index,1] = opposite(input_fixed[index,1])
  
  while (!curr_i %in% hist_i[-length(hist_i)]){    
    
    # if the curr_i is the last row of our commands, exit the while loop 
    if (curr_i == nrow(input_fixed)){
      break
    }
    
    # run commands 
    output = rules(input_fixed[curr_i,1], input_fixed[curr_i, 2], acc, curr_i)
    
    # update hist_i
    hist_i = c(hist_i, output[[1]])
    
    # update curr_i 
    curr_i = output[[1]]
    
    # update accumulator 
    acc = output[[2]]
    
    
  }
  
  # Determine if the while loop got to the end of the matrix by checking if 
  # all the hist_i are unique 
  if (does_terminate(hist_i)){
    print(acc)
    stop('Fixed the broken loop!')
  }
}


