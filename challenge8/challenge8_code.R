# PART 1 GOAL ----: 
# With these rules: 
# acc = add the value, advance 1 line
# jmp = advance specified number of lines
# nop = advance 1 line 

# Immediately before any instruction is executed a second time, 
# what value is in the accumulator?

# READ IN INPUT
input = read.table('challenge8/input_data/input')

# RULES AS A FUNCTION 
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
# curr_i = current index 
# hist_i = vector of all indices we've been before 
# acc = accumulator value 
curr_i = 1
hist_i = curr_i
acc = 0 

while (!curr_i %in% hist_i[-length(hist_i)]){ # remove the last curr_i, that is, ignore the curr_i until the next iteration
  print('start')
  print(curr_i)
  print(acc)
  output = rules(input[curr_i,1], input[curr_i, 2], acc, curr_i)
  hist_i = c(hist_i, output[[1]])
  print('here')
  curr_i = output[[1]]
  acc = output[[2]]
  
  print(hist_i)
  print(curr_i)
  print(acc)
  
}

print(acc)

# PART 2 GOAL ----: 



