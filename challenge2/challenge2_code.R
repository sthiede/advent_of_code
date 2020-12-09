# Given a list of rules and passwords, how many are valid?


# READ IN INPUT DATA 
passwords = read.table('challenge2/input_data/input')

# Part 1 ----
# Password rule is that the password must contain the specified range of letter

# Using stringr to solve this problems! 
library(stringr)

# CLEAN DATA 
# renames columns
colnames(passwords) = c('Range', 'Letter', 'Password')
# remove colon after letter
passwords$Letter = gsub(':', '', passwords$Letter)


# COUNT LETTERS IN STRING USING TIDYVERSE! 
meets_criteria = rep(NA, nrow(passwords))
for (i in 1:nrow(passwords)){
  
  # how many times does the letter appear in the password?
  letter_count = str_count(passwords$Password[i], passwords$Letter[i])
  
  # Split the range 
  range_split = str_split(passwords$Range[i], '-', simplify = TRUE)
  min_letter = as.numeric(range_split[1,1])
  max_letter = as.numeric(range_split[1,2])
  
  meets_criteria[i] = letter_count >= min_letter  & letter_count <= max_letter
    
  
}


sum(meets_criteria)


# Part 2 ----
# Password rule describes 2 positions in the password. Exactly one of those positions
# must contain the letter specified 

# Can use substr in base R to get the character value at specified string positions 
meets_criteria = rep(NA, nrow(passwords))
for (i in 1:nrow(passwords)){
  
  # Get positions 
  pos = as.vector(as.numeric(str_split(passwords$Range[i], '-', simplify = TRUE)))
  
  # Get value at position 1 and position 2 
  values = c(substr(passwords$Password[i], pos[1], pos[1]), substr(passwords$Password[i], pos[2], pos[2]))
  
  # Meets criteria if only 1 value is the specified letter   
  meets_criteria[i] = sum(values == passwords$Letter[i]) == 1
 
   
}

sum(meets_criteria)

passwords[meets_criteria,]
