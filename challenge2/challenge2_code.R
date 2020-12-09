# Given a list of rules and passwords, how many are valid?

# READ IN INPUT DATA 
passwords = read.table('challenge2/input_data/input')

# Part 1 ----
# Password rule is that the password must contain the specified range of letter

# CLEAN DATA 
# renames columns
colnames(passwords) = c('Range', 'Letter', 'Password')
# remove colon after letter
passwords$Letter = gsub(':', '', passwords$Letter)


# COUNT LETTERS IN STRING USING TIDYVERSE! 
library(tidyverse)
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
