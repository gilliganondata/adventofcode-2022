# Day 6
library(tidyverse)
datastream <- read_file("data/data-day_6.txt")

# Function to find the match given the length of unique chars needed
get_match <- function(u_chars){
  
  # Cycle through and keep flipping additional chars in the vector to TRUE
  for(i in u_chars:nchar(datastream)){
    
    # Starting string
    ltr_match <- rep(FALSE, 26)
    
    # Kinda' proud of this. Keep updating that 26-value boolean vector by
    # comparing TRUE/FALSE for each character.
    for(j in (u_chars-1):0){
      ltr_match <- ltr_match |
        letters %in% substr(datastream, i-j, i-j)
    }
    if(sum(ltr_match) == u_chars){
      return(i)
    }
  }
}

cat("Part 1:", get_match(4))
cat("Part 2:", get_match(14))