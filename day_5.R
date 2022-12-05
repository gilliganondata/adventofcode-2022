# Day 4
library(tidyverse)

# Get the data. This feels pretty hacky, but we're going to iterate
filename <- "data/data-day_5.txt"

# Find the # of rows that have a "#" in the first column
df <- read.delim(filename, sep = " ", header = FALSE, blank.lines.skip = FALSE)

total_rows <- nrow(df)

# Figure out the tallest stack of crates by looking for the first row with "move"
# in it and backing off from there
max_crates <- which(df$V1=="move") |> min() - 3

########
# Get the crate data. This is super hacky
########

# Get the bottom row to figure out how wide this whole thing is
num_stacks <- read.delim(filename, header = FALSE, skip = max_crates-1, nrow = 1) |> 
  pull(V1) |> nchar() + 1
num_stacks <- num_stacks/4

# I'm going to assume that the stacks will always be ordered from 1 to n
df_crates <- read_fwf(filename, col_positions = fwf_widths(rep(4, num_stacks)), n_max = max_crates)
colnames(df_crates) <- c(1: num_stacks)
df_crates_matrix <- as.matrix(df_crates)

# Make a list where each item is a vector with the "stack" for that item
# with the first item in the vector being the bottom item

# Function to make each vector
get_stack_vec <- function(stack_num){
  # Grab a column and then reverse the order
  stack <- df_crates[,stack_num] |> unlist() |> as.vector() |> rev()
  
  # Drop the NAs
  stack <- stack[!is.na(stack)]
  
  # Remove the brackets
  stack <- gsub("^\\[(.*)\\]", "\\1", stack)
  
  stack
}

# Now make the list
stacks <- map(seq(1:num_stacks), get_stack_vec)

# Make a copy for part 2
stacks_2 <- stacks

########
# Get the movement data. This is a similar hack as the above
########

# Read in as a df and clean up the colunames
df_moves <- read.delim(filename, header = FALSE, skip = max_crates+2, sep = " ") |> 
  select(num_crates = V2, from = V4, to = V6)

########
# Walk through the movements. Can't see doing this without loops
########

for(i in 1:nrow(df_moves)){
  
  mv_num_crates <- df_moves$num_crates[[i]]
  mv_from <- df_moves$from[[i]]
  mv_to <- df_moves$to[[i]]
  
  for(j in 1:mv_num_crates){
    # Get the item
    item_id <- stacks[[mv_from]][length(stacks[[mv_from]])]
    # Remove it from the "from" stack
    # cat("from:", stacks[[mv_from]], "-> ")
    stacks[[mv_from]] <- stacks[[mv_from]][1:length(stacks[[mv_from]])-1]
    # cat(stacks[[mv_from]], "\n")
    
    # Add it to the "to" stack
    # cat("to:", stacks[[mv_to]], "-> ")
    stacks[[mv_to]] <- c(stacks[[mv_to]], item_id)
    # cat(stacks[[mv_to]], "\n\n")
  }
}

# And...get the solution to Part 1
part_1 <- ""
for(k in 1:num_stacks){
  part_1 <- paste0(part_1, stacks[[k]][[length(stacks[[k]])]])
}

cat("Part 1:", part_1)

##########
# Part 2
##########

########
# Walk through the movements. Simpler! Although... taking shortcuts that are
# making it less readable
########

for(i in 1:nrow(df_moves)){
  
  mv_num_crates <- df_moves$num_crates[[i]]
  mv_from <- df_moves$from[[i]]
  mv_to <- df_moves$to[[i]]
  
  # Get the item(s)
  item_id <- stacks_2[[mv_from]][(length(stacks_2[[mv_from]])-mv_num_crates+1):length(stacks_2[[mv_from]])]
  # Remove it from the "from" stack
  cat("from:", stacks_2[[mv_from]], "-> ")
  stacks_2[[mv_from]] <- stacks_2[[mv_from]][1:(length(stacks_2[[mv_from]]) - mv_num_crates)]
  cat(stacks_2[[mv_from]], "\n")
  
  # Add it to the "to" stack
  cat("to:", stacks_2[[mv_to]], "-> ")
  stacks_2[[mv_to]] <- c(stacks_2[[mv_to]], item_id)
  cat(stacks_2[[mv_to]], "\n\n")
}

# And...get the solution to Part 1
part_2 <- ""
for(k in 1:num_stacks){
  part_2 <- paste0(part_2, stacks_2[[k]][[length(stacks_2[[k]])]])
}

cat("Part 2:", part_2)
