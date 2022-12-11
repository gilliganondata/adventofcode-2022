# Day 11
library(tidyverse)
# library(gmp) # For Part 2. The numbers get really big

# Seems like this should be a list with each monkey being an item
monkeys_vec <- scan("data/data-day_11_test.txt", what = character(), sep = "\n") |> 
  trimws()

monkeys <- list()

# Just going to change one variable and then run this once for part 1 and once for part 2
part <- 1

if(part == 2){
  worry_level <- as.bigz("3")
  num_rounds <- 20
} else {
  worry_level <- as.bigz("1")
  num_rounds <- 20
}

# Get the monkeys data into a list
for (i in 1:(length(monkeys_vec)/6)){
  
  # Base vector position
  pos_base <- (i - 1) * 6 + 1
  
  # Build the list
  monkeys[[i]] <- list(
    id = gsub("^.*(\\d+)\\:$", "\\1", monkeys_vec[pos_base]),
    # Vector of items (numeric, since we'll be doing math on them)
    items = gsub("^.*\\: (.*$)", "\\1", monkeys_vec[pos_base + 1]) |> 
      str_split(", ") |> unlist() |> as.bigz(),
    # It looks like it's just multiplication, addition, or square the value, so this can be hardcoded
    operation_type = case_when(
      substr(monkeys_vec[pos_base + 2], 22, 26) == "* old" ~ "square", 
      substr(monkeys_vec[pos_base + 2], 22, 22) == "*" ~ "multiply",
      TRUE ~ "add"),
    # If we're squaring the item, then there is no amount
    operation_amount = if_else(substr(monkeys_vec[pos_base + 2], 22, 26) == "* old", as.bigz("-99"),
                               gsub("^.*(\\*|\\+) (.*$)", "\\2", monkeys_vec[pos_base + 2]) |> as.bigz()),
    test = gsub("^.*divisible by.(.*$)", "\\1", monkeys_vec[pos_base + 3]) |> as.bigz(),
    # We're going to store the *index* of the monkey, which will be the *value* of the "throw two) 
    # PLUS 1
    if_true = gsub("^.*to.monkey.(.*$)", "\\1", monkeys_vec[pos_base + 4]) |> as.numeric() + 1,
    if_false = gsub("^.*to.monkey.(.*$)", "\\1", monkeys_vec[pos_base + 5]) |> as.numeric() + 1,
    # And we've got to count the inspections, so put a counter in for that
    inspection_ct = 0
  )
}

for (i in 1:num_rounds){
  for (m in 1:length(monkeys)){
    cat("Monkey being processed is:", monkeys[[m]]$id, "\n")
    
    # Function to inspect and act on an item
    item_inspect <- function(item_val){
      
      cat("Monkey:", i, "and item #", m, "which is", item_val, "of type", typeof(item_val), "\n")
      # Perform the initial operation
      item_val <- case_when(
        monkeys[[m]]$operation_type == "multiply" ~ item_val * monkeys[[m]]$operation_amount,
        monkeys[[m]]$operation_type == "add" ~ item_val + monkeys[[m]]$operation_amount,
        TRUE ~item_val^2
      )
      
      # Divide by 3 and round down
      item_val <- floor(item_val/worry_level)
      
      # Perform the test
      test <- if_else(item_val %% monkeys[[m]]$test == 0, TRUE, FALSE)
      # cat("Item val:", item_val, "and test result is",test, "\n")
      
      # Throw the item. Glad I remembered the <<- operation (without even Googling for it!).
      # I feel like this is a further hack, though.
      if(test){
        monkeys[[monkeys[[m]]$if_true]]$items <<- c(monkeys[[monkeys[[m]]$if_true]]$items, item_val)
      } else {
        monkeys[[monkeys[[m]]$if_false]]$items <<- c(monkeys[[monkeys[[m]]$if_false]]$items, item_val)
      }
      
      # Increment the inspection
      monkeys[[m]]$inspection_ct <<- monkeys[[m]]$inspection_ct + 1
    }
    
    # Process all the items and then wipe out the items for the monkey
    map(monkeys[[m]]$items, item_inspect)
    monkeys[[m]]$items <- NULL
  }
}

# Make a data frame with the inspection counts
inspections_df <- map_dfr(monkeys, ~ data.frame(monkey_id = .x$id, insp_ct = .x$inspection_ct)) |> 
  arrange(-insp_ct)

cat("Part 1:", inspections_df$insp_ct[1] * inspections_df$insp_ct[2], "\n")
