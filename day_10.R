# Day 10
library(tidyverse)

signal_df <- read.table("data/data-day_10.txt", sep = " ", header = FALSE, fill = TRUE) |> 
  rename(action = "V1", val = "V2")

# Part 1

# Function to process the signal and return a 1-row or 2-row df as needed
process_sig <- function(action, val){
  if(action == "addx"){
    result <-  data.frame(sig_adjust = c(0, val))
  } else {
    result <- data.frame(sig_adjust = 0)
  }
  result
}

df <- map2_dfr(signal_df$action, signal_df$val, process_sig) |> 
  # This feels wrong...but we need to start at 1 rather than 0
  mutate(sig_adjust = if_else(row_number() == 1, 1, sig_adjust)) |> 
  # Simply accumulate the signal values
  mutate(x_reg = cumsum(sig_adjust))

# Add the cycle number
df$cycle <- seq(1,nrow(df))

# Calculate the signal strength (rearranging the columns for OCD reasons)
df <- df |> 
  select(cycle, x_reg, sig_adjust) |> 
  # Not entirely clear why I need to lag this one. So it goes.
  mutate(x_reg = lag(x_reg, 1)) |> 
  mutate(sig_strength = cycle * x_reg)

cat("Part 1 values:", df$sig_strength[seq(20, 220, 40)])
cat("Part 1:", sum(df$sig_strength[seq(20, 220, 40)]))

# Part 2
df$x_reg[[1]] <- 0

# Add the row (top to bottom) and the position
df$row <- map(seq(1,6), ~rep(.x, 40)) |> unlist()
df$col <- rep(seq(0, 39), 6)

# Do the logic (use a " " instead of a "." for readability)
df <- df |> 
  mutate(pixel = if_else(x_reg >= col - 1 & x_reg <= col + 1, "#", " "))

output <- df |> 
  group_by(row) |> 
  summarize(pixels = paste0(pixel, collapse = "")) |> 
  select(-row)

output
