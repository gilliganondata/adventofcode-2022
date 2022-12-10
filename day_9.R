# Day 9
library(tidyverse)

moves_df <- read_delim("data/data-day_9.txt", delim = " ", col_names = FALSE) |> 
  rename(dir = "X1", steps = "X2") 

# Expand the data frame so that each row is one move
moves <- map2(moves_df$dir, moves_df$steps, ~ rep(.x, .y)) |> unlist()
df <- data.frame(move = c("s", moves), head_x = 0, head_y = 0, tail_x = 0, tail_y = 0)

# Calculate all of the positions AFTER the move
for(i in 2:nrow(df)){
  
  # The head moves are pretty straightforward
  df$head_x[i] <- df$head_x[i] + case_when(
    df$move[i] == "R" ~ df$head_x[i-1] + 1,
    df$move[i] == "L" ~ df$head_x[i-1] - 1,
    TRUE ~ df$head_x[i-1])
  
  df$head_y[i] <- df$head_y[i] + case_when(
    df$move[i] == "U" ~ df$head_y[i-1] + 1,
    df$move[i] == "D" ~ df$head_y[i-1] - 1,
    TRUE ~ df$head_y[i-1])
  
  # The tail has to follow..if it needs to, so see how far away
  # the tail is from the new position of the head
  new_h_vs_prev_t_x <- df$head_x[i] - df$tail_x[i-1]
  new_h_vs_prev_t_y <- df$head_y[i] - df$tail_y[i-1]
  
  # If the new head position is within 1 space of the current tail,
  # then the tail doesn't move
  if(abs(new_h_vs_prev_t_x) <= 1 & abs(new_h_vs_prev_t_y) <= 1){
    df$tail_x[i] <- df$tail_x[i-1]
    df$tail_y[i] <- df$tail_y[i-1]
  } else {
    # Otherwise, "follow" the head to wherever it was last
    df$tail_x[i] <- df$head_x[i-1]
    df$tail_y[i] <- df$head_y[i-1]
  }
}

# Part 1: just count the unique tail_x + tail_y combinations
pt_1 <- df |> 
  mutate(tail_combined = paste0(tail_x, "|", tail_y)) |> 
  pull(tail_combined) |> unique() |> length()

cat("Part 1:", pt_1)

# Part 2: 10-knot rope
# Same basic approach...but with a function to cascade the results. Unfortunately,
# "just follow the previous segment" es no bueno for this (I'd initially thought it
# was), so getting a little hacky.

# We can re-use the head moves
df_2 <- data.frame(move = c("s", moves), x0 = df$head_x, y0 = df$head_y, x1 = 0, y1 = 0,
                   x2 = 0, y2 = 0, x3 = 0, y3 = 0, x4 = 0, y4 = 0, x5 = 0, y5 = 0,
                   x6 = 0, y6 = 0, x7 = 0, y7 = 0, x8 = 0, y8 = 0, x9 = 0, y9 = 0)

for(i in 2:nrow(df)){
  
  # Use base notation and "walk" through the snake from left to right. Each
  # pair of columns is the next x/y for the next segment in the snake.
  for(j in 2:10){
    
    col_x <- j * 2
    col_y <- col_x + 1
    
    # Check to see where the "segment that's dragging me" falls relative
    # to where I already am.
    new_vs_prev_x <- df_2[i, col_x - 2] - df_2[i-1, col_x]
    new_vs_prev_y <- df_2[i, col_y - 2] - df_2[i-1, col_y]
    
    # Check if the segment needs to move
    if(abs(new_vs_prev_x) <= 1 & abs(new_vs_prev_y) <= 1){
      df_2[i, col_x] <- df_2[i-1, col_x]
      df_2[i, col_y] <- df_2[i-1, col_y]
    } else 
      # If the move required is JUST horizontal or vertical move 1 in towards it.
      if(new_vs_prev_x == 0 | new_vs_prev_y == 0) {
        df_2[i, col_x] <- df_2[i-1, col_x] + new_vs_prev_x/2
        df_2[i, col_y] <- df_2[i-1, col_y] + new_vs_prev_y/2
      } else # Now things get messy
        # Conditions where the resulting move is up-right diagonal
        if(new_vs_prev_x > 0 & new_vs_prev_y > 0){
          df_2[i, col_x] <- df_2[i-1, col_x] + 1
          df_2[i, col_y] <- df_2[i-1, col_y] + 1
        } else
          # Conditions where the resulting move is down-right diagonal
          if(new_vs_prev_x > 0 & new_vs_prev_y < 0){
            df_2[i, col_x] <- df_2[i-1, col_x] + 1
            df_2[i, col_y] <- df_2[i-1, col_y] - 1
          } else
            # Conditions where the resulting move is down-left diagonal
            if(new_vs_prev_x < 0 & new_vs_prev_y < 0){
              df_2[i, col_x] <- df_2[i-1, col_x] - 1
              df_2[i, col_y] <- df_2[i-1, col_y] - 1
            } else
              # Conditions where the resulting move is up-right diagonal
              if(new_vs_prev_x < 0 & new_vs_prev_y > 0){
                df_2[i, col_x] <- df_2[i-1, col_x] - 1
                df_2[i, col_y] <- df_2[i-1, col_y] + 1
              }
  } 
}

pt_2 <- df_2 |> mutate(p9 = paste0(x9, "|", y9)) |> 
  pull(p9) |> unique() |> length()

cat("Part 2:", pt_2)

