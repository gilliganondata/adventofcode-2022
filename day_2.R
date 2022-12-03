library(tidyverse)

# Lookup for scoring
df_lookup <- data.frame(play = c("Rock", "Paper", "Scissors"),
                        p1_code = c("A", "B", "C"),
                        p2_code = c("X", "Y", "Z"),
                        val = c(1, 2, 3))

# Get the data
df <- read.delim("data/data-day_2.txt", sep=" ", header = FALSE)

# p1 as the opponent; p2 as me
colnames(df) <- c("p1", "p2")

# Kinda' overloading the data with details
df_1 <- df |> 
  left_join(df_lookup, by = c(p1 = "p1_code")) |> 
  rename(p1_play = play, p1_val = val) |> 
  left_join(df_lookup, by = c(p2 = "p2_code")) |> 
  rename(p2_play = play, p2_val = val)

# Clunky (may bite me on a part 2) scoring
df_1 <- df_1 |> 
  mutate(score = p2_val +
           case_when(
             p1_play == p2_play ~ 3,
             p1_play == "Scissors" & p2_play == "Rock" ~ 6,
             p1_play == "Rock" & p2_play == "Scissors" ~ 0,
             p1_val < p2_val ~ 6,
             p1_val > p2_val ~ 0,
             TRUE ~ -999999
           ))

cat("The total score for part 1 is:", sum(df_1$score))

# Part 2 - Going to do a redo
df_lookup_2 <- data.frame(result = c("Lose", "Draw", "Win"),
                          result_code = c("X", "Y", "Z"),
                          result_score = c(0, 3, 6))

# New data version
df_2 <- df |> 
  left_join(df_lookup, by = c(p1 = "p1_code")) |> 
  rename(p1_play = play, p1_val = val) |> 
  left_join(df_lookup_2, by = c(p2 = "result_code")) |> 
  select(-p2_code, -p1_val)

# Calculate p2's play
df_2 <- df_2 |> 
  mutate(p2_play = case_when(
    result == "Draw" ~ p1_play,
    result == "Win" & p1_play == "Rock" ~ "Paper",
    result == "Win" & p1_play == "Paper" ~ "Scissors",
    result == "Win" & p1_play == "Scissors" ~ "Rock",
    result == "Lose" & p1_play == "Rock" ~ "Scissors",
    result == "Lose" & p1_play == "Paper" ~ "Rock",
    result == "Lose" & p1_play == "Scissors" ~ "Paper",
    TRUE ~ "OTHER"
  ))

# Get the base value
df_2 <- df_2 |> 
  left_join(df_lookup, by = c(p2_play = "play")) |> 
  select(-p1_code, -p2_code) |> 
  rename(base_score = val) |> 
  mutate(total_score = result_score + base_score)

cat("The total score for part 2 is:", sum(df_2$total_score))
