if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

df <- read.csv("data/data-day1.txt", blank.lines.skip = FALSE)
df$elf_id <- NA

id <- 1000

for (i in 1:nrow(df)){
  if(!is.na(df$calories[[i]])){
    df$elf_id[[i]] <- id
  } else {
    id <- id + 1
  }
}

df_totals <- df |> 
  filter(!is.na(elf_id)) |> 
  group_by(elf_id) |> 
  summarise(total_cals = sum(calories)) |> 
  arrange(-total_cals)

cat("Total calories for top elf:", df_totals$total_cals[1])

cat("Total calories for top 3 elves:", sum(df_totals$total_cals[1:3]))