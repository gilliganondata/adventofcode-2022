# Day 3
library(tidyverse)

# Get the data
df <- read.csv("data/data-day_3.txt", header = FALSE)
colnames(df) <- "items"

# Get the priorities
df_priorities <- data.frame(item_id = c(letters, LETTERS),
                            item_priority = seq(1:52))

# PART 1
df_1 <- df |> 
  # Split into two compartments
  mutate(comp_1 = substr(items, 1, nchar(items)/2),
         comp_2 = substr(items, nchar(items)/2 + 1, nchar(items))) |> 
  # Find the item ID that is in both compartments
  rowwise() |> 
  mutate(common_val = intersect(unlist(strsplit(comp_1, split = "")), 
                                unlist(strsplit(comp_2, split = "")))) |> 
  ungroup() |> 
  # Get the priority score for that item
  left_join(df_priorities, by = c(common_val = "item_id"))

cat("Part 1: The sum of the priorities is:", sum(df_1$item_priority))

# PART 2
df_2 <- df
# Put them in groups based on blocks of 3
df_2$group_id <- rep(1:(nrow(df_2)/3), each = 3)
# Make what will become the three column names
df_2$intra_group_id <- paste0("elf_", seq(1:3))

# Widen the data and find the common item
df_2 <- df_2 |>
  pivot_wider(group_id, names_from = intra_group_id, values_from = items) |> 
  # The same trick from above to find the intersection (but intersect only works
  # with two inputs, so have to do this in two steps)
  rowwise() |>
  mutate(common_item = intersect(unlist(strsplit(elf_1, split = "")),
                                 unlist(strsplit(elf_2, split = ""))) |> 
           intersect(unlist(strsplit(elf_3, split = "")))) |> 
  # Get the priority score
  left_join(df_priorities, by = c(common_item = "item_id"))

cat("Part 2: The sum of the priorities is:", sum(df_2$item_priority))

