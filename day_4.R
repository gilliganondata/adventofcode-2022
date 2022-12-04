# Day 4
library(tidyverse)

# Get the data
df <- read.csv("data/data-day_4.txt", header = FALSE)
colnames(df) <- c("elf_1", "elf_2")

# Function to convert the text range to a sequence
get_seq <- function(rg_text){
  start <- gsub("(^.*)\\-.*", "\\1", rg_text) |> as.numeric()
  end <- gsub("^.*\\-(.*$)", "\\1", rg_text) |> as.numeric()
  seq(start, end, 1)
}

df_1 <- df |> 
  rowwise() |> 
  mutate(fully_contained = case_when(
    all(get_seq(elf_1) %in% get_seq(elf_2)) ~ TRUE,
    all(get_seq(elf_2) %in% get_seq(elf_1)) ~ TRUE,
    TRUE ~ FALSE
  ),
 overlap_exists = if_else(
    any(get_seq(elf_1) %in% get_seq(elf_2)), TRUE, FALSE)
  )

cat("Part 1: The total cases where one elf's assignment fully contains another:", sum(df_1$fully_contained))
cat("Part 2: The total cases where one elf's assignment overlaps with another:", sum(df_1$overlap_exists))