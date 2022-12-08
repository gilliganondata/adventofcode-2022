# Day 7
library(tidyverse)
term_out <- read.delim("data/data-day_7.txt", header=FALSE)

# Ignore lines starting with $ ls or dir (thanks, Stew)
df <- term_out |> 
  filter(!grepl("(^\\$.ls.*)|(^dir.*).*", V1)) |> 
  mutate(path = NA, filename = NA, filesize = NA)

# First row (hack, I know)
df$path[1] <- "/"

for(i in 2:nrow(df)){
  # Go through each row and figure out the file path (excluding the file) by looking
  # at the command and then referencing the path on the previous row
  df$path[[i]] <- case_when(
    
    # cd .. means lop off a directory
    grepl("^\\$.cd.\\.\\.*", df$V1[[i]]) ~ gsub("(^.*/)(.*/$)", "\\1", df$path[[i-1]]),
    
    # other cd means add to the directory
    grepl("^\\$.cd.*", df$V1[[i]]) ~ paste0(df$path[[i-1]], gsub("^\\$.cd.(.*$)", "\\1", df$V1[[i]]), "/"),
    
    # otherwise, it's a file, so keep the path
    TRUE ~ df$path[[i-1]]
  )
  
  # Split out the filename and filesize for files
  if(!grepl("^\\$.*", df$V1[[i]])){
    df$filename[[i]] <- gsub("^.* (.*$)", "\\1", df$V1[[i]])
    df$filesize[[i]] <- gsub("(^.*) .*$", "\\1", df$V1[[i]]) |> as.numeric()
  }
  
}

# Flatten down to just the directory totals
df <- df |> 
  group_by(path) |> 
  summarize(filesize = sum(filesize, na.rm = TRUE))

df$total_filesize <- NA

# Now, we need to get the directory totals to include the totals for subdirectories,
# which we can do just by matching the start. This...feels brute force-y
for(i in 1:nrow(df)){
  
  base_path <- df$path[[i]]
  
  total_filesize <- df |> 
    # Clip the paths to be identical length of the match path
    mutate(path = substr(path, 1, nchar(df$path[[i]]))) |> 
    filter(path == base_path) |> 
    group_by(path) |> 
    summarize(filesize = sum(filesize)) |> 
    ungroup() |> 
    pull(filesize)
  
  df$total_filesize[[i]] <- total_filesize
  
}

# Part 1
part_1 <-  df |> 
  filter(total_filesize <= 100000) |> 
  pull(total_filesize) |> sum()

cat("Part 1:", part_1)

# Part 2
total_used <- sum(df$filesize)
current_unused <- 70000000 - total_used
space_needed <- 30000000 - current_unused

# Getting late...
cat("Part 2:", df |> filter(total_filesize >= space_needed) |> 
  pull(total_filesize) |> 
  min())
