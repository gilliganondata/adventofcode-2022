# Day 8
library(tidyverse)

file <- "data/data-day_8.txt"

# Figure out how wide the grid is
grid_width <- scan(file, what = "character", nlines = 1) |> 
  nchar()
  

# Feels like it ought to be a matrix so ae can easily do rows and columns
tree_grid <- read_fwf(file, col_positions = fwf_widths(rep(1,grid_width))) |> 
  as.matrix()

# Get the number of rows (m_rows) and columns (n_cols)
m_rows <- dim(tree_grid)[[1]]
n_cols <- dim(tree_grid)[[2]]

##############
# Part 1

# Perimeter trees. All are visible.
perimeter_trees <- ((m_rows - 2) * 2) + (n_cols * 2)

int_vis_trees <- 0

# Check the trees
for(i_row in 2:(m_rows-1)){
  for(j_col in 2:(n_cols-1)){
    
    tree_height <- tree_grid[i_row, j_col]
    
    # Check for visibility
    if(
      # Look above (col)
      (max(tree_grid[1:(i_row-1), j_col]) < tree_height) | 
      # Check below (col)
      (max(tree_grid[(i_row+1):m_rows, j_col]) < tree_height) | 
      # Look left (row)
      (max(tree_grid[i_row, 1:(j_col-1)]) < tree_height) |
      # Look right (row)
      (max(tree_grid[i_row, (j_col+1):n_cols]) < tree_height)){

      int_vis_trees <- int_vis_trees + 1
      # cat("The tree at", i_row, "x", j_col, "is visible at a height of", tree_height, "\n")
    }
  }
}

total_visible <- perimeter_trees + int_vis_trees
cat("Part 1:", total_visible)

##############
# Part 2

# I can't see a way to do this other than to "crawl" in each direction. Maybe
# it would be cleaner with a function that had some smarts

max_scenic <- 0

# Check the trees
for(i_row in 2:(m_rows-1)){
  for(j_col in 2:(n_cols-1)){
    
    tree_height <- tree_grid[i_row, j_col]
    
    # Check in each direction

    # Look above (col)
    for(x in (i_row-1):1){
      abv <- i_row - x
      if(tree_grid[x, j_col] >= tree_height){
        break
      }}
    
    # Look below (col)
    for(x in (i_row+1):m_rows){
      blw <- x - i_row
      if(tree_grid[x, j_col] >= tree_height){
        break
      }}
    
    # Look left (row)
    for(x in (j_col-1):1){
      lft <- j_col - x
      if(tree_grid[i_row, x] >= tree_height){
        break
      }}
    
    # Look right (row)
    for(x in (j_col+1):n_cols){
      rgt <- x - j_col
      if(tree_grid[i_row, x] >= tree_height){
        break
      }}
  
    if(abv * blw * lft * rgt > max_scenic){
      max_scenic <- abv * blw * lft * rgt
      cat("New max scenic is", max_scenic, "located at row", i_row, "and column", j_col, "\n")
    }
    
  }
}





