#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate functions for hospital activity SNA indicators  

# Latest author: James Kilgour, james.kilgour2@phs.scot
# Original author: Hannah Little

# 2025-02-10
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### 1 - Housekeeping ----

# Loading packages:

library(tidyverse)


### 2 Data wrangling functions -----------------------------------------------

# These functions are adapted from the one written by Hannah Little for the MSG data
# output. This function wrangles the MSG data into the desire output format. 
# It takes the column name and the name of the variable to be summarised.
# The function wrangles dates and then the data is summarised according to its 
# hscp/locality (if applicable) via some conditional logic. 

# 2.1 Base function: 

data_wrangle_fun  <- function(file_name, ind_col, ind_name, apply_filter){
  
  # Conditionally filter for Lanarkshire
  if (apply_filter){
    
    ind_col <- file_name %>% 
      filter(., council %in% c("North Lanarkshire","South Lanarkshire"),
             # Below necessary as of April 25 update
             area_treated != "All",
             simd_quintile != "All") %>% 
      # Dates
      mutate(month = phsmethods::extract_fin_year(month),
             month = str_sub(month, 1,4)) %>%
      rename(year = month) %>% 
      select(year, everything()) %>% 
      
      # Summarise data
      group_by(council, locality, year) %>% 
      
      # Curly curly brackets {{}} and special assignment operator :=
      # used inside function as arguments are column names
      summarise({{ind_col}}:= sum({{ind_name}})) %>% 
      # mutate({{ind_col}}:= ({{ind_name}})*2) %>% works for mutate!
      
      ungroup() %>%
      arrange(year, council, locality)
    
    
  } 
  
  else {
    
    # Data wrangling
    ind_col <- file_name %>% 
      filter(.,# Below necessary as of April 25 update
             area_treated != "All",
             simd_quintile != "All") %>%  
      # Dates
      mutate(month = phsmethods::extract_fin_year(month),
             month = str_sub(month, 1,4),
             council = "Scotland",
             locality = "Scotland") %>%
      rename(year = month) %>% 
      select(year, everything()) %>% 
      
      # Summarise data
      group_by(council, locality, year) %>% 
      
      # Curly curly brackets {{}} and special assignment operator :=
      # used inside function as arguments are column names
      summarise({{ind_col}}:= sum({{ind_name}})) %>% 
      # mutate({{ind_col}}:= ({{ind_name}})*2) %>% works for mutate!
      
      ungroup() %>%
      arrange(year, council, locality)
    
  }
  
  return(ind_col)
  
  
}


# 2.2 Age:

age_breakdown <- function(file_name, ind_col, ind_name, apply_filter){
  
  # Conditionally filter for Lanarkshire
  if (apply_filter){
    
    ind_col <- file_name %>% 
      filter(., council %in% c("North Lanarkshire","South Lanarkshire"),
             # Below necessary as of April 25 update
             area_treated != "All",
             simd_quintile != "All") %>% 
      # Dates
      mutate(month = phsmethods::extract_fin_year(month),
             month = str_sub(month, 1,4)) %>%
      rename(year = month) %>% 
      select(year, everything()) %>% 
      
      # Summarise data
      group_by(council, locality, age_group, year) %>% 
      
      # Curly curly brackets {{}} and special assignment operator :=
      # used inside function as arguments are column names
      summarise({{ind_col}}:= sum({{ind_name}})) %>% 
      # mutate({{ind_col}}:= ({{ind_name}})*2) %>% works for mutate!
      
      ungroup() %>%
      arrange(year, council, locality)
    
    
  } 
  
  else {
    
    # Data wrangling
    ind_col <- file_name %>% 
      filter(.,# Below necessary as of April 25 update
        area_treated != "All",
        simd_quintile != "All") %>%  
      # Dates
      mutate(month = phsmethods::extract_fin_year(month),
             month = str_sub(month, 1,4),
             council = "Scotland",
             locality = "Scotland") %>%
      rename(year = month) %>% 
      select(year, everything()) %>% 
      
      # Summarise data
      group_by(council, locality, age_group, year) %>% 
      
      # Curly curly brackets {{}} and special assignment operator :=
      # used inside function as arguments are column names
      summarise({{ind_col}}:= sum({{ind_name}})) %>% 
      # mutate({{ind_col}}:= ({{ind_name}})*2) %>% works for mutate!
      
      ungroup() %>%
      arrange(year, council, locality)
    
  }
  
  return(ind_col)
  
  
}


# 2.3 Age:

SIMD_breakdown <- function(file_name, ind_col, ind_name, apply_filter){
  
  # Conditionally filter for Lanarkshire
  if (apply_filter){
    
    ind_col <- file_name %>% 
      filter(., council %in% c("North Lanarkshire","South Lanarkshire"),
             # Below necessary as of April 25 update
             area_treated != "All",
             simd_quintile != "All") %>% 
      # Dates
      mutate(month = phsmethods::extract_fin_year(month),
             month = str_sub(month, 1,4)) %>%
      rename(year = month) %>% 
      select(year, everything()) %>% 
      
      # Summarise data
      group_by(council, locality, simd_quintile, year) %>% 
      
      # Curly curly brackets {{}} and special assignment operator :=
      # used inside function as arguments are column names
      summarise({{ind_col}}:= sum({{ind_name}})) %>% 
      # mutate({{ind_col}}:= ({{ind_name}})*2) %>% works for mutate!
      
      ungroup() %>%
      arrange(year, council, locality)
    
    
  } 
  
  else {
    
    # Data wrangling
    ind_col <- file_name %>% 
      filter(.,# Below necessary as of April 25 update
        area_treated != "All",
        simd_quintile != "All") %>%  
      # Dates
      mutate(month = phsmethods::extract_fin_year(month),
             month = str_sub(month, 1,4),
             council = "Scotland",
             locality = "Scotland") %>%
      rename(year = month) %>% 
      select(year, everything()) %>% 
      
      # Summarise data
      group_by(council, locality, simd_quintile, year) %>% 
      
      # Curly curly brackets {{}} and special assignment operator :=
      # used inside function as arguments are column names
      summarise({{ind_col}}:= sum({{ind_name}})) %>% 
      # mutate({{ind_col}}:= ({{ind_name}})*2) %>% works for mutate!
      
      ungroup() %>%
      arrange(year, council, locality)
    
  }
  
  return(ind_col)
  
  
}


### END OF SCRIPT ###