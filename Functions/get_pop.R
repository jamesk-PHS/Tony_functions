suppressPackageStartupMessages(
  {
    library(magrittr)
    library(dplyr)
    library(tidyr)
  }
)

get_pop_cols <- function(){

  latest_file <- list.files("/conf/linkage/output/lookups/Unicode/Populations/Estimates/", 
                            pattern = "DataZone2011_pop_est_\\d{4}_\\d{4}.rds") %>%  # List all files with matching format
    max() # Select most recent one
    
  
  return(
    readRDS(
      paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates/",latest_file) # read file
    ) %>% colnames() # Show column names
  )
}

get_pop <- function(area = "Scotland",
                    years_of_interest=2016:2020,
                    grouping_vars = c("sex", "age", "simd2020v2_sc_quintile")) {
  
  loc_dz11 <- list.files("/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/", 
                         pattern = ".rds") %>% 
    paste0(
      "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/",.) %>% 
    readRDS() %>% 
    select(hscp_locality, datazone2011)
  
  pop_raw <- list.files("/conf/linkage/output/lookups/Unicode/Populations/Estimates/", 
                        pattern = "DataZone2011_pop_est_\\d{4}_\\d{4}.rds") %>%  # List all files with matching format
    max() %>%  # Select most recent one
    paste0(
      "/conf/linkage/output/lookups/Unicode/Populations/Estimates/",.) %>%  # paste full file path and read in
    readRDS() %>% 
    mutate(country = "Scotland") %>% 
    left_join(loc_dz11, by = "datazone2011") %>% 
    rename(age90 = age90plus)
  
    cols_containing_x <-
    sapply(pop_raw, function(x)
      area %in% unique(x))
  
  cols_containing_x <-
    tibble(col = names(cols_containing_x),
           contains_x = cols_containing_x)
  filter_col <-
    cols_containing_x[cols_containing_x$contains_x,][[1, 1]]
  
  pop_wide <-
    pop_raw %>%
    filter(year %in% years_of_interest) %>%
    filter(!!sym(filter_col) == area)
  
  pop_long <-
    pop_wide %>% pivot_longer(
      cols = age0:age90,
      names_to = "age",
      values_to = "pop",
      names_prefix = "age"
    ) %>% mutate(age=as.double(age))
  
  
  grouping_vars <- unique(c("year", filter_col, grouping_vars))
  
  pop_final <-
    pop_long %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(pop = sum(pop), .groups = "drop") %>%
    rename(geography = !!sym(filter_col))
  
  return(pop_final)
}
