# ***************************************************************************
# Header ####
# ***************************************************************************
# ***************************************************************************
# Author: antony.clark2@phs.scot
# Date: April 2021

# Input:
# Wide-format population data for Scotland at dz11 level

# Output:
# Long-format population data for:
# Localities within HSCPs of interest &
# HSCP,
# Pan-HSCPs of interest
# Scotland
# source()


# Rest of Scotland (i.e. exclusing HSCPs of interst) with breakdown by:
# year hscp2019name     hscp_locality    sex   age_group simd_sc    pop

# Note:
# There are no overlapping populations in the output
# i.e. if aggregated by year, the summary will give pop for Scotland

# ***************************************************************************
# Libraries #### 
# ***************************************************************************

suppressPackageStartupMessages({
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(janitor)
})



#rm(list=ls())
get_pop_data <- function(
  hscps_of_interest = "South Lanarkshire",
  pan_hscp_name,
  years_of_interest,
  aggregation_variables,
  pop_file = "/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2020.rds",
  geog_lookup = "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20200825.rds",
  age_group_breaks = c(seq(0,90,5), Inf),
  file_tag = NULL

){
  
  # ***************************************************************************
  # Parameters #### 
  # ***************************************************************************

  # age cols selected by default; age_group added later so cannot select here
  selection_variables <- aggregation_variables[!(aggregation_variables %in% c("age","age_group"))]
  # if a consolidated SIMD2016/2020 is required then include "simd_sc" in the aggregation_variables
  simd_sc_colnames <- c("simd2016_sc_quintile","simd2020v2_sc_quintile")
  selection_variables <- 
    if("simd_sc" %in% aggregation_variables){
      c(selection_variables[!(selection_variables %in% "simd_sc")],simd_sc_colnames)
    } else{
      selection_variables
    }

  

  # ***************************************************************************
  # Custom functions #### 
  # ***************************************************************************
  
  # add age group colum #
  source("/conf/linkage/output/anthoc03/R/functions/add_age_group_col.R")
  source("/conf/linkage/output/anthoc03/R/functions/get_all_summaries.R")
  
  # ***************************************************************************
  # Load reference data #### 
  # ***************************************************************************
  
  {
    # DZ11-Locality-HSCP lookup file
    hscp_locality_dz_orig <- readRDS(geog_lookup) %>% clean_names()
    rm(geog_lookup)
    # read rds file into tibble
    tb_pop_wide_orig <- readRDS(pop_file) %>% clean_names()
    rm(pop_file)
    gc()
  }
  
  # ***************************************************************************
  # Get Population Estimate Data ####
  # ***************************************************************************
  
  # Join locality and subset columns
  tb_pop_wide <- tb_pop_wide_orig %>%
    left_join(hscp_locality_dz_orig %>% select(hscp2019name, hscp_locality, datazone2011),
              by = c("datazone2011", "hscp2019name"))
  rm(hscp_locality_dz_orig)
  rm(tb_pop_wide_orig)
  gc()
  
  tb_pop_wide <- tb_pop_wide %>%
    select(all_of(selection_variables), 
           contains("age")) %>%
    # rename the 90 plus column to make pivoting longer easy
    rename(age90 = age90plus)
  
  # ***************************************************************************
  # Consolidate SIMD SCOTLAND 2016 & 2020 ####
  # ***************************************************************************
  
  if ("simd_sc" %in% aggregation_variables){
    tb_pop_wide <- tb_pop_wide %>%
      mutate(simd_sc = case_when(
        year <= 2016 ~ simd2016_sc_quintile,
        year >= 2017 ~ simd2020v2_sc_quintile
      )) %>%
      select(-c(simd2016_sc_quintile, simd2020v2_sc_quintile)) %>%
      select(simd_sc, everything()) %>% 
      mutate(simd_sc=factor(simd_sc,ordered=T,levels = 1:5))  
  }
  
  # age names of columns to be pivoted long
  age_cols <-
    tb_pop_wide %>% select(starts_with("age")) %>% colnames()
  
  
  # Aggregate to Locality. N.B. age is wide at this stage so do not need to group by age
  tb_pop_wide <- tb_pop_wide %>%
    group_by(across(all_of(selection_variables)),.drop = FALSE) %>%
    summarise(across(all_of(age_cols), sum),.groups="drop") %>%
    ungroup()
  
  # ***************************************************************************
  # Pivot longer ####
  # ***************************************************************************
  
  # Pivot longer (variables to cases)
  tb_pop_long <- tb_pop_wide %>%
    pivot_longer(
      cols = all_of(age_cols),
      names_prefix = "age",
      names_to = "age",
      values_to = "pop"
    )
  rm(age_cols,tb_pop_wide)
  # We must convert the new age column to numeric
  tb_pop_long <- tb_pop_long %>%
    mutate(age = as.numeric(age))
  
  # Code below no longer required as we have 2020 data
  # # repeat 2019 data for 2020 to allow later right join to work
  # tb_pop_long_2020_temp <- tb_pop_long %>%
  #   filter(year == 2019) %>%
  #   mutate(year = 2020)
  # 
  # tb_pop_long <- bind_rows(tb_pop_long, tb_pop_long_2020_temp)
  # rm(tb_pop_long_2020_temp)
  # gc()
  
  # Add age group
  tb_pop_long <- tb_pop_long %>% add_age_group_col(age_col = age,
                                                   age_group_col = "age_group",
                                                   breaks = age_group_breaks)
  rm(age_group_breaks)
  
  # Aggregate up to age_group 
  tb_pop_agg <- tb_pop_long %>%
    # This is the most granular we will need pop data
    group_by(across(all_of(aggregation_variables)),.drop=F) %>%
    summarise(pop = sum(pop),.groups="drop") %>% 
    ungroup() %>% 
    clean_names()
  
  # ********************************************************************************
  # Create population records for larger areas (Localities, Scotland) ####
  # ********************************************************************************
  
  tb_pop_agg <-
    get_all_summaries(df = tb_pop_agg,
                      grp_vars = aggregation_variables,
                      level_2s_of_interest = hscps_of_interest) %>%
    filter(year %in% years_of_interest)
  
  
  # ********************************************************************************
  # Create label e.g. for use in a filename ####
  # ********************************************************************************
  
  breakdown_desc <- 
    tb_pop_agg %>% 
    colnames() %>% 
    .[!grepl("pop",.)] %>% 
    paste0(collapse = "_") %>% 
    gsub("hscp2019name","hscp",.) %>% 
    gsub("hscp_locality","locality",.) %>% 
    paste0(.,file_tag)
  
  
  gc()
  
  return(list(tb_pop_agg=tb_pop_agg,
              description=breakdown_desc))
  
  # END OF SCRIPT ####
  
}

# pop_file_wide  <-
#   readRDS(
#     paste0(
#       "/conf/linkage/output/lookups/Unicode/Populations/Estimates/",
#       "DataZone2011_pop_est_2011_2019.rds"
#     )
#   ) 

# Examples of how to call the function ####
# pop_file_wide <- 
#   get_pop_data(aggregation_variables = c("year","hscp2019name",
#                                          "hscp_locality","age_group"),
#              hscps_of_interest = "South Lanarkshire",
#              years_of_interest = 2015:2020) %>% .[[1]]
# 
# pop_file_wide %>% View()
# tail(pop_file_wide)
# get_pop_data(aggregation_variables = c("year","hscp2019name",
#                                        "hscp_locality","age_group"),
#              hscps_of_interest = "South Lanarkshire",
#              years_of_interest = 2015:2020) %>% .[[2]]

# get_pop_data(aggregation_variables = c("year","hscp2019name",
#                                         "hscp_locality","age_group"),
#               hscps_of_interest = "South Lanarkshire",
#               years_of_interest = 2015:2020,
#              age_group_breaks = c(0,18,65,Inf)) %>% .[[1]]
