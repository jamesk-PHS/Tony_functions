# population projections are only available down to hscp
# this function uses historical population data at locality level
# to estimate projections at locality level by taking the mean
# proportions of the hscp across the localities
get_pop_projections_by_locality <- function(
  pop_file,
  years_of_interest,
  years_past, # these are the years on which the pop split across localities is based
  hscps_of_interest = "South Lanarkshire",
  debugging_mode = F){
  
  # libs and custom functions
  suppressWarnings(
    {library(magrittr)
      library(dplyr)
      library(tidyr)
      library(janitor)} 
  )
  source("/conf/linkage/output/anthoc03/R/functions/add_age_group_col.R")
  source("/conf/linkage/output/anthoc03/R/functions/get_pop_data.R")
  
  # Automating fetching and loading of this filepath
  hscp_pop_proj_file <- list.files("/conf/linkage/output/lookups/Unicode/Populations/Projections/", 
             pattern = "HSCP2019_pop_proj_\\d{4}_\\d{4}.rds") %>% 
    paste0(
      "/conf/linkage/output/lookups/Unicode/Populations/Projections/",.) %>% 
    readRDS()
  
  
  
  # start of function body ####

  pop_proj_hscp <- # read in HSCP projections from cl-out
    readRDS(hscp_pop_proj_file) %>% 
    clean_names() %>% 
    filter(year %in% years_of_interest) %>% 
    select(year,hscp2019name,age,sex,pop) %>% 
    mutate(sex=case_when(sex==1~"M",
                         sex==2~"F"))
  
  pop_proj_scotland <- 
    pop_proj_hscp %>% 
    group_by(year,age,sex) %>% 
    summarise(pop_proj=sum(pop),.groups="drop") %>% 
    mutate(hscp2019name = "Scotland",
           hscp_locality = "Scotland") %>% 
    select(year,hscp2019name,hscp_locality,age,sex,pop_proj)
  
  
  # filter for hscps of interest
  
  pop_proj_hscp <- filter(pop_proj_hscp,hscp2019name %in% hscps_of_interest)
  
  # load pop data to determine proportion of pop in each locality
  pop_est <- get_pop_data(
    hscps_of_interest = hscps_of_interest,
    years_of_interest = years_past,
    pop_file = pop_file,
    aggregation_variables = c("year","hscp2019name","hscp_locality","age","sex")
  ) %>% .[["tb_pop_agg"]] #%>% rename(year=year)
  
  
  # get_pop_data() returns hscp, pan-hscp and scotland summaries, get rid of these
  pop_est_loc <-
    pop_est %>%
    filter(hscp2019name == hscps_of_interest,
           hscp_locality != hscps_of_interest)
  
  pop_est_hscp <-
    pop_est %>%
    filter(hscp2019name == hscps_of_interest,
           hscp_locality == hscps_of_interest) %>% 
    select(-hscp_locality) %>% 
    rename(pop_hscp=pop)
  
  # join loc pop (as denominator) to loc pops
  pop_est_loc <- 
    left_join(pop_est_loc,
              pop_est_hscp,
              by=c("year","hscp2019name","age","sex"))
  
  # work out proportions
  pop_est_loc <- 
    pop_est_loc %>% 
    mutate(pop_prop=pop/pop_hscp)
  
  pop_props_mean <-
    pop_est_loc %>% 
    group_by(hscp2019name,hscp_locality,age,sex) %>%
    summarise(pop_prop=mean(pop_prop),.groups="drop")
  
  # build shell of locality level pop proj
  pop_proj_locality <- 
    crossing(year=unique(pop_proj_hscp$year),
           hscp_locality=unique(pop_est_loc$hscp_locality),
           age=unique(pop_proj_hscp$age),
           sex=unique(pop_proj_hscp$sex))
  
  # join historical mean population proportions
  pop_proj_locality <- 
    left_join(
      pop_proj_locality,
      pop_props_mean,
      by=c("hscp_locality","age","sex")
    )
  
  # join hscp pop projections
  pop_proj_locality <- 
    left_join(
      pop_proj_locality,
      pop_proj_hscp,
      by=c("year","hscp2019name","age","sex")
    ) %>% rename(pop_hscp=pop)
  
  # infer locality pops
  pop_proj_locality <- 
    pop_proj_locality %>% 
    mutate(pop_proj=pop_hscp*pop_prop) %>% 
    select(year, hscp2019name, hscp_locality, age, sex, pop_proj)
  
  # standardise col names and arrangement
  pop_proj_hscp <- 
    pop_proj_hscp %>% 
    mutate(hscp_locality=hscp2019name) %>% 
    select(year, hscp2019name, hscp_locality, age, sex, pop_proj=pop)
  
  # bind projections
  pop_proj <- bind_rows(
    pop_proj_locality,
    pop_proj_hscp,
    pop_proj_scotland
  )
  
  if (debugging_mode){
    list_of_outputs <- 
      list(pop_proj_hscp=pop_proj_hscp,
           pop_est_loc=pop_est_loc,
           pop_est_hscp=pop_est_hscp,
           pop_props_mean=pop_props_mean,
           pop_proj_locality=pop_proj_locality,
           pop_proj_scotland=pop_proj_scotland,
           pop_proj=pop_proj)
    return(list_of_outputs)
  } else {
    return(pop_proj)
  }
  
  
  
}

#function_output <- get_pop_projections_by_locality()

# checks in debugging mode: ####

# function_output[["pop_est_loc"]]
# 
# # check proportions add up to 1
# function_output[["pop_est_loc"]] %>% 
#   group_by(fy,hscp2019name,age,sex) %>% 
#   summarise(pop_prop=sum(pop_prop),.groups="drop") %>% .[["pop_prop"]] %>% `==`(1) %>% all()
# 
# # check proportions add up to 1 (allowing for very small rounding errors)
# function_output[["pop_props_mean"]]
# function_output[["pop_props_mean"]] %>% 
#   group_by(hscp2019name,age,sex) %>% 
#   summarise(pop_prop=sum(pop_prop),.groups="drop") %>% .[["pop_prop"]] %>% abs() %>% `-`(1) %>% `<`(1e-6) %>% all()
# 
# this_x <- function_output$pop_proj_hscp
# pop_proj_locality <- tibble(
#   fy=unique(function_output$pop_proj_hscp$fy)
# )
#   
# function_output[["pop_proj_locality"]] %>% 
#   select(-hscp_locality) %>% 
#   group_by(!!!syms(colnames((select(.,fy:sex))))) %>% 
#   summarise(pop_hscp=sum(pop_loc))
# 
# function_output[["pop_proj_hscp"]]

  