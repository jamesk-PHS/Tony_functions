suppressPackageStartupMessages(
  {
    library(magrittr)
    library(dplyr)
    library(tidyr)
  }
)

get_pop_cols <- function(){

  latest_file <- list.files("/conf/linkage/output/lookups/Unicode/Populations/Estimates/", 
                            pattern = "DataZone2011_pop_est_\\d{4}_\\d{4}.rds") %>% 
    max()
    
  
  return(
    readRDS(
      paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates/",latest_file)
    ) %>% colnames()
  )
}

get_pop <- function(area = "Scotland",
                    years_of_interest=2016:2020,
                    grouping_vars = c("sex", "age", "simd2020v2_sc_quintile")) {
  loc_dz11 <-
    readRDS(
      "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20220630.rds"
    )
  loc_dz11 <-
    loc_dz11 %>% select(hscp_locality, datazone2011)
  pop_raw <-
    readRDS(
      "/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2020.rds"
    )
  pop_raw <-
    pop_raw %>% mutate(country = "Scotland")
  pop_raw <-
    pop_raw %>% left_join(loc_dz11, by = "datazone2011")
  pop_raw <-
    pop_raw %>% rename(age90 = age90plus)
  
  pop_raw[grepl("f|F",pop_raw$sex),"sex"] <- "F"
  pop_raw[grepl("m|M",pop_raw$sex),"sex"] <- "M"
  
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
# pop_raw <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2020.rds")
# pop_raw$datazone2011[1]
# pop_Sco <- get_pop(area="Scotland")
# pop_SL <- get_pop(area="South Lanarkshire")
# pop_Ha <- get_pop(area="Hamilton")
# my_summary <- function(df,...) df %>% group_by(...) %>% summarise(pop=sum(pop),.groups="drop")
# pop_Sco %>% my_summary(year,simd2020v2_sc_quintile)
# pop_SL %>% my_summary(year)
# pop_Ha %>% my_summary(year)
# get_pop("Scotland",years_of_interest = 2020)
# 
# 
# pop_test <- 
#   lapply(c("Hamilton","South Lanarkshire","Scotland"), function(x) get_pop(x,years_of_interest = 2020)) %>% 
#   bind_rows()
# 
# pop_dz <- get_pop(pop_raw$datazone2011[1])
# 
# fill_levels <- function(df,levels_list){
#   these_cols <- names(levels_list)
#   #return(these_cols)
#   df %>%
#     mutate(across(all_of(these_cols),
#                   ~factor(.x,
#                           ordered=is.numeric(.x),
#                           levels=levels_list[[cur_column()]])))
# }
# these_levels <- list(year=2016:2020,
#                      sex=c("M","F"),
#                      age=0:90,
#                      simd2020v2_sc_quintile=1:5)
#   
# pop_dz %>% fill_levels(these_levels)
#   
# f <- function(df,l){
#   col_sym <- sym(names(l)[1])
#   col_name <- as.character(col_sym)
#   these_levels <- l[[col_name]]
#   is_ordered <- is.numeric(df[[col_name]])
#   #return(list(col_sym,col_name,these_levels))
#   pop_raw %>%
#     mutate(!!col_sym:=factor(!!col_sym,
#                              ordered = is_ordered,
#                              levels = these_levels)) %>% 
#     select(!!col_sym)
#   
# }
# 
# pop_raw %>% f(list(year=2011:2020))
# 
# 
# l <- list(year=2011:2020)
# col_syms <- syms(names(l))
# 
# pop_raw %>% 
#   mutate(across(year,~factor(.x,ordered = T,levels=l[[cur_column()]])))

  









