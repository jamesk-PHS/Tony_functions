get_all_summaries <- function(df,
                              level_1="Scotland",
                              level_2="hscp2019name",
                              level_3="hscp_locality",
                              level_2s_of_interest=c("North Lanarkshire","South Lanarkshire"),
                              pan_level_2_name = "Lanarkshire",
                              grp_vars=c("fy",level_2,level_3,"age_group"),
                              .debug=F){

  # level 3 (e.g. locality)
  if (level_3!=level_2){
    level_3_summary <-
      df %>%
      filter(.data[[level_2]] %in% level_2s_of_interest) %>% 
      group_by(across(all_of(grp_vars)),.drop=FALSE) %>%
      summarise(across(where(is.numeric),sum),.groups="drop")  
  } else(
    level_3_summary <- NULL
  )
  

  # level 2 (e.g. hscp)
  level_2_summary <-
    df %>%
    filter(.data[[level_2]] %in% level_2s_of_interest) %>% 
    group_by(across(all_of(grp_vars)),.drop=FALSE) %>%
    summarise(across(where(is.numeric),sum),.groups="drop") %>% 
    mutate({{level_3}} := .data[[level_2]]) %>%
    group_by(across(all_of(grp_vars)), .drop = FALSE) %>%
    summarise(across(where(is.numeric), sum), .groups = "drop")
  


  # pan-level 2 (e.g. both North and South Lanarkshire)
  if(length(level_2s_of_interest)>1){
    pan_level_2_summary <-
      level_2_summary %>%
      mutate({{level_2}}:=pan_level_2_name,
             {{level_3}}:=pan_level_2_name) %>%
      group_by(across(all_of(grp_vars)),.drop=FALSE) %>%
      summarise(across(where(is.numeric),sum),.groups="drop")
  } else{
    pan_level_2_summary <- NULL
  }
  
  
  # level 1 (e.g. Scotland)
  level_1_summary <- 
    df %>% 
    mutate({{level_2}}:=level_1,
           {{level_3}}:=level_1) %>% 
    group_by(across(all_of(grp_vars)),.drop=FALSE) %>% 
    summarise(across(where(is.numeric),sum),.groups="drop")
  
  # all summarise list
  all_summaries_list <- 
    list(
      level_3=level_3_summary,
      level_2=level_2_summary,
      pan_level_2=pan_level_2_summary,
      level_1=level_1_summary
    )
  # all summaries df
  all_summaries_df <- 
    bind_rows(
      level_3_summary,
      level_2_summary,
      pan_level_2_summary,
      level_1_summary
    )
  #return(level_2==level_3)
  ifelse(.debug==T,
         return(all_summaries_list),
         return(all_summaries_df))

}

