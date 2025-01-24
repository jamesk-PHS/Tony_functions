# This script queries SMR01.PI and counts emergency hospital admissions by
# aggregation_variables (dates, age filters can be applied)
# age groups can be created

# Memory housekeeping and script timing
# dummy

# ********************************************************************************************
# Libs/functions ####
# ********************************************************************************************
#suppressPackageStartupMessages(
#  {
#    library(magrittr)
#    library(dplyr)
#    library(tidyr)
#    library(lubridate)
#    library(readr)
#    library(odbc)
#    library(janitor)    
#  }
#)
#
#source("/conf/linkage/output/anthoc03/R/functions/get_all_summaries.R")
#source("/conf/linkage/output/anthoc03/R/functions/get_sql_age_group.R")
#
#
#get_SMR01_data <- function(username,
#                           hscps_of_interest,
#                           pan_hscp_name,
#                           discharge_from_stays,
#                           discharge_to_stays,
#                           episode_cols,
#                           age_group_breaks,
#                           aggregation_variables,
#                           geog_lookup = paste0(
#                             "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/",
#                             "HSCP Localities_DZ11_Lookup_20200825.rds"
#                           ),
#                           age_filter = NULL,
#                           filename_tag = NULL,
#                           hscps_of_interest_smra = NULL) {
#  
#  
#  
#  
#  # ********************************************************************************************
#  # parameters derived from function arguments ####
#  # ********************************************************************************************
#  
#  # extract episodes 3 months earlier than desired discharge from date
#  discharge_from_episodes <-
#    discharge_from_stays %>% as_date() %>% `%m-%`(months(3)) %>% as.character()
#  
#  # build age group labels
#  age_group_labels <-
#    paste0(head(age_group_breaks,-1),
#           "-",
#           tail(age_group_breaks,-1) - 1)
#  
#  # change last label to e.g. 90 plus rather than having 90-Inf
#  age_group_labels <- gsub("-Inf", " plus", age_group_labels)
#  
#  # ********************************************************************************************
#  # Geography ref data ####
#  # ********************************************************************************************
#  
#  # We need the DZ11-Locality lookup to join Locality for aggregation
#  
#  hscp_locality_dz11 <-
#    readRDS(geog_lookup) %>%
#    select(hscp2019name, hscp2019, hscp_locality, datazone2011)
#  
#  hscp_name_code <-
#    hscp_locality_dz11 %>%
#    select(hscp2019name, hscp2019) %>%
#    unique()
#  
#  if (!is.null(hscps_of_interest_smra)) {
#    hscp_of_interest_code <-
#      hscp_name_code %>%
#      filter(hscp2019name == hscps_of_interest_smra) %>%
#      pull(hscp2019)
#  }
#  
#  # ******************************************************************************
#  # Prepare for SQL queries via dbplyr ####
#  # ******************************************************************************
#  
#  emergency_codes <- c(20:22, 30:39)
#  
#  # use this in the select(); it renames the columns as per episode_cols vector's names
#  episode_select <-
#    episode_cols %>% lapply(., function(string)
#      quo(!!sym(string)))
#  
#  # use this to ref cols after they've been renamed in the initial select()
#  episode_cols_v2 <-
#    setNames(names(episode_cols), names(episode_cols))
#  
#  # this is how we always sort esisodes
#  episode_arrange <- quos(link_no,
#                          cis,
#                          admission_date,
#                          discharge_date,
#                          desc(admission_type))
#  zx
#  # this is how we group episodes to determine CISs
#  group_by_for_stays <- quos(link_no, cis)
#  
#  # most times we choose first() except dates which are min, max for admission, discharge
#  episodes_mutate <- c(
#    # firsts
#    episode_cols_v2[!grepl("date", names(episode_cols))] %>%
#      lapply(., function(string)
#        quo(first(!!sym(
#          string
#        )))),
#    # min
#    episode_cols_v2[grepl("admission_date", names(episode_cols))] %>%
#      lapply(., function(string)
#        quo(min(
#          !!sym(string), na.rm = T
#        ))),
#    # max
#    episode_cols_v2[grepl("discharge_date", names(episode_cols))] %>%
#     lapply(., function(string)
#       quo(max(
#         !!sym(string), na.rm = T
#       )))
# )
# 
## # ******************************************************************************
## Get Hospital Episodes from SMR01 ####
  # ******************************************************************************
# 
# channel <- dbConnect(
#    odbc(),
#   dsn = "SMRA",
#    uid = username,
#    pwd = .rs.askForPassword("LDAP Password:")
#  )

#  episodes <-
#    tbl(channel, "SMR01_PI") %>%
#    select(!!!syms(episode_cols)) %>%
#    filter(
#      discharge_date >= TO_DATE(discharge_from_episodes, 'YYYY-MM-DD'),
#      discharge_date <= TO_DATE(discharge_to_stays, 'YYYY-MM-DD'),
#      admission_type %in% emergency_codes
#    )
# 
#
#  # ******************************************************************************
#  # Get Emergency Admissions from Episodes from SMR01 ####
#  # ******************************************************************************
#  
#  emergency_admissions_patient <-
#    episodes %>%
#    arrange(!!!episode_arrange) %>%
#    group_by(!!!group_by_for_stays) %>%
#    mutate(!!!episodes_mutate) %>%
#    group_by(!!!syms(episode_cols_v2)) %>%
#    summarise(n_episodes = n()) %>%
#    filter(
#      discharge_date >= TO_DATE(discharge_from_stays, 'YYYY-MM-DD'),
#      discharge_date <= TO_DATE(discharge_to_stays, 'YYYY-MM-DD')
#    ) %>%
#    ungroup()
#  
#  # ******************************************************************************
#  # Apply age filter
#  # ******************************************************************************
#  
#  if (!is.null(age_filter)) {
#    emergency_admissions_patient <-
#      emergency_admissions_patient %>%
#      filter(!!rlang::parse_expr(age_filter))
#  }
#  
#  # ******************************************************************************
#  # Join HSCP name, Locality ####
#  # ******************************************************************************
#  
#  dbWriteTable(channel,
#               "LOCALITY_DZ11",
#               hscp_locality_dz11 %>% select(-hscp2019),
#               overwrite = T)
#  # Verify the new table has been created
#  dbListTables(channel, schema = toupper(username))
#  DB_LOCALITY_DZ11 <-
#    tbl(channel, dbplyr::in_schema(toupper(username), "LOCALITY_DZ11"))
#  
#  emergency_admissions_patient <-
#    emergency_admissions_patient %>%
#    left_join(DB_LOCALITY_DZ11,
#              by = "datazone2011")
#  
#  
#  # ******************************************************************************
#  # Filter for HSCP of interest ####
#  # ******************************************************************************
#  
#  if (!is.null(hscps_of_interest_smra)) {
#    emergency_admissions_patient <-
#      emergency_admissions_patient %>%
#      filter(hscp2019name == hscps_of_interest_smra)
#  }
#  
#  # ******************************************************************************
#  # Create age groups ####
#  # ******************************************************************************
#  if ("age_group" %in% aggregation_variables) {
#    # https://stackoverflow.com/questions/61789717/dplyr-case-when-with-dynamic-number-of-cases
#    # mostly from first answer
#    
#    age_group_cases <-
#      rlang::parse_exprs(paste0(
#        "age >=",
#        head(age_group_breaks,-1),
#        "& age<",
#        tail(age_group_breaks,-1),
#        "~",
#        '\"',
#        age_group_labels,
#        '\"'
#      ))
#    
#    # SQL doest not allow "<Inf"
#    age_group_last_case <-
#      age_group_cases[[length(age_group_cases)]] %>% as.character() %>% gsub("& age < Inf", "", .)
#    age_group_cases[length(age_group_cases)] <-
#      rlang::parse_exprs(
#        paste0(
#          age_group_last_case[2],
#          age_group_last_case[1],
#          '\"',
#          age_group_last_case[3],
#          '\"'
#        )
#      )
#    
#    emergency_admissions_patient <-
#      emergency_admissions_patient %>%
#      mutate(age_group = case_when(!!!age_group_cases))
#  }
#  
#  # ******************************************************************************
#  # Derive FY ####
#  # ******************************************************************************
#  
#  # We need to split the mutate into two steps or SQL transaltion fails
#  # 1) get cal_year and month
#  # 2) derive fy from cal_year and month
#  
#  emergency_admissions_patient <-
#    emergency_admissions_patient %>%
#    mutate(
#      cal_year = sql('extract(year from "discharge_date")'),
#      month = sql('extract(month from "discharge_date")')
#    )
#  
#  emergency_admissions_patient <-
#    emergency_admissions_patient %>%
#    mutate(fy = case_when(month %in% 1:3 ~ cal_year - 1,
#                          month %in% 4:12 ~ cal_year))
#  
#  
#  # ******************************************************************************
#  # Aggregate ####
#  # ******************************************************************************
#  
#  emergency_admissions_aggregate <-
#    emergency_admissions_patient %>%
#    group_by(!!!syms(aggregation_variables)) %>%
#    summarise(n_emergency_admissions = n())
#  
#  # ******************************************************************************
#  # Collect aggregated data ####
#  # ******************************************************************************
#  
#  emergency_admissions_aggregate <-
#    emergency_admissions_aggregate %>%
#    collect() %>%
#    clean_names()
#  
#  if ("age_group" %in% aggregation_variables){
#    emergency_admissions_aggregate <-
#      emergency_admissions_aggregate %>%
#      mutate(age_group = factor(age_group,
#                                ordered = T,
#                                levels = age_group_labels))  
#  }
#  
#  if ("sex" %in% aggregation_variables){
#    emergency_admissions_aggregate <-
#      emergency_admissions_aggregate %>%
#      filter(sex != 0) %>%
#      mutate(sex = case_when(sex == "1" ~ "M",
#                             sex == "2" ~ "F")) %>% 
#      filter(!is.na(sex))  
#  }
#  
#  
#  # ******************************************************************************
#  # Filter for HSCPs of interest, get pan-HSCP summary and Scotland summary ####
#  # ******************************************************************************
#  
#  ea_all_summaries <- emergency_admissions_aggregate %>%
#    ungroup() %>%
#    get_all_summaries(level_2s_of_interest = hscps_of_interest,
#                      grp_vars = aggregation_variables) %>%
#    arrange(across(all_of(aggregation_variables)))
#  
#  
#  breakdown_desc <- ea_all_summaries %>%
#    colnames() %>%
#    .[!grepl("n_emergency_admissions", .)] %>%
#    paste0(collapse = "_") %>%
#    paste0("ea_", .) %>%
 ##   paste0(., "_", filename_tag) %>%
#    gsub("hscp2019name_", "", .) %>%
#    gsub("hscp_locality", "locality", .) %>% 
#    paste0(.,"_",min(ea_all_summaries$fy),"_",max(ea_all_summaries$fy))
#  
#  return(list(ea_all_summaries = ea_all_summaries,
#              description = breakdown_desc))
#  
#}