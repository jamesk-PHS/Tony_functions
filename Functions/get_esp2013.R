# ********************************************************************************************
# Load European Standard Populations
# ********************************************************************************************

get_esp2013 <- function(){
  suppressWarnings(library(magrittr)) 
  esp2013_url <- 
    "https://www.opendata.nhs.scot/dataset/4dd86111-7326-48c4-8763-8cc4aa190c3e/resource/29ce4cda-a831-40f4-af24-636196e05c1a/download/european_standard_population_by_sex.csv"
  pop_esp <- readr::read_csv(esp2013_url,
                             col_types = list(AgeGroup = col_character(),
                                              Sex = col_character(),
                                              EuropeanStandardPopulation = col_double()))
  names(pop_esp) <- c("age_group","sex","pop_esp")
  pop_esp$age_group <- gsub(" years","",pop_esp$age_group)
  pop_esp$age_group <- gsub("90plus","90 plus",pop_esp$age_group)
  

  age_group_labels <- c(seq(0,90,5),Inf)
  age_group_labels <- paste0(head(age_group_labels,-1), "-", tail(age_group_labels,-1) - 1)
  age_group_labels <- gsub("-Inf", " plus", age_group_labels)
  
  # factor age_group
  pop_esp <- 
    pop_esp %>% 
    dplyr::mutate(age_group=factor(age_group,
                                   ordered = T,
                                   levels = age_group_labels))
  
  # recode sex
  pop_esp <- 
    pop_esp %>% 
    dplyr::mutate(sex=case_when(sex=="Male"~"M",
                                sex=="Female"~"F"))  
  # return tibble
  return(pop_esp)
}

