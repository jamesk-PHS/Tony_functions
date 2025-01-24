get_age_group_cases <- function(age_col_name,age_group_breaks,age_group_labels){
  # lower age bounds
  lower <- head(age_group_breaks,-1)
  # upper age bounds
  upper <- tail(age_group_breaks,-1)
  # create cases as character vector
  age_group_cases_as_text <- paste(
    age_col_name,">=", lower, "&", age_col_name, "<", upper,"~",paste0('\"',age_group_labels,"\"")
  )
  # drop the <Inf part if it exists
  Inf_string <- paste0(" & ",age_col_name," < Inf")
  age_group_cases_as_text <- gsub(Inf_string,"",age_group_cases_as_text)
  # return list of expressions
  return(rlang::parse_exprs(age_group_cases_as_text))
  
}