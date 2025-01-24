mutate_episodes <- function(col_names) {
  # https://stackoverflow.com/questions/61789717/dplyr-case-when-with-dynamic-number-of-cases
  # mostly from first answer
  # drop link_no and cis_marker as they are grouping vars
  col_names <- col_names[which(!col_names %in% c("LINK_NO","CIS_MARKER"))]
  # setNames so that the output quosure is named
  col_names <- setNames(col_names,col_names)
  # helper functions to create quosures
  quo_1st <- function(x) quo(first(!!(sym(x))))
  quo_max <- function(x) quo( max(!!(sym(x)),na.rm = T) )
  quo_min <- function(x) quo( min(!!(sym(x)),na.rm = T) )
  # get mutation expression for a single column
  mutate_col <- function(this_col) {
    if (this_col == "ADMISSION_DATE") {
      return(quo_min("ADMISSION_DATE"))
    } else if (this_col == "DISCHARGE_DATE")
    {
      return(quo_max("DISCHARGE_DATE"))
    } else
    {
      return(quo_1st(this_col))
    }
  }
  # get mutate expressions for all columns
  return(lapply(col_names, mutate_col))
}