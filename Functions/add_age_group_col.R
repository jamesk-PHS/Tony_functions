# add age group colum #
add_age_group_col <-
  function(df, # a dataframe
           age_col, # raw column name of age column (in years)
           age_group_col = "age_group", # char string for new age_group col
           breaks, # age breaks for cut()
           labels = NULL) {
    # the double braces are required in order to reference the raw column names supplied as arguments
    # the colon in := is essential...
    # df = data frame or tibble with an age column
    # age_col
    if (is.null(labels)) {
      labels <- paste0(head(breaks,-1), "-", tail(breaks,-1) - 1)
      labels <- gsub("-Inf", " plus", labels)
    }
    df %>%
      mutate({{age_group_col}} := cut({{age_col}},
                                      breaks = breaks,
                                      right = FALSE,
                                      ordered_result = TRUE,
                                      labels = labels)) %>%
      return()
  }