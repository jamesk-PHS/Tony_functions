get_sql_age_group <- function(age_group_breaks
                              ) {
  # generate 'case when' SQL to create age_group variable
  all_clauses <- NULL
  for (i in 1:(length(age_group_breaks) - 1)) {
    from <- age_group_breaks[i]
    to <- age_group_breaks[i + 1]
    # create the clause
    clause <- paste0(
      "WHEN \'age\' >=",
      from,
      " AND \'age\' <",
      to,
      " THEN ",
      "'",
      from,
      "-",
      (to - 1),
      "'",
      "\n"
    )
    # fix for clause for the end case (when >Inf)
    clause <- gsub("-Inf", " plus", clause)
    clause <- gsub("AND \'age\' <Inf ", "", clause)
    clause <- gsub("plus',", "plus'", clause)
    all_clauses <- paste0(all_clauses, clause)
  }
  sql_statement <- paste0("CASE\n",
                          all_clauses,
                          "END")
  return(sql_statement)
}