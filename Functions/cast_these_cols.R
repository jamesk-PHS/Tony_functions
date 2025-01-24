# helper function to cast column types
cast_these_cols <- function(df,these_cols,fun=as.double){
  df[,these_cols] <- lapply(df[,these_cols],FUN = fun)
  return(df)
}