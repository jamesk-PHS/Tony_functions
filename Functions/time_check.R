time_check <- function(start,end=Sys.time(),text_to_display) {
  end <- Sys.time()
  diff_in_secs <- as.numeric(difftime(end, start, units = "secs"))
  hour_component <- diff_in_secs %/% (60 * 60)
  min_component <- (diff_in_secs %/% 60) - hour_component * 60
  sec_component <-
    diff_in_secs - (hour_component * 60 * 60) - (min_component * 60)
  my_pad <-
    function(x)
      formatC(x,
              width = 2,
              format = "d",
              flag = "0")
  l <- list(
    hour_component = hour_component,
    min_component = min_component,
    sec_component = sec_component
  )
  rm(hour_component,min_component,sec_component)
  l <- lapply(l,my_pad)
  l_collapse <- paste0(l,collapse = ":")
  dt_char <- paste("2020-01-01",l_collapse)
  dur_hms <- substr(dt_char,nchar(dt_char)-7,nchar(dt_char))
  message <- paste0(
    "\n",
    "********************************************","\n",
    "Execution time to this point, ",text_to_display,", is (HH:MM:ss):\n",
    dur_hms,"\n",
    "********************************************"
  )
  cat(message)
  return(dur_hms)
}
