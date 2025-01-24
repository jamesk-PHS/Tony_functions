# Depreciated due to other functionality centrally controlled by PHS available. 
# See PHS Methods package here for more:
# https://github.com/Public-Health-Scotland/phsmethods#readme
# https://scotland.shinyapps.io/phs-learnr-phsmethods/#section-introduction


#get_fy_from_date <- function(cal_date){
#  jan_to_march <- lubridate::month(cal_date)%/%4==0
#  return(
#    ifelse(jan_to_march,
#           lubridate::year(cal_date)-1,
#           lubridate::year(cal_date))
#  )
# }

# cal_dates <- lubridate::as_date(paste0("2020-",1:12,"-",1))
# 
# test_df <- tibble(
#   cal_date=cal_dates,
#   month=lubridate::month(cal_date),
#   x=as.numeric(month%/%4==0),
#   #y=month%%4,
#   fy_rowwise = lubridate::year(cal_date)-x,
#   fy_from_func=sapply(cal_dates, get_fy_from_date),
#   test = fy_rowwise==fy_from_func
# )
# test_df
