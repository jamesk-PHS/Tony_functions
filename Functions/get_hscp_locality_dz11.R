get_hscp_locality_dz11 <- function(this_pattern="(r|R)(d|D)(s|S)",
                                   these_cols=c("hscp2019name","hscp2019","hscp_locality","datazone2011","datazone2011name",
                                                "hb2019name", "hb2019", "hb2018","hb2014")){
  lkup_path <- list.files("/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality",
                     pattern=this_pattern,
                     full.names = T)
  #lkup_path <- gsub("//","/",lkup_path)
  
  #return(list(lkup_path,lkup))
  if (length(lkup_path)>1) {
    
    error_message <- 
      paste0(
        "There are more than 1 HSCP Localities_DZ11_Lookup_*.rds files;\n",
        "please specify a pattern that identifies the required file\n",
        paste0(lkup_path,collapse = "\n")
      )
    
    stop(error_message)
  } else {
    lkup <- readRDS(lkup_path)
    lkup <- lkup[,these_cols]
    lkup <- unique(lkup)
    return(lkup)
  }

}

