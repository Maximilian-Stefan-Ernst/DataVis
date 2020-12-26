unique_naomit <- function(vec){
  if(all(is.na(vec))){
    return(NA)
  }
  else{
    return(unique(as.vector(na.omit(vec))))
  }
}

statfromname <- function(name){
  if(str_detect(name, "mean")){return("mean")}
  if(str_detect(name, "sd")){return("sd")}
  if(str_detect(name, "min")){return("min")}
  if(str_detect(name, "max")){return("max")}
}

get_value_evn_nd <- function(dayno, value, keys){
  if(!is.na(value)){
    return(value)
  }
  else{
    key = filter(keys, dayno == !!dayno)
    return(key$evn_niceday)
  }
}

