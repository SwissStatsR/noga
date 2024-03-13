#' Function to recode a variable containing noga codes to labels and vice versa
#' @name noga_recode
#' @param var The variable containing the noga-codes or noga-values that you want to recode. Must be numeric or string, factor variables are not supported.
#' @param language One of "de" (German), "en" (English), "fr" (French) or "it" (Italian). Defaults to English.
#' @param level The NOGA-Level you want to recode. Must be one of "auto" (Default), "section" (1. level), "division" (2. level), "group" (3.), "class" (4.) or "type" (5.). If set to "auto", the NOGA level will be identified based on the number of digits/characters of the maximum non-missing value of that variable.
#' @param to Recode values to labels or vice versa? Defaults to "auto" (will identify based on the input variable the recoding direction), or "values" will recode labels to values or "labels" will recode values to labels.
#' @returns a recoded variable in numeric() string format
#' @export
#' @examples
#' example.data <-
#' noga::noga_recode(var=example.data$,language="fr",level="section",to="auto")

noga_recode <- function(var,language="en",level="auto",to="auto"){
  lookup <- noga_lookup()
  vartype <- class(var)
  if(!to %in% c("auto","values","labels")==TRUE)stop("Please provide a valid value for the 'to' parameter. Type ?noga_recode for help.")
  if(!level %in% c("auto","section","division","group","class","type")==TRUE)stop("Please provide a valid value for the 'level' paramter. Type ?noga_recode for help.")
  if(!language %in% c("en","de","fr","it")==TRUE)stop("Please provide a valid value for the 'language' parameter. Type ?noga_recode for help.")
  if(vartype=="factor")stop("Please provide only a numeric or character variable to recode. Factor variables are not supported.")


  if(to!="auto"){
    direction.to <- to
  }else{
    direction.to <- directioncheck(to,vartype,var)
  }

  label.var <- paste0("name_",language)

  if(level!="auto"){
    if(vartype!="numeric"){
      noga.level <- level
    }else if(vartype=="numeric"){
      noga.level <- paste0(level,".n")
    }
    detected.noga.level <- gsub(pattern="(\\.n$)",replacement="",automaticleveldetection(vartype,var))
    if(noga.level!=detected.noga.level)warning("The noga level you have supplied manually in the level parameter does not correspond to the automatically detected noga level of the input variable. Expect weird output. If you think the noga level you have supplied does match the one of the varaible and that this is a bug of this function, please open an issue at https://github.com/jbeoh/noga/issues")
  }else{
    noga.level <- automaticleveldetection(vartype,var)
    if(noga.level=="Please provide the noga level manually, the automatic detection failed.")stop("Please provide the noga level manually, the automatic detection failed.")
  }

  lookup <- lookup[,c(eval(noga.level),eval(label.var))]
  lookup <- lookup[!is.na(lookup[,eval(noga.level)]),]
  if(direction.to=="values"){
    from.vector <- as.vector(unlist(lookup[,eval(label.var)]))
    to.vector <- as.vector(unlist(lookup[,eval(noga.level)]))
  }else{
    from.vector <- as.vector(unlist(lookup[,eval(noga.level)]))
    to.vector <- as.vector(unlist(lookup[,eval(label.var)]))
  }
  plyr::mapvalues(var,from=from.vector,to=to.vector,warn_missing=FALSE)

}
