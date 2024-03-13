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
  runerrors(to,level,language,vartype)

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
  }else{
    noga.level <- automaticleveldetection(vartype,var)
  }

  if(direction.to=="values"){
    plyr::mapvalues(var,from=lookup[,eval(label.var)],to=lookup[,eval(noga.level)],warn_missing=FALSE)
  }else{
    plyr::mapvalues(var,from=lookup[,eval(noga.level)],to=lookup[,eval(label.var)],warn_missing=FALSE)
  }

}
