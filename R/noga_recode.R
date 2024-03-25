#' Function to recode a variable containing noga codes to labels and vice versa
#' @name noga_recode
#' @param var The variable containing the noga-codes or noga-values that you
#'  want to recode. Must be numeric or string, factor variables are not supported.
#' @param language One of "de" (German), "en" (English), "fr" (French) or "it"
#'  (Italian). Defaults to English.
#' @param level The NOGA-Level you want to recode. Must be one of "auto"
#' (Default), "section" (1. level), "division" (2. level), "group" (3.),
#' "class" (4.) or "type" (5.). If set to "auto", the NOGA level will be
#' identified based on the number of digits/characters of the maximum non-missing
#' value of that variable.
#' @param to Recode values to labels or vice versa? Defaults to "auto"
#' (will identify based on the input variable the recoding direction), or
#' "values" will recode labels to values or "labels" will recode values to labels.
#' @importFrom plyr mapvalues
#' @returns a recoded variable in numeric() string format
#' @export
#' @examples
#' example.data <- data.frame(
#'   test1 = c(702,620),
#'   test2 = c("Management consultancy activities","Extraction of natural gas"),
#'   test3=c("0702","0620"))
#' noga::noga_recode(var=example.data$test1,language="fr",level="section",to="auto")
#' noga::noga_recode(var=example.data$test2,language="en",level="auto",to="values")
#' noga::noga_recode(var=example.data$test3,language="de",level="auto",to="auto")

noga_recode <- function(var,language="en",level="auto",to="auto"){
  lookup <- noga_levels
  vartype <- class(var)
  runerrors <- function(to,level,language,vartype)

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
