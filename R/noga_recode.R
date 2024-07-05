#' Function to recode a variable containing noga codes to labels and vice versa
#' @name noga_recode
#' @param var The variable containing the noga-codes or noga-values that you
#'   want to recode. Must be numeric or string, factor variables are not
#'   supported. Note that the function can handle only variables that contain
#'   one NOGA-level only.
#' @param language One of "de" (German), "en" (English), "fr" (French) or "it"
#'   (Italian). Defaults to English.
#' @param level The NOGA-Level you want to recode. Must be one of "auto"
#'   (Default), "section" (1. level), "division" (2. level), "group" (3.),
#'   "class" (4.) or "type" (5.). If set to "auto", the NOGA level will be
#'   identified based on the number of digits/characters of the maximum
#'   non-missing value of that variable. If all the labels belong to more than
#'   one level, the deepest one is always used. The variable you recode can only
#'   contain one NOGA-level.
#' @param to Recode values to labels or vice versa? Defaults to "auto" (will
#'   identify based on the input variable the recoding direction), or "values"
#'   will recode labels to values or "labels" will recode values (codes in
#'   string format) to labels.
#' @param warn TRUE (default) or FALSE. When TRUE will give warnings about
#'   non-matching recodings.
#' @returns a recoded variable in string format (either noga-labels, or
#'   noga-codes).
#' @export
#' @examples
#' example.data <- data.frame(
#'   test1 = c(702,620),
#'   test2 = c("Management consultancy activities","Extraction of natural gas"),
#'   test3=c("0702","0620"))
#' noga::noga_recode(var=example.data$test1,language="fr",level="group",to="auto")
#' noga::noga_recode(var=example.data$test2,language="en",level="auto",to="values")
#' noga::noga_recode(var=example.data$test3,language="de",level="auto",to="auto")

noga_recode <- function(var,language="en",level="auto",to="auto",warn=TRUE){
 lookup <- noga::lookup
  vartype <- class(var)
  runerrors(to,level,language,vartype)
  label.var <- paste0("name_",language)
  if(vartype %in% c("numeric","integer")){
    if(min(nchar(as.character(var)))!=max(nchar(as.character(var)))){
      var <- transform_numeric_var(var)
    }
  }

  if (to != "auto") {
    direction.to <- to
  } else {
    direction.to <- directioncheck(vartype, var)
  }



  if(level!="auto"){

    detected.noga.level <- gsub(pattern="(\\.n$)",replacement="",automaticleveldetection(var,language))
    if(level!=detected.noga.level)warning("The noga level you have supplied manually in the level parameter does not correspond to the automatically detected noga level of the input variable. Expect weird output. If you think the noga level you have supplied does match the one of the varaible and that this is a bug of this function, please open an issue at https://github.com/jbeoh/noga/issues")
    noga.level <- level
    }else{
    noga.level <- automaticleveldetection(var,language)
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

  matched.var <- match(var, from.vector)
  missings.matched.var <- is.na(matched.var)
  if(warn==TRUE){
    non.matched <- var[missings.matched.var]
    if(length(non.matched)>0){
    message("The following values in the supplied variable could not be recoded and has been set to `NA` as there was no according NOGA-level match: ",paste(non.matched,collapse=","))
    }
  }
  var[!missings.matched.var] <- to.vector[matched.var[!missings.matched.var]]
  var[missings.matched.var] <- NA
  #var <- iconv(var, from = "UTF-8", to = "latin-1")
  return(var)
}

