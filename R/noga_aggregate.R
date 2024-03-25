#' Function to assign a lower noga level values a higher one (e.g. assign divisions to noga groups) for aggregation. Note that this function doesn't actually aggregate, it prepares your data for aggregation. It also works only with noga-values not with its labels
#' @name noga_aggregate
#' @param var The variable containing the noga-codes or noga-values that you want to aggregate. Must be numeric or string, factor variables are not supported.
#' @param language Required if either your input or output are labels. One of "de" (German), "en" (English), "fr" (French) or "it" (Italian). Defaults to English.
#' @param level For which higher NOGA-Level you want the values? Must be one of "section" (1. level), "division" (2. level), "group" (3.), "class" (4.). "Type" is not possible because it has no lower level to aggregate.
#' @param type Determines whether your output variable is "character" or "numeric". Defaults to "numeric".
#' @importFrom plyr mapvalues
#' @returns A variable or vector that contains the values or labels of a higher noga-unit for each lower noga-unit input.
#' @export
#' @examples
#' example.data <- c(479,433,990)
#' noga::noga_aggregate(var=example.data,language="en",level="section",type="")

noga_aggregate <- function(var,language="en",level,type="numeric"){

  lookup <- noga_levels_agg
  vartype <- class(var)

  noga.level <- automaticleveldetection(vartype,var)

  #errors
  if(vartype=="factor")stop("Please provide only a numeric or character variable to recode. Factor variables are not supported.")

  #the input noga level is higher than the desired output noga level
  detected.noga.level <- gsub(pattern="(\\.n$)",replacement="",noga.level)
  nogalevelcheck(level,detected.noga.level)
  #wrong output level specified
  if(!level %in% c("section","division","group","class"))stop("Your desired noga level must be one of 'section', 'division', 'group' or 'class'")

  if(vartype!="numeric"){
    noga.level.agg <- level
  }else if(vartype=="numeric"){
    noga.level.agg <- paste0(level,".n")
  }

  lookup <- lookup[,c(eval(noga.level.agg),eval(noga.level))]
  lookup <- lookup[!is.na(lookup[,eval(noga.level)]),]

  from.vector <- as.vector(unlist(lookup[,eval(noga.level)]))
  to.vector <- as.vector(unlist(lookup[,eval(noga.level.agg)]))

  plyr::mapvalues(var,from=from.vector,to=to.vector,warn_missing=FALSE)

  }

