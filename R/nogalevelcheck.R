#' Internal function to automatically detect whether input for noga aggregate function cannot be aggregated to desired output noga level.
#' @name nogalevelcheck
#' @noRd

nogalevelcheck <- function(level,detected.noga.level){

  level.numeric <- switch(level,
                          "section"=1,
                          "division"=2,
                          "group"=3,
                          "class"=4,
                          "type"=5)

  detected.level.numeric <- switch(level,
                                   "section"=1,
                                   "division"=2,
                                   "group"=3,
                                   "class"=4,
                                   "type"=5)

  if(level.numeric==detected.level.numeric)stop("Your input variable has the same noga level as your desired output. No aggregation possible. If you want to recode values and labels for that level, use noga_recode function.")
  if(detected.level.numeric<level.numeric)stop(paste0("Your input variable has the noga-level ",detected.noga.level,", your desired aggergation is noga-level ",level,". This function can only aggregate, not disaggregate."))

}
