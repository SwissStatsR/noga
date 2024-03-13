#' Internal function to automatically detect which noga level is required
#' @name automaticleveldetection
#' @noRd

automaticleveldetection <- function(vartype,var){
  if(vartype=="numeric"){
    noga.level <- switch(as.character(nchar(max(var,na.rm=TRUE))),
                         "2"="division.n",
                         "3"="group.n",
                         "4"="class.n",
                         "6"="type.n",
                         warning("Please provide the noga level manually, the automatic detection failed."))

  }else{
    noga.level <- switch(as.character(nchar(max(var,na.rm=TRUE))),
                         "1"="section",
                         "2"="division",
                         "3"="group",
                         "4"="class",
                         "6"="type",
                         warning("Please provide the noga level manually, the automatic detection failed."))
  }
  return(noga.level)
}
