#' Internal function to check whether recoding values to labels or vice versa
#' @name directioncheck
#' @noRd

directioncheck <- function(to,vartype,var){
  if(to=="auto" & vartype=="numeric"){
    direction.to="labels"
  }else if(to=="auto" & vartype=="character" & max(nchar(var))>1){
    direction.to="values"
  }else if(to=="auto" & vartype=="character" & max(nchar(var))==1){
    direction.to="labels"
  }
  return(direction.to)
}
