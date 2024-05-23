#' Internal function to check whether recoding values to labels or vice versa
#' @name directioncheck
#' @noRd

directioncheck <- function(to,vartype,var){
  if(to=="auto" & vartype%in%c("numeric","integer")){
    direction.to="labels"
  }else if(to=="auto" & vartype=="character" & any(grepl("[a-z]",var))){
    direction.to="values"
  }else if(to=="auto" & vartype=="character" & (max(nchar(var))==1)|all(grepl("[0-9]",var))){
    direction.to="labels"
  }
  return(direction.to)
}
