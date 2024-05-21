#' Function to transform numeric variables to avoid issue that leading 0's will be elimianted
#' @name transform_numeric_var
#' @noRd

transform_numeric_var <- function(var){
  var <- ifelse(nchar(as.character(var))<max(nchar(as.character(var))),paste0("0",var),as.character(var))
  return(var)
}
