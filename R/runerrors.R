#' Intenral function to run error messages in case noga_recode parameters are invalid
#' @name runerrors
#' @noRd

runerrors <- function(to,level,language,vartype){
  if(!to %in% c("auto","values","labels")==TRUE)stop("Please provide a valid value for the 'to' parameter. Type ?noga_recode for help.")
  if(!level %in% c("auto","section","division","group","class","type")==TRUE)stop("Please provide a valid value for the 'level' paramter. Type ?noga_recode for help.")
  if(!language %in% c("en","de","fr","it")==TRUE)stop("Please provide a valid value for the 'language' parameter. Type ?noga_recode for help.")
  if(vartype=="factor")stop("Please provide only a numeric or character variable to recode. Factor variables are not supported.")

}
