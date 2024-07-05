#' Internal function to check whether recoding values to labels or vice versa
#' @name directioncheck
#' @noRd

directioncheck <- function(vartype, var) {
  if (vartype %in% c("numeric", "integer")) {
    direction.to <- "labels"
  } else if (vartype == "character" & any(grepl("[a-zA-Z]", var)) & max(nchar(var)) > 1) {
    direction.to <- "values"
  } else if (vartype == "character" & (max(nchar(var)) == 1) | all(grepl("[0-9]", var))) {
    direction.to <- "labels"
  }
  return(direction.to)
}
