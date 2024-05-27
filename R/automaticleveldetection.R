#' Internal function to automatically detect which noga level is required
#' @name automaticleveldetection
#' @noRd

automaticleveldetection <- function(var,language){
    if(any(grepl("[A-Za-z]", var) )==TRUE){

      if(max(nchar(var))==1){
        noga.level <- "section"
        } else{
        label.var <- paste0("name_",language)
        lookup.filtered <- noga::lookup[eval(label.var) %in% var,]
        if(all(!is.na(lookup.filtered$type))==TRUE){noga.level <- "type"}
        if(all(!is.na(lookup.filtered$class))==TRUE & any(is.na(lookup.filtered$type))==TRUE){noga.level <- "class"}
        if(all(!is.na(lookup.filtered$group))==TRUE & any(is.na(lookup.filtered$class))==TRUE){noga.level <- "group"}
        if(all(!is.na(lookup.filtered$division))==TRUE & any(is.na(lookup.filtered$group))==TRUE){noga.level <- "division"}
      }


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
