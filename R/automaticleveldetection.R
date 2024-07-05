#' Internal function to automatically detect which noga level is required
#' @name automaticleveldetection
#' @noRd

automaticleveldetection <- function(var, language) {
  if (any(grepl("[A-Za-z]", var))) {
    # Section are either a code of one letter or all uppercase labels
    if (max(nchar(var)) == 1 | ! any(grepl("[a-z]", var))) {
      noga.level <- "section"
    } else {
      label.var <- paste0("name_", language)
      to.filter <- noga::lookup[[eval(label.var)]]
      # Looking for the pattern from the end of string avoid taking narrower/deeper labels into account
      # eg. Enseignement would otherwise return Enseignement primaire, Enseignement secondaire and so on
      matched.successfully <- sapply(paste0(unique(var), "$"), function(x) grepl(x, to.filter))
      matched.succesfully.index <- which(rowSums(matched.successfully) > 0)
      lookup.filtered <- noga::lookup[matched.succesfully.index, ]

      if (nrow(lookup.filtered) == 0) {
        # No match in the lookup table
        warning("Please provide the noga level manually, the automatic detection failed.")
      }

      matches_type <- nrow(lookup.filtered[!is.na(lookup.filtered$type), ])
      matches_class <- nrow(lookup.filtered[!is.na(lookup.filtered$class) & is.na(lookup.filtered$type), ])
      matches_group <- nrow(lookup.filtered[!is.na(lookup.filtered$group) & is.na(lookup.filtered$class), ])
      matches_division <- nrow(lookup.filtered[!is.na(lookup.filtered$division) & is.na(lookup.filtered$group), ])

      matches <- c("division" = matches_division, "group" = matches_group,
                   "class" = matches_class, "type" = matches_type)

      noga.level <- names(which(matches == max(matches)))

      # If ties, take the deepest level
      if (length(noga.level) > 1) {
        if ("type" %in% noga.level) {
          noga.level <- "type"
        } else if ("class" %in% noga.level) {
          noga.level <- "class"
        } else if ("group" %in% noga.level){
          noga.level <- "group"
        } else {
          noga.level <- "division"
        }
      }
    }

  } else {
    noga.level <- switch(as.character(nchar(max(var, na.rm = TRUE))),
                         "1" = "section", # This should fail because section codes aren't numeric but sectors can be and aren't supported (yet)
                         "2" = "division",
                         "3" = "group",
                         "4" = "class",
                         "6" = "type",
                         warning("Please provide the noga level manually, the automatic detection failed.")
                        )
  }

  message(paste("Detected level is :", noga.level))

  return(noga.level)
}
