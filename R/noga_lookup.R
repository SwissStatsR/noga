#' Function to read the noga lookup table
#' @name noga_lookup
#' @returns a data frame including the lookup table
#' @noRd

noga_lookup <- function(){
  nomenklaturpath <- system.file("extdata", package = "noga") |>
    list.files(full.names=TRUE) |>
    grep(pattern="HCL_NOGA_levels_1-5.R",x=_,value=TRUE)

    lookup <- readRDS(nomenklaturpath)
    return(lookup)
}
