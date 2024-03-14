#' Function to read the noga lookup table for aggregation
#' @name noga_lookup_agg
#' @returns a data frame including the lookup table
#' @noRd

noga_lookup_agg <- function(){
  nomenklaturpath <- system.file("extdata", package = "noga") |>
    list.files(full.names=TRUE) |>
    grep(pattern="HCL_NOGA_levels_1-5_agg",x=_,value=TRUE)

  lookup <- readRDS(nomenklaturpath)
  return(lookup)
}
