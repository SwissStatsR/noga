# Document datasets here
# see: https://r-pkgs.org/data.html#sec-documenting-data

#' NOGA lookup table
#'
#' This data.frame contains all the NOGA codes and labels at various levels.
#' The labels are available in english, german, french and italian.
#'
#' @format ## `lookup`
#' A data frame with 1,790 rows and 10 columns:
#' \describe{
#'   \item{code}{NOGA code}
#'   \item{section}{section code}
#'   \item{division}{division code (2-digit)}
#'   \item{group}{group code (3-digit)}
#'   \item{class}{class code (4-digit)}
#'   \item{type}{type code (full 6-digit)}
#'   \item{name_en}{English label}
#'   \item{name_de}{German label}
#'   \item{name_fr}{French label}
#'   \item{name_it}{Italian label}
#' }
#' @source <https://www.i14y.admin.ch/it/catalog/datasets/HCL_NOGA/api>
"lookup"


#' NOGA code table
#'
#' This tibble contains all the NOGA codes and labels at various levels.
#' The labels are available in english, german, french and italian.
#'
#' @format ## `noga_levels`
#' A tibble with 1,790 rows and 15 columns:
#' \describe{
#'   \item{section}{section code}
#'   \item{division}{division code (2-digit)}
#'   \item{group}{group code (3-digit)}
#'   \item{class}{class code (4-digit)}
#'   \item{type}{type code (full 6-digit)}
#'   \item{code}{NOGA code}
#'   \item{name_en}{English label}
#'   \item{name_de}{German label}
#'   \item{name_fr}{French label}
#'   \item{name_it}{Italian label}
#'   \item{division.n}{division code, numeric version}
#'   \item{group.n}{group code, numeric version}
#'   \item{class.n}{class code, numeric version}
#'   \item{type.n}{type code, numeric version}
#'   \item{code.n}{code, numeric version}
#' }
#' @source <https://www.i14y.admin.ch/it/catalog/datasets/HCL_NOGA/api>
"noga_levels"


#' NOGA code table
#'
#' This tibble contains all the NOGA codes and labels at various levels.
#' The labels are available in english, german, french and italian.
#'
#' @format ## `noga_levels_agg`
#' A tibble with 1,790 rows and 19 columns:
#' \describe{
#'   \item{section}{section code}
#'   \item{division}{division code (2-digit)}
#'   \item{group}{group code (3-digit)}
#'   \item{class}{class code (4-digit)}
#'   \item{type}{type code (full 6-digit)}
#'   \item{code}{NOGA code}
#'   \item{name_de}{German label}
#'   \item{Section}{section code, only for sections}
#'   \item{Division}{division code, only for divisions}
#'   \item{Group}{group code, only for groups}
#'   \item{Class}{class code, only for classes}
#'   \item{Type}{type code, only for types}
#'   \item{name_en}{English label}
#'   \item{name_fr}{French label}
#'   \item{name_it}{Italian label}
#'   \item{division.n}{division code, numeric version}
#'   \item{group.n}{group code, numeric version}
#'   \item{class.n}{class code, numeric version}
#'   \item{type.n}{type code, numeric version}
#' }
#' @source <https://www.i14y.admin.ch/it/catalog/datasets/HCL_NOGA/api>
"noga_levels_agg"
